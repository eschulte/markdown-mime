;;; markdown-mime.el --- org html export for text/html MIME emails  -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2015 Eric Schulte, 2016-2021 Chen Bin

;; Authors: Eric Schulte, Chen Bin <chenbin.sh@gmail.com>
;; Maintainer: Eric Schulte <schulte.eric@gmail.com>
;; Keywords: mime, mail, email, html, markdown
;; Homepage: http://github.com/eschulte/markdown-mime
;; Version: 0.3.2
;; Package-Requires: ((emacs "25.1"))

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; WYSIWYG, html mime composition using markdown-mode
;;
;; For mail composed using markdown syntax, this provides a function
;; for converting all or part of your mail buffer to embedded html as
;; exported by markdown.  Call `markdown-mime-htmlize' in a message
;; buffer to convert either the active region or the entire buffer to
;; html.
;;
;; Quick start:
;; Write a message in message-mode, make sure the mail body follows
;; markdown format.  Run `markdown-mime-htmlize' to convert the plain
;; text mail to html mail.  (You can always just `undo' to get back to
;; the plain text.)  Run `markdown-mime-preview' to double-check the
;; text/html part before sending.
;;
;; Setup (OPTIONAL):
;; You might want to bind these to keys with something like the
;; following message-mode binding
;;
;;   (add-hook 'message-mode-hook
;;             (lambda ()
;;               (local-set-key (kbd "C-c M-o") 'markdown-mime-htmlize)
;;               (local-set-key (kbd "C-c M-p") 'markdown-mime-preview)
;;               ;; This next obscure little bit stops
;;               ;; `electric-indent-mode' from running
;;               ;; `newline-and-indent' which will strip trailing
;;               ;; whitespace at the end of a line when RET is
;;               ;; pressed.  Trailing whitespace is needed in
;;               ;; markdown syntax to signal a </br>, e.g. after a
;;               ;; closing like "Thanks, ".
;;               (setq electric-indent-inhibit 'electric-layout-mode)))
;;
;; You (recommended) may want to enable list and table editing in your
;; message buffers.
;;
;;     (add-hook 'message-mode-hook
;;                 (lambda ()
;;                   (local-set-key (kbd "C-c M-o") 'markdown-mime-htmlize)
;;                   (when (fboundp 'orgalist-mode) (orgalist-mode 1))
;;                   (when (fboundp 'orgtbl-mode) (orgtbl-mode 1))))
;;
;; Or (not recommended) you may want to enable markdown in the
;; messsage body of your email.  You can do this with mmm-mode by
;; adding the following to your config.
;;
;;     (mmm-add-classes
;;      '((message-minor-markdown
;;         :submode markdown-mode
;;         :face mmm-declaration-submode-face
;;         :front "--text follows this line--"
;;         :front-offset 1 ; Skip the following newline.
;;         ;; :front-offset 36 ; Skip "<#secure method=pgpmime mode=sign>" and 2 newlines.
;;         :back "^-- ")))
;;     (mmm-add-mode-ext-class 'message-mode nil 'message-minor-markdown)
;;
;; Extra Tips:
;; 1. You can embed images into your mail using normal markdown syntax.
;;
;;    ![](/full/path/to/your.jpg
;;
;; 2. It's easy to define your hack the exported plain text and html.
;;    For example, below code removes "\\" from plain text,
;;
;;   (add-hook 'markdown-mime-plain-text-hook
;;             (lambda ()
;;               (while (re-search-forward "\\\\" nil t)
;;                 (replace-match ""))))
;;
;; For example, below code renders text between "#" in red color from html,
;;
;;   (add-hook 'markdown-mime-html-hook
;;             (lambda ()
;;               (while (re-search-forward "#\\([^#]*\\)#" nil t)
;;                 (replace-match "<span style=\"color:red\">\\1</span>"))))
;;
;; 4. Please note this program only embeds exported HTML into mail.
;;    Markdown is responsible for rendering HTML.  See
;;    `markdown-standalone' and the `markdown-command' variable to
;;    customize HTML export.
;;

;;; Code:
(require 'cl-lib)
(require 'markdown-mode)
(require 'message)

(defcustom markdown-mime-library 'mml
  "Library to use for marking up MIME elements."
  :group 'markdown-mime
  :type '(choice 'mml 'semi 'vm))

(defcustom markdown-mime-mail-signature-separator
  (or message-signature-separator "^--\s?$")
  "Default mail signature separator."
  :group 'markdown-mime
  :type 'string)

(defvar markdown-mime-plain-text-hook nil
  "Hook to run over the plain text buffer before adding it to email.
This is used to process plain text part of email .")

(defvar markdown-mime-html-hook nil
  "Hook to run over the html buffer before adding it to email.
This is used to process html part of email.")

(defun markdown-mime-export-string (string &optional options)
  "Export STRING into html with OPTIONS."
  (with-temp-buffer
    (let ((output-buffer (buffer-name (current-buffer))))
      (unwind-protect
          (progn
            (with-temp-buffer
              (insert string)
              (markdown-standalone output-buffer))))
      (buffer-string))))

;; example hook, for setting a dark background in
;; <pre style="background-color: #EEE;"> elements
(defun markdown-mime-change-element-style (element style)
  "Set <ELEMENT> elements in exported html with new default html STYLE."
  (while (re-search-forward (format "<%s" element) nil t)
    (replace-match (format "<%s style=\"%s\"" element style))))

(defun markdown-mime-file (ext path id)
  "Markup a file with EXT, PATH and ID for attachment."
  (cl-case markdown-mime-library
    (mml (format "<#part type=\"%s\" filename=\"%s\" disposition=inline id=\"<%s>\">\n<#/part>\n"
                 ext path id))
    (semi (concat
           (format "--[[%s\nContent-Disposition: inline;\nContent-ID: <%s>][base64]]\n"
                   ext id)
           (base64-encode-string
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert-file-contents-literally path)
              (buffer-string)))))
    (vm "?")))

(defun markdown-mime-multipart (plain html &optional images)
  "Markup PLAIN body a multipart/alternative with HTML alternatives.
If html portion of message includes IMAGES they are wrapped in
multipart/related part."
  (cl-case markdown-mime-library
    (mml (concat "<#multipart type=alternative>\n<#part type=text/plain>\n"
                 plain
                 (when images "<#multipart type=related>")
                 "<#part type=text/html>\n"
                 html
                 images
                 (when images "<#/multipart>\n")
                 "<#/multipart>\n"))
    (semi (concat
           "--" "<<alternative>>-{\n"
           "--" "[[text/plain]]\n" plain
           (when images (concat "--" "<<alternative>>-{\n"))
           "--" "[[text/html]]\n"  html
           images
           (when images (concat "--" "}-<<alternative>>\n"))
           "--" "}-<<alternative>>\n"))
    (vm "?")))

(defun markdown-mime-url-to-path (url current-file)
  "If URL is file path, convert to valid path.
Or else use CURRENT-FILE to calculate path."
  (let* ((dir (file-name-directory current-file))
         (path (expand-file-name url dir)))
    (cond
     ((string-match-p "^file:///" url)
      (let* ((str (replace-regexp-in-string "^file://" "" url)))
        (when (and (eq system-type 'windows-nt)
                   (string-match "^/[a-zA-Z]:" str))
          ;; remove the first character from "/C:/Windows/File.txt"
          (setq str (substring str 1)))
        str))

     ((file-exists-p path)
      path)

     (t
      (expand-file-name url default-directory)))))

(defun markdown-mime-replace-images (str current-file)
  "Replace images in STR with cid links.
CURRENT-FILE is used to calculate full path of images."
  (let* (html-images)
    (cons
     (replace-regexp-in-string ;; replace images in html
      "src=\"\\([^\"]+\\)\""
      (lambda (text)
        (if (string= ".js\"" (subseq text (- (length text) 4)))
            text       ; Skip JavaScript links leaving them unchanged.
          (format
           "src=\"cid:%s\""
           (let* ((url (and (string-match "src=\"\\([^\"]+\\)\"" text)
                            (match-string 1 text)))
                  (path (markdown-mime-url-to-path url current-file))
                  (ext (file-name-extension path))
                  (id (replace-regexp-in-string "[\/\\\\]" "_" path)))

             ;; Catch non-existent files here. Otherwise users get an error on sending.
             (unless (file-exists-p path)
               (user-error "Path: %s does not exist" path))

             ;; Do it
             (add-to-list 'html-images
                          (markdown-mime-file (concat "image/" ext) path id))
             id))))
      str)
     html-images)))

(defun markdown-mime-extract-non-image-files ()
  "Extract non-image links in current buffer."
  (save-excursion
    (cl-loop while (markdown-next-link)
             when (string= "file" (first (split-string (markdown-link-url) ":")))
             collect (markdown-link-url))))

(defun markdown-mime-format-forwarded-message ()
  "Format the headers and offset lines in forwarded messages."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "[\n]\\(-+ Start of forwarded message -+\\)" nil t)
      (replace-match "  \n`\\1`  ")
      (goto-char (match-end 0))
      (let ((header-end (save-excursion (re-search-forward "^$") (point-marker))))
        (cl-loop while (re-search-forward "[\n]\\([^[:space:]]+\\): \\(.*\\)$" header-end t)
                 do (replace-match "  \n**\\1**: \\2")))
      (when (re-search-forward "[\n]\\(-+ End of forwarded message -+\\)" nil t)
        (replace-match "  \n`\\1`  ")))))

(defun markdown-mime-apply-plain-text-hook (text)
  "Apply TEXT hook."
  (if markdown-mime-plain-text-hook
      (with-temp-buffer
        (insert text)
        (goto-char (point-min))
        (run-hooks 'markdown-mime-plain-text-hook)
        (buffer-string))
    text))

(defun markdown-mime-apply-html-hook (html)
  "Apply HTML hook."
  (if markdown-mime-html-hook
      (with-temp-buffer
        (insert html)
        (goto-char (point-min))
        (run-hooks 'markdown-mime-html-hook)
        (buffer-string))
    html))

(defun markdown-mime-insert-html-content (plain file html)
  "Insert PLAIN into FILE with HTML content."
  (let* ((files (markdown-mime-extract-non-image-files))
         (html-and-images (markdown-mime-replace-images html file))
         (images (cdr html-and-images))
         (html (markdown-mime-apply-html-hook (car html-and-images))))

    ;; Format any forwarded emails.
    (markdown-mime-format-forwarded-message)

    ;; If there are files that were attached, we should remove the links,
    ;; and mark them as attachments. The links don't work in the html file.
    (when files
      (mapc (lambda (f)
              (setq html (replace-regexp-in-string
                          (format "<a href=\"%s\">%s</a>"
                                  (regexp-quote f) (regexp-quote f))
                          (format "%s (attached)" (file-name-nondirectory f))
                          html)))
            files))

    (insert (markdown-mime-multipart (markdown-mime-apply-plain-text-hook plain)
                                     html
                                     (if images (mapconcat 'identity images "\n"))))

    ;; Attach any residual files
    (when files (mapc mml-attach-file files))

    ;; spacer
    (insert "\n\n")))

(defun markdown-mime-mail-body-begin ()
  "Get begin of mail body."
  (save-excursion
    (goto-char (point-min))
    (search-forward mail-header-separator)
    (+ (point) 1)))

(defun markdown-mime-mail-signature-begin ()
  "Find start of signature line in email."
  (save-excursion
    (goto-char (point-max))
    (re-search-backward markdown-mime-mail-signature-separator nil t nil)))

(defmacro markdown-mime-extract-tag-in-current-buffer (beginning end result)
  "Extract the text between BEGINNING and END and insert it into RESULT."
  `(when (and ,beginning ,end (< ,beginning ,end))
     (push (buffer-substring-no-properties ,beginning ,end) ,result)
     ;; delete old tag
     (delete-region ,beginning ,end)))

(defun markdown-mime-extract-non-text ()
  "Extract content not in org format (gpg signature, attachments ...)."
  (unless (org-region-active-p)
    (let* (secure-tags
           part-tags
           str
           b
           e
           (old-pos (point)))
      (goto-char (point-min))
      (while (re-search-forward "<#secure \\|<#part .* filename=" (point-max) t)
        (setq str (match-string 0))
        (setq b (match-beginning 0))
        (cond
         ;; one line gpg signature tag
         ((string-match "^<#secure " str)
          (setq e (line-end-position))
          (markdown-mime-extract-tag-in-current-buffer b e secure-tags))

         ;; multi-lines attachment
         ((string-match "^<#part .* filename=" str)
          (save-excursion
            (unless (re-search-forward "<#/part>" (point-max) t)
              (error (format "\"%s\" should have end tag." str)))
            (setq e (match-end 0))
            (markdown-mime-extract-tag-in-current-buffer b e part-tags))))

        ;; search next tag
        (goto-char (point-min)))

      ;; move cursor back to its original position
      (goto-char old-pos)

      (list :secure-tags (nreverse secure-tags)
            :part-tags (nreverse part-tags)))))

;;;###autoload
(defun markdown-mime-htmlize ()
  "Export a portion of an email to html using `org-mode'.
If called with an active region only export that region, otherwise entire body."
  (interactive)

  (let* ((region-p (use-region-p))
         (all-tags (markdown-mime-extract-non-text))
         (html-start (or (and region-p (region-beginning))
                         (markdown-mime-mail-body-begin)))
         (html-end (or (and region-p (region-end))
                       (or
                        (markdown-mime-mail-signature-begin)
                        (point-max))))
         (raw-text (buffer-substring html-start html-end))
         ;; to hold attachments for inline html images
         (html (markdown-mime-export-string raw-text))
         (file (make-temp-name (expand-file-name
                                "mail" temporary-file-directory))))

    ;; delete current region
    (delete-region html-start html-end)
    (goto-char html-start)

    ;; restore secure tags
    (when (plist-get all-tags :secure-tags)
      (insert (mapconcat #'identity (plist-get all-tags :secure-tags) "\n"))
      ;; spacer
      (insert "\n\n"))

    ;; insert converted html
    (markdown-mime-insert-html-content raw-text file html)

    ;; restore part tags (attachments)
    (when (plist-get all-tags :part-tags)
      (insert (mapconcat #'identity (plist-get all-tags :part-tags) "\n"))
      (insert "\n\n"))))

;;;###autoload
(defun markdown-mime-preview ()
  "Open the text/html mime alternative of the email in the current bufffer.
NOTE: This preview will NOT resolve attached images in the HTML
preview although they will render when shown in the recipients
mail reader."
  (interactive)
  (let ((html-part (save-excursion
                     (goto-char (point-min))
                     (buffer-substring
                      (progn (search-forward "<#part type=text/html>") (match-end 0))
                      (progn (search-forward "<#/multipart>") (match-beginning 0))))))
    (with-temp-buffer
      (insert html-part)
      (browse-url-of-buffer))))

(provide 'markdown-mime)
;; Local Variables:
;; coding: utf-8
;; tab-width: 2
;; indent-tabs-mode: nil
;; End:
;;; markdown-mime.el ends here
