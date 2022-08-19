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
;; text mail to html mail.  Run
;; `markdown-mime-revert-to-plain-text-mail' if you want to restore to
;; plain text mail.
;;
;; Setup (OPTIONAL):
;; You might want to bind this to a key with something like the
;; following message-mode binding
;;
;;   (add-hook 'message-mode-hook
;;             (lambda ()
;;               (local-set-key (kbd "C-c M-o") 'markdown-mime-htmlize)))
;;
;; Extra Tips (TODO):
;; 1. In order to embed images into your mail, use the syntax below,
;;
;;    [[/full/path/to/your.jpg]]
;;
;; 2. It's easy to define your hack the exported plain text and html.
;; For example, below code removes "\\" from plain text,
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
;; 3. The quoted mail uses Gmail's style, so reply looks clean and modern.
;;
;; 4. Please note this program can only embed exported HTML into mail.
;;    Markdown is responsible for rendering HTML.
;;

;;; Code:
(require 'cl-lib)
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

(defvar markdown-mime-pre-html-hook nil
  "Hook to run before html export.
Functions should take no arguments and will be run in a
buffer holding the text to be exported.")

(defvar markdown-mime-send-buffer-hook nil
  "Hook to run in the markdown file before export.")

(defvar markdown-mime-debug nil
  "Enable debug logger.")

;; internal variables
(defvar markdown-mime-src--overlay nil)
(defvar markdown-mime-src--beg-marker nil)
(defvar markdown-mime-src--end-marker nil)
(defvar markdown-mime--saved-temp-window-config nil)
(defconst markdown-mime-src--hint "## markdown-mime hint: Press C-c C-c to commit change.\n")

(defun markdown-mime-current-line ()
  "Get current line."
  (buffer-substring-no-properties (line-beginning-position)
                                  (line-end-position)))

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

(defun markdown-mime-change-class-style (class style)
  "CLASS is used for new default html STYLE in exported html."
  (while (re-search-forward (format "class=\"%s\"" class) nil t)
    (replace-match (format "class=\"%s\" style=\"%s\"" class style))))

(defun markdown-mime-file (ext path id)
  "Markup a file with EXT, PATH and ID for attachment."
  (when markdown-mime-debug (message "markdown-mime-file called => %s %s %s" ext path id))
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
  (when markdown-mime-debug (message "markdown-mime-replace-images called => %s" current-file))
  (let* (html-images)
    (cons
     (replace-regexp-in-string ;; replace images in html
      "src=\"\\([^\"]+\\)\""
      (lambda (text)
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
           id)))
      str)
     html-images)))

(defun markdown-mime-extract-non-image-files ()
  "Extract non-image links in current buffer."
  (cond
   ((>= (markdown-mime-org-major-version) 9)
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (when (and (string= (org-element-property :type link) "file")
                   (not (string-match
                         (cdr (assoc "file" org-html-inline-image-rules))
                         (org-element-property :path link))))
          (org-element-property :path link)))))
   (t
    (message "Warning: org-element-map is not available. File links will not be attached.")
    nil)))

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
    (when files
      (mapc (lambda (f)
              (when markdown-mime-debug (message "attaching: %s" f))
              (mml-attach-file f))
            files))

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

(defun markdown-mime-extract-non-org ()
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
  (when markdown-mime-debug (message "markdown-mime-htmlize called"))

  (let* ((region-p (org-region-active-p))
         (all-tags (markdown-mime-extract-non-org))
         (secure-tags (plist-get all-tags :secure-tags))
         (part-tags (plist-get all-tags :part-tags))
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
    (when secure-tags
      (insert (mapconcat #'identity secure-tags "\n"))
      ;; spacer
      (insert "\n\n"))

    ;; insert converted html
    (markdown-mime-insert-html-content raw-text file html)

    ;; restore part tags (attachments)
    (when part-tags
      (insert (mapconcat #'identity part-tags "\n"))
      (insert "\n\n"))))

(defun markdown-mime--get-buffer-title ()
  "Get buffer title."
  (let* ((options (markdown-mime-get-buffer-export-options))
         (tmp (and options (plist-get options :title))))
    (when tmp
      (let ((txt (car tmp)))
        (set-text-properties 0 (length txt) nil txt)
        txt))))

(defun markdown-mime-compose (exported file to subject headers)
  "Create mail body from EXPORTED in FILE with TO, SUBJECT, HEADERS."
  ;; start composing mail
  (let* ((html (car exported))
         (plain (cdr exported))
         patched-html)
    (compose-mail to subject headers nil)
    (message-goto-body)
    (setq patched-html (with-temp-buffer
                         (insert html)
                         (goto-char (point-min))
                         (run-hooks 'markdown-mime-pre-html-hook)
                         (buffer-string)))
    ;; insert text
    (markdown-mime-insert-html-content plain file patched-html)))

(defun markdown-mime-buffer-properties ()
  "Extract buffer properties."
  (let* (rlt value key)
    (org-element-map (org-element-parse-buffer) 'keyword
      (lambda (keyword)
        (when (and (string= (org-element-property :key keyword) "PROPERTY")
                   (string-match "^MAIL_\\(TO\\|SUBJECT\\|CC\\|BCC\\|FROM\\) +"
                                 (setq value (org-element-property :value keyword)))
                   (setq key (concat "MAIL_" (match-string 1 value)))
                   (setq rlt
                         (plist-put rlt
                                    (intern (concat ":" key))
                                    (string-trim (replace-regexp-in-string key "" value))))))))
    rlt))

(defun markdown-mime-build-mail-other-headers (cc bcc from)
  "Build mail header from CC, BCC, and FROM."
  (let* ((arr (list (cons "Cc" cc) (cons "Bcc" bcc)  (cons "From" from )))
         rlt)
    (dolist (e arr)
      (when (cdr e)
        (push e rlt)))
    rlt))

;;;###autoload
(defun markdown-mime-org-buffer-htmlize ()
  "Create an email buffer of the current org buffer.
The email buffer will contain both html and in org formats as mime
alternatives.

The following file keywords can be used to control the headers:
#+MAIL_TO: some1@some.place
#+MAIL_SUBJECT: a subject line
#+MAIL_CC: some2@some.place
#+MAIL_BCC: some3@some.place
#+MAIL_FROM: sender@some.place

The cursor ends in the TO field."
  (interactive)
  (run-hooks 'markdown-mime-send-buffer-hook)
  (let* ((org-html-klipsify-src nil)
         (file (buffer-file-name (current-buffer)))
         (props (markdown-mime-buffer-properties))
         (subject (or (plist-get props :MAIL_SUBJECT)
                      (markdown-mime--get-buffer-title)
                      (if (not file) (buffer-name (buffer-base-buffer))
                        (file-name-sans-extension
                         (file-name-nondirectory file)))))
         (exported (markdown-mime-export-buffer-or-subtree nil))
         (to (plist-get props :MAIL_TO))
         (cc (plist-get props :MAIL_CC))
         (bcc (plist-get props :MAIL_BCC))
         (from (plist-get props :MAIL_FROM))
         (other-headers (markdown-mime-build-mail-other-headers cc
                                                           bcc
                                                           from)))
    (markdown-mime-compose exported file to subject other-headers)
    (message-goto-to)))

(defun markdown-mime-org-major-version ()
  "Get Org major version."
  (string-to-number (car (split-string (org-release) "\\."))))

(defun markdown-mime-attr (property)
  "Get org mime PROPERTY."
  (org-entry-get nil property markdown-mime-use-property-inheritance))

;;;###autoload
(defun markdown-mime-org-subtree-htmlize (&optional htmlize-first-level)
  "Create an email buffer from current subtree.
If HTMLIZE-FIRST-LEVEL is t, first level subtree of current node is htmlized.

Following headline properties can determine the mail headers.
* subtree heading
  :PROPERTIES:
  :MAIL_SUBJECT: mail title
  :MAIL_TO: person1@gmail.com
  :MAIL_CC: person2@gmail.com
  :MAIL_BCC: person3@gmail.com
  :MAIL_FROM: sender@gmail.com
  :END:"
  (interactive "P")
  (save-excursion
    (org-back-to-heading)

    (when (and htmlize-first-level
               (not (string-match "^\\* " (markdown-mime-current-line))))
      ;; go back to the 1st level subtree
      (re-search-backward "^\\* ")
      (org-back-to-heading))

    (when (outline-on-heading-p nil)
      (let* ((file (buffer-file-name (current-buffer)))
             (props (markdown-mime-buffer-properties))
             (subject (or (markdown-mime-attr "MAIL_SUBJECT")
                          (plist-get props :MAIL_SUBJECT)
                          (nth 4 (org-heading-components))))
             (to (or (markdown-mime-attr "MAIL_TO")
                     (plist-get props :MAIL_TO)))
             (cc (or (markdown-mime-attr "MAIL_CC")
                     (plist-get props :MAIL_CC)))
             (bcc (or (markdown-mime-attr "MAIL_BCC")
                      (plist-get props :MAIL_BCC)))
             (from (or (markdown-mime-attr "MAIL_FROM")
                       (plist-get props :MAIL_FROM)))
             ;; Thanks to Matt Price improving handling of cc & bcc headers
             (other-headers (markdown-mime-build-mail-other-headers cc bcc from))
             (org-export-show-temporary-export-buffer nil)
             (org-export-show-temporary-export-buffer nil)
             ;; I wrap these bodies in export blocks because in markdown-mime-compose
             ;; they get exported again. This makes each block conditionally
             ;; exposed depending on the backend.
             (exported (save-restriction (org-narrow-to-subtree)
                                         (markdown-mime-export-buffer-or-subtree t))))
        (save-restriction
          (org-narrow-to-subtree)
          (markdown-mime-compose exported file to subject other-headers))
        (message-goto-to)))))

(defun markdown-mime-src--remove-overlay ()
  "Remove overlay from current source buffer."
  (when (overlayp markdown-mime-src--overlay)
    (delete-overlay markdown-mime-src--overlay)))

(defun markdown-mime-edited-code ()
  "Get edited code."
  (save-excursion
    (goto-char (point-min))
    (search-forward markdown-mime-src--hint (point-max) t)
    (goto-char (line-beginning-position))
    (buffer-substring-no-properties (point) (point-max))))

(defun markdown-mime-edit-src-save ()
  "Save parent buffer with current state source-code buffer."
  (interactive)
  (set-buffer-modified-p nil)
  (let* ((edited-code (markdown-mime-edited-code))
         (beg markdown-mime-src--beg-marker)
         (end markdown-mime-src--end-marker)
         (overlay markdown-mime-src--overlay))
    (with-current-buffer (marker-buffer markdown-mime-src--beg-marker)
      (undo-boundary)
      (goto-char beg)
      ;; Temporarily disable read-only features of OVERLAY in order to
      ;; insert new contents.
      (delete-overlay overlay)
      (delete-region beg end)
      (let* ((expecting-bol (bolp)))
        (insert edited-code)
        (when (and expecting-bol (not (bolp))) (insert "\n")))
      (save-buffer)
      (move-overlay overlay beg (point))))
  ;; `write-contents-functions' requires the function to return
  ;; a non-nil value so that other functions are not called.
  t)

(defun markdown-mime-src-mode-configure-edit-buffer ()
  "Set up clean up functions when editing source code."
  (add-hook 'kill-buffer-hook #'markdown-mime-src--remove-overlay nil 'local)
  (setq buffer-offer-save t)
  (setq-local write-contents-functions '(markdown-mime-edit-src-save)))

(defvar markdown-mime-src-mode-hook nil
  "Hook run after switching embedded org code to its `org-mode'.")

(defun markdown-mime-edit-src-exit ()
  "Kill current sub-editing buffer and return to source buffer."
  (interactive)
  (let* ((beg markdown-mime-src--beg-marker)
         (end markdown-mime-src--end-marker)
         (edit-buffer (current-buffer))
         (source-buffer (marker-buffer beg)))
    (markdown-mime-edit-src-save)
    (unless source-buffer (error "Source buffer disappeared.  Aborting"))
    ;; Insert modified code.  Ensure it ends with a newline character.
    (kill-buffer edit-buffer)

    ;; to the beginning of the block opening line.
    (goto-char beg)

    ;; Clean up left-over markers and restore window configuration.
    (set-marker beg nil)
    (set-marker end nil)
    (when markdown-mime--saved-temp-window-config
      (set-window-configuration markdown-mime--saved-temp-window-config)
      (setq markdown-mime--saved-temp-window-config nil))))

(defvar markdown-mime-src-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'markdown-mime-edit-src-exit)
    (define-key map (kbd "C-x C-s") #'markdown-mime-edit-src-save)
    map))

(define-minor-mode markdown-mime-src-mode
  "Minor mode for org major mode buffers generated from mail body."
  :lighter " OrgMimeSrc")
(add-hook 'markdown-mime-src-mode-hook #'markdown-mime-src-mode-configure-edit-buffer)

(defun markdown-mime-src--make-source-overlay (beg end)
  "Create overlay between BEG and END positions and return it."
  (let* ((overlay (make-overlay beg end)))
    (overlay-put overlay 'face 'secondary-selection)
    (let ((read-only
           (list
            (lambda (&rest _)
              (user-error
               "Cannot modify an area being edited in a dedicated buffer")))))
      (overlay-put overlay 'modification-hooks read-only)
      (overlay-put overlay 'insert-in-front-hooks read-only)
      (overlay-put overlay 'insert-behind-hooks read-only))
    overlay))

(defun markdown-mime-edit-mail-in-org-mode ()
  "Call a special editor to edit the mail body in `org-mode'."
  (interactive)
  ;; see `org-src--edit-element'
  (cond
   ((eq major-mode 'org-mode)
    (message "This command is not for `org-mode'."))
   (t
    (setq markdown-mime--saved-temp-window-config (current-window-configuration))
    (let* ((beg (copy-marker (markdown-mime-mail-body-begin)))
           (end (copy-marker (or (markdown-mime-mail-signature-begin) (point-max))))
           (bufname "OrgMimeMailBody")
           (buffer (generate-new-buffer bufname))
           (overlay (markdown-mime-src--make-source-overlay beg end))
           (text (buffer-substring-no-properties beg end)))

      (setq markdown-mime-src--beg-marker beg)
      (setq markdown-mime-src--end-marker end)
      ;; don't use local-variable because only user can't edit multiple emails
      ;; or multiple embedded org code in one mail
      (setq markdown-mime-src--overlay overlay)

      (save-excursion
        (delete-other-windows)
        (org-switch-to-buffer-other-window buffer)
        (erase-buffer)
        (insert markdown-mime-src--hint)
        (insert text)
        (goto-char (point-min))
        (org-mode)
        (markdown-mime-src-mode))))))

(defun markdown-mime-revert-to-plain-text-mail ()
  "Revert mail body to plain text."
  (interactive)
  (let* ((txt-sep "<#part type=text/plain>")
         (html-sep "<#part type=text/html>")
         mail-beg
         mail-text
         txt-beg
         txt-end)
    (save-excursion
      (goto-char (point-min))
      (setq mail-beg (search-forward mail-header-separator (point-max) t))
      (setq txt-beg (search-forward txt-sep (point-max) t))
      (setq txt-end (search-forward html-sep (point-max) t)))
    (cond
     ((and mail-beg txt-beg txt-end (< mail-beg txt-beg txt-end))
      ;; extract text mail
      (setq mail-text (buffer-substring-no-properties txt-beg
                                                      (- txt-end (length html-sep))))
      ;; delete html mail
      (delete-region mail-beg (point-max))
      (when markdown-mime-debug
        (message "mail-beg=%s mail-text=%s" mail-beg mail-text))
      ;; restore text mail
      (insert mail-text))
     (t
      (message "Can not find plain text mail.")))))

(defun markdown-mime-confirm-when-no-multipart ()
  "Prompt whether to send email if the buffer is not htmlized."
  (let ((found-multipart (save-excursion
                           (save-restriction
                             (widen)
                             (goto-char (point-min))
                             (search-forward "<#multipart type=alternative>" nil t)))))
    (when (and (not found-multipart)
               (not (y-or-n-p "It seems `markdown-mime-htmlize' is NOT called; send anyway? ")))
      (setq quit-flag t))))


(provide 'markdown-mime)
;; Local Variables:
;; coding: utf-8
;; tab-width: 2
;; indent-tabs-mode: nil
;; End:
;;; markdown-mime.el ends here
