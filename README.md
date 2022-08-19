markdown-mime
=============

This program sends HTML email using Markdown HTML export.

This approximates a WYSiWYG HTML mail editor from within Emacs, and
can be useful for sending tables, fontified source code, and inline
images in email.

For example the following message buffer:

```
To: Eric Schulte <my.self@gmail.com>
Subject: Test markdown-mime
From: Eric Schulte <my.self@gmail.com>
--text follows this line--
<#secure method=pgpmime mode=sign>
Paragraphs wrapping nicely because nobody expects you to pick your own
line breaks these days.  Also, Markdown export including:

- Fancy *italics* and **bold**,

- lists,

- and inline images.
  ![markdown mark](~/Desktop/Markdown-mark.png)

Just call `markdown-mime-htmlize `which you can bind to a key with

    (add-hook 'message-mode-hook
                (lambda ()
                  (local-set-key (kbd "C-c M-o") 'markdown-mime-htmlize)))

Or get really fancy and edit emails in markdown-mode using mmm-mode with

    (mmm-add-classes
       '((message-minor-markdown
          :submode markdown-mode
          :face mmm-declaration-submode-face
          :front "--text follows this line--"
          :front-offset 36 ; Covers "
          :back "-- ")))
    (mmm-add-mode-ext-class 'message-mode nil 'message-minor-markdown)

-- 
Eric Schulte (he/him)
https://eschulte.github.io
PGP Fingerprint: FA8D C2C3 E8A0 A749 34CD  9DCF 3C1B 8581 614C A05D
```

sends an html email rendering as follows:

![](https://raw.githubusercontent.com/eschulte/markdown-mime/master/screenshot.png)


## Setup

```elisp
(require 'markdown-mime)
;; for gnus – this is set by default
(setq markdown-mime-library 'mml)
;; OR for Wanderlust (WL)
;; (setq markdown-mime-library 'semi)
```


## Usage

### `M-x markdown-mime-htmlize`

Run `M-x markdown-mime-htmlize` from within a mail composition buffer
to export either the entire buffer or just the active region to html,
and embed the results into the buffer as a text/html mime section.

Export a portion of an email body composed using `mml-mode` to html
using `markdown-mode`.  If called with an active region only export
that region, otherwise export the entire body.

Warning: There has been some concern voiced over the potential
complexity of email resulting from calling this function on an active
region resulting in multiple =multipart/alternative= sections in the
same email. Please see [this email thread][] for a discussion of the
potential pitfalls of this approach. Speaking from personal experience
this has not been a problem for the author.

[this email thread]: http://thread.gmane.org/gmane.emacs.orgmode/23617

You could use `markdown-mime-edit-mail-in-org-mode` edit mail in a special editor with `org-mode`.

After `markdown-mime-htmlize`, you can always undo to restore the
original plain text mail.

## Tips

### CSS style customization
Email clients will often strip all global CSS from email messages. In
the case of web-based email readers this is essential in order to
protect the CSS of the containing web site. To ensure that your CSS
styles are rendered correctly they must be included in the actual body
of the elements to which they apply.

The exported HTML could be modified in `markdown-mime-html-hook`. For example, below code renders text between "#" in red color,
```elisp
(add-hook 'markdown-mime-html-hook
          (lambda ()
            (while (re-search-forward "#\\([^#]*\\)#" nil t)
              (replace-match "<span style`\"color:red\">\\1</span>"))))
```
