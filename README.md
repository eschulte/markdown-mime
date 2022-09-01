markdown-mime
=============

This program sends HTML email using Markdown HTML export.

This approximates a WYSiWYG HTML mail editor from within Emacs, and
can be useful for sending tables, fontified source code, and inline
images in email.

For example the following message buffer:


    To: Eric Schulte <my.self@gmail.com>
    Subject: Markdown-mime example email
    From: Eric Schulte <my.self@gmail.com>
    --text follows this line--
    <#secure method=pgpmime mode=sign>
    Paragraphs wrapping nicely because nobody expects you to pick your own
    line breaks these days.  Also, Markdown export including:

    - Fancy *italics* and **bold**,

    - lists,

    - tables,

        | Heading One   | Heading Two  |
        |---------------|--------------|
        | Content One   | Content Two  |
        | Content Three | Content Four |

    - and inline images.

        ![markdown mark](~/Desktop/Markdown-mark.png)

    Just call `markdown-mime-htmlize` which you can bind to a key with

    ```lisp
    (add-hook 'message-mode-hook
                (lambda ()
                  (local-set-key (kbd "C-c M-o") 'markdown-mime-htmlize)))
    ```

    Or (*recommended*) enable list and table editing in your message buffers
    
    ```lisp
    (add-hook 'message-mode-hook
                (lambda ()
                  (local-set-key (kbd "C-c M-o") 'markdown-mime-htmlize)
                  (local-set-key (kbd "C-c M-p") 'markdown-mime-preview)
                  (when (fboundp 'orgalist-mode) (orgalist-mode 1))
                  (when (fboundp 'orgtbl-mode) (orgtbl-mode 1))))
    ```

    Or (*not recommended*) get really really fancy and edit emails in markdown-mode using mmm-mode with

    ```lisp
    (mmm-add-classes ;; mmm-mode class for markdown keybindings and highlighting
       '((message-minor-markdown
          :submode markdown-mode
          :face mmm-declaration-submode-face
          :front "--text follows this line--"
          :back "^-- ")))
    (mmm-add-mode-ext-class 'message-mode nil 'message-minor-markdown)
    ```


sends an html email rendering as follows:

![](https://raw.githubusercontent.com/eschulte/markdown-mime/main/screenshot.png)


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

### Pandoc for highlighted source code and tables
The plain `markdown` markup engine does not support fontified source
code or tables.  This fun functionality can be relatively easily added
by switching to using [pandoc][] as your markdown to HTML exporter,
with [premailer][] used as a post-processing step to inline the CSS
generated by pandoc into the generated html so that mail user agents
won't strip it out.

[pandoc]: https://pandoc.org
[premailer]: https://pypi.org/project/premailer/

The `pandoc-email.sh` script included in this directory is a simple
shell script that calls `pandoc` and passes the results through
`premailer`.

To enable tables and source code highlighting with pandoc and
premailer:

1. Install [pandoc][].

2. Install [premailer][].

3. Set the value of `markdown-command` to point to the
   `pandoc-email.sh` script in this directory.

    ```elisp
    (setq markdown-command "/path/to/this/repo/pandoc-email.sh")
    ```


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
