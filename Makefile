EMACS ?= emacs

.PHONY: test

# Delete byte-compiled files etc.
clean:
	rm -f *~
	rm -f \#*\#
	rm -f *.elc

test/htmlize.el:
	curl -L https://raw.githubusercontent.com/hniksic/emacs-htmlize/HEAD/htmlize.el > $@

# Run tests.
test: clean test/htmlize.el
	$(EMACS) -batch -Q -l markdown-mime.el -l test/htmlize.el -l test/markdown-mime-tests.el
