EMACS ?= emacs
EMACS_DIR := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))

.PHONY: config tarball

config:
	$(EMACS) --batch -Q \
	  --eval '(setq user-emacs-directory "$(EMACS_DIR)/")' \
	  -l "$(EMACS_DIR)/etc/setup.el"

tarball:
	tar -C "$(dir $(EMACS_DIR))" -zcf /tmp/emacs.d.tgz \
	  --exclude-vcs-ignores "$(notdir $(EMACS_DIR))"
