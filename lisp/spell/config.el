;;; -*- lexical-binding: t; -*-

(after! ispell
  (cond
   (ymacs-aspell-path
    (setq ispell-program-name ymacs-aspell-path))
   (ymacs-hunspell-path
    (setq ispell-program-name ymacs-hunspell-path)
    ;; Just reset dictionary to the safe one "en_US" for hunspell.  if
    ;; we need use different dictionary, we specify it in command line
    ;; arguments
    (setq ispell-local-dictionary "en_US")
    (setq ispell-local-dictionary-alist
          '(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil nil nil utf-8))))
   (t (setq ispell-program-name nil)))

  ;; `ispell-extra-args' is the command arguments which will *always*
  ;; be used when start ispell process
  (setq-default ispell-extra-args (ymacs-spell//detect-ispell-args t)))

(after! flyspell
  (setq flyspell-large-region 1)
  ;; Better performance
  (setq flyspell-issue-message-flag nil))
