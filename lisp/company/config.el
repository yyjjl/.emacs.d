;;; -*- lexical-binding: t; -*-

(after! company
  (define-key! :map company-active-map
    ("C-d")
    ([tab] . ymacs-company/complete-common)
    ("TAB" . ymacs-company/complete-common)
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous)
    ("M-n" . company-next-page)
    ("M-p" . company-previous-page))

  (setq-default company-backends
                `((company-capf
                   company-dabbrev-code company-keywords
                   ;; company-files
                   :with company-yasnippet
                   :separate)
                  (company-gtags company-etags)
                  company-dabbrev))
  ;; Company should be case sensitive
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case t)
  (setq company-dabbrev-code-ignore-case nil)
  (setq company-dabbrev-char-regexp "[0-9a-zA-Z-_]")
  (setq company-dabbrev-code-other-buffers t)
  (setq company-show-numbers t)
  ;; Don't auto auto complete
  (setq company-idle-delay 0.3)
  (setq company-clang-insert-arguments nil)
  (setq company-require-match nil)
  (setq company-etags-ignore-case t)
  (setq company-minimum-prefix-length 3)
  (setq company-tooltip-align-annotations t)
  (setq company-auto-commit nil)
  ;; Not to load company-mode for certain major modes.
  (setq company-global-modes
        '(not eshell-mode comint-mode erc-mode
              gud-mode rcirc-mode shell-mode
              minibuffer-inactive-mode))

  (setf (alist-get 'company-search-mode mode-line-misc-info)
        '(("" company-search-lighter " "))))

(after! company-capf
  (advice-add 'company-capf :around #'ignore-errors!))
