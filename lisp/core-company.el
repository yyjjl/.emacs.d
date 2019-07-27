(make-variable-buffer-local 'company-backends)

(defun company//complete-common ()
  (interactive)
  (unless (ignore-errors (yas-expand))
    (company-complete-common)))

(with-eval-after-load 'company
  (define-key! :map company-active-map
    ("C-d" . nil)
    ([tab] . company//complete-common)
    ("TAB" . company//complete-common)
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous)
    ("M-n" . company-next-page)
    ("M-p" . company-previous-page))

  (company-statistics-mode 1)

  ;; Make company-files a work before capf
  (setq-default company-backends
                '(company-nxml
                  company-css
                  ;; (company-files company-cmake)
                  company-cmake
                  company-capf
                  (company-dabbrev-code
                   company-etags
                   company-gtags
                   company-keywords)
                  company-dabbrev
                  company-yasnippet))

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
  ;; press SPACE will accept the highlighted candidate and insert a space
  ;; `M-x describe-variable company-auto-complete-chars` for details
  ;; That's BAD idea.
  (setq company-auto-complete nil)
  ;; Not to load company-mode for certain major modes.
  (setq company-global-modes
        '(not eshell-mode comint-mode erc-mode
              gud-mode rcirc-mode shell-mode
              minibuffer-inactive-mode)))

(with-eval-after-load 'company-capf
  (advice-add 'company-capf :around #'ignore-errors!))

(define-key!
  ("C-c <tab>" . company-complete)
  ("C-c TAB" . company-complete)
  ("C-c F" . company-files)

  ("C-}" . core/company-yasnippet)
  ([f6] . core/toggle-company-ispell))

(provide 'core-company)
