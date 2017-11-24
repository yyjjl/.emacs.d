(defun core/toggle-company-ispell ()
  "Toggle `company-ispell'"
  (interactive)
  (cond
   ((memq 'company-ispell company-backends)
    (setq company-backends (delete 'company-ispell company-backends))
    (message "company-ispell disabled"))
   (t
    ;; Company-ispell it will stop other completions
    (add-to-list 'company-backends 'company-ispell)
    (message "company-ispell enabled!"))))

(defun core/company-yasnippet ()
  "Call `company-yasnippet'"
  (interactive)
  (company-abort)
  (call-interactively 'company-yasnippet))

(make-variable-buffer-local 'company-backends)
(with-eval-after-load 'company
  (define-key! :map company-active-map
    ("C-d" . nil)
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous)
    ("M-n" . company-next-page)
    ("M-p" . company-previous-page))

  (company-statistics-mode 1)
  (add-to-list 'company-transformers
               'company-sort-by-backend-importance :append)
  ;; Make company-files a work before capf
  (setq-default company-backends
                '(company-nxml
                  company-css
                  (company-files company-cmake)
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
  (setq company-idle-delay 0.2)
  (setq company-clang-insert-arguments nil)
  (setq company-require-match nil)
  (setq company-etags-ignore-case t)
  (setq company-minimum-prefix-length 3)
  (setq company-tooltip-align-annotations t)
  ;; press SPACE will accept the highlighted candidate and insert a space
  ;; `M-x describe-variable company-auto-complete-chars` for details
  ;; That's BAD idea.
  (setq company-auto-complete nil)
  ;; NOT to load company-mode for certain major modes.
  (setq company-global-modes
        '(not eshell-mode comint-mode erc-mode
              gud-mode rcirc-mode shell-mode
              minibuffer-inactive-mode)))

(with-eval-after-load 'company-capf
  (advice-add 'company-capf :around #'ignore-errors!))

(provide 'core-company)
