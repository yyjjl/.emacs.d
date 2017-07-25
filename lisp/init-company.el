(defun main|toggle-company-ispell ()
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

(defun main|company-yasnippet ()
  "Call `company-yasnippet'"
  (interactive)
  (company-abort)
  (call-interactively 'company-yasnippet))

(make-variable-buffer-local 'company-backends)

(with-eval-after-load 'company
  (company-statistics-mode 1)
  ;; make company-files a work before capf
  (setq-default company-backends
                '(company-bbdb company-nxml company-css
                               (company-files company-cmake)
                               company-capf
                               ;; company-semantic
                               (company-dabbrev-code
                                company-gtags
                                company-etags
                                company-keywords)
                               company-oddmuse
                               company-dabbrev))
  ;; company should be case sensitive
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case nil)
  (setq company-dabbrev-code-ignore-case t)
  (setq company-show-numbers t)
  (setq company-idle-delay 0.3)
  (setq company-clang-insert-arguments nil)
  (setq company-require-match nil)
  (setq company-etags-ignore-case t)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)

  (define-keys :map company-active-map
    ("C-n" . company-select-next)
    ("C-p" . company-select-previous)
    ("M-n" . company-next-page)
    ("M-p" . company-previous-page))

  ;; press SPACE will accept the highlighted candidate and insert a space
  ;; `M-x describe-variable company-auto-complete-chars` for details
  ;; That's BAD idea.
  (setq company-auto-complete nil)
  ;; NOT to load company-mode for certain major modes.
  (setq company-global-modes
        '(not eshell-mode
              comint-mode erc-mode gud-mode rcirc-mode shell-mode
              minibuffer-inactive-mode)))

(with-eval-after-load 'company-etags
  (add-to-list 'company-etags-modes 'web-mode))

(provide 'init-company)
