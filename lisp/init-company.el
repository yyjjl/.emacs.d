(with-eval-after-load 'company
  (company-statistics-mode 1)
  ;; can't work with TRAMP
  (make-variable-buffer-local 'company-backends)
  (setq-default company-backends (delete 'company-ropemacs company-backends))
  (setq-default company-backends (delete 'company-semantic company-backends))
  (setq-default company-backends (delete 'company-clang company-backends))
  ;; make company-files a work before capf
  ;; (setq company-backends (delete 'company-capf company-backends))
  (setq-default company-backends (delete 'company-files  company-backends))
  ;; company-files before company-capf
  (setq-default company-backends (cons 'company-files company-backends))

  ;; I don't like the downcase word in company-dabbrev
  ;; for languages use camel case naming convention
  ;; company should be case sensitive
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case nil)
  (setq company-dabbrev-code-ignore-case t)
  (setq company-show-numbers t)
  (setq company-idle-delay 0.2)
  (setq company-clang-insert-arguments nil)
  (setq company-require-match nil)
  (setq company-etags-ignore-case t)
  (setq company-minimum-prefix-length 2)
  (setq company-tooltip-align-annotations t)

  (bind-keys :map company-active-map
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous)
             ("M-n" . company-next-page)
             ("M-p" . company-previous-page))

  ;; @see https://github.com/redguardtoo/emacs.d/commit/2ff305c1ddd7faff6dc9fa0869e39f1e9ed1182d
  (defadvice company-in-string-or-comment
      (around company-in-string-or-comment-hack activate)
    ;; you can use (ad-get-arg 0) and (ad-set-arg 0) to tweak the arguments
    (if (memq major-mode '(php-mode html-mode web-mode nxml-mode))
        (setq ad-return-value nil)
      ad-do-it))

  ;; press SPACE will accept the highlighted candidate and insert a space
  ;; `M-x describe-variable company-auto-complete-chars` for details
  ;; That's BAD idea.
  (setq company-auto-complete nil)
  ;; NOT to load company-mode for certain major modes.
  ;; Ironic that I suggested this feature but I totally forgot it
  ;; until two years later.
  ;; https://github.com/company-mode/company-mode/issues/29
  (setq company-global-modes
        '(not
          eshell-mode comint-mode erc-mode gud-mode rcirc-mode
          minibuffer-inactive-mode)))

;; {{ setup company-ispell
(defun toggle-company-ispell ()
  (interactive)
  (cond
   ((memq 'company-ispell company-backends)
    (setq company-backends (delete 'company-ispell company-backends))
    (message "company-ispell disabled"))
   (t
    ;; company-ispell need to be after company-capf
    ;; or it will stop use completions
    (let ((index (cl-position 'company-capf company-backends)))
      (if index
          (add-to-list-after index 'company-ispell company-backends)
        (add-to-list 'company-backends 'company-ispell)))
    (message "company-ispell enabled!"))))

(with-eval-after-load 'company-etags
  (add-to-list 'company-etags-modes 'web-mode))

(with-eval-after-load 'company-shell
  (setq company-shell-modes '(sh-mode)))

(add-hook 'cmake-mode-hook '(lambda ()
                              (add-to-list 'company-backends 'company-cmake)))

(provide 'init-company)
