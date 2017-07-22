(defhook lisp|minibuffer-setup (minibuffer-setup-hook minibuffer-exit-hook)
  (if (and (not (bound-and-true-p lispy-mode))
           (memq this-command
                 '(eval-expression
                   pp-eval-expression
                   eval-expression-with-eldoc
                   ibuffer-do-eval
                   ibuffer-do-view-and-eval)))
      (lispy-mode 1)
    (lispy-mode -1)))


(defun lisp|common-setup ()
  "Enable features useful in any Lisp mode."
  (rainbow-delimiters-mode 1)
  (unless (> (buffer-size) core|large-buffer-size)
    (prettify-symbols-mode 1))
  (lispy-mode 1)
  (local-set-key (kbd "M-,") 'pop-tag-mark))

(defun lisp|racket-setup ()
  (lisp|common-setup)
  (rainbow-delimiters-mode 1)
  ;; (setq eldoc-documentation-function 'racket-eldoc-function)
  (when (buffer-temporary-p)
    (setq completion-at-point-functions nil))
  (setq flycheck-check-syntax-automatically
        '(save mode-enabled)))

;; Hippie-expand
(defun lisp|hippie-expand-setup ()
  "Locally set `hippie-expand' completion functions for use with
Emacs Lisp."
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list
               'try-complete-lisp-symbol t)
  (add-to-list 'hippie-expand-try-functions-list
               'try-complete-lisp-symbol-partially t))

(defun lisp|elisp-setup ()
  (lisp|common-setup)
  (flycheck-mode -1)
  (define-key emacs-lisp-mode-map (kbd "C-c e") 'macrostep-expand)

  (setq company-backends (remove 'company-capf company-backends))
  (add-to-list 'company-backends 'company-elisp)

  (unless (buffer-temporary-p)
    (when (require 'eldoc nil t)
      (setq eldoc-idle-delay 0.2)
      (setq eldoc-echo-area-use-multiline-p t))
    (require 'semantic/bovine/el nil t)
    (auto-compile-on-save-mode)

    (lisp|hippie-expand-setup)

    (checkdoc-minor-mode)))

(add-hook 'emacs-lisp-mode-hook #'lisp|elisp-setup)
(add-hook 'racket-mode-hook #'lisp|racket-setup)

(let ((hooks '(lisp-mode-hook
               inferior-lisp-mode-hook
               lisp-interaction-mode-hook)))
  (dolist (hook hooks)
    (add-hook hook #'lisp|common-setup)))




;; Highlight current sexp. Prevent flickery behaviour due to
;; `hl-sexp-mode' un-highlighting before each command
(with-eval-after-load 'hl-sexp
  (defadvice hl-sexp-mode (after unflicker (turn-on) activate)
     (when turn-on
       (remove-hook 'pre-command-hook #'hl-sexp-unhighlight))))

(with-eval-after-load 'lisp-mode
  ;; Add keyword `defhook'
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(defhook\\)\\_>[ 	'(]*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
      (1 font-lock-keyword-face)
      (2
       (let ((type (get (intern-soft (match-string 1)) 'lisp-define-type)))
         (cond ((eq type 'var)
                font-lock-variable-name-face)
               ((eq type 'type)
                font-lock-type-face)
               (t font-lock-function-name-face)))
       nil t)))))

(with-eval-after-load 'racket-mode
  ;; Hide error message when racket script is not loaded
  (defun lisp|racket-complete-at-point-advice (fn &rest args)
    (when (and (buffer-file-name) (racket--in-repl-or-its-file-p))
      (let ((result (apply fn args)))
        (when result
          (delete 'racket--get-type (delete :company-docsig result))))))
  (advice-add 'racket-complete-at-point
              :around #'lisp|racket-complete-at-point-advice)
  ;; Use `ivy' as default completion backends
  (defun racket--read-identifier (prompt default)
    (ivy-read prompt
              (racket--get-namespace-symbols)
              :initial-input default
              :preselect default)))

(provide 'init-lisp)
