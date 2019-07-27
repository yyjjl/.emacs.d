(setvar! lisp-has-racket-p (executable-find "racket"))
(require-packages!
 ;; Auto compile after .el file load or save
 auto-compile
 ;; pair edit
 lispy
 elisp-def
 macrostep
 (racket-mode :when lisp-has-racket-p))

(defface lisp-argument-face
  `((t :underline t))
  "Face for arguments"
  :group 'lisp)



(defun lisp/describe-at-point ()
  (interactive)
  (let* ((symbol (symbol-at-point))
         (symbol-types (and symbol (remove nil (list (when (symbol-function symbol) 'function)
                                              (when (boundp symbol) 'variable))))))
    (unless symbol
      (user-error "Nothing is at point."))
    (unless symbol-types
      (user-error "`%s' is undefined" symbol))
    (let ((symbol-type (if (null (cdr symbol-types))
                           (car symbol-types)
                         (intern (ivy-read "Type: " symbol-types :require-match t)))))
      (cl-case symbol-type
        (function (describe-function symbol))
        (variable (describe-variable symbol))))))

(defun edebug/remove-all-instrumentation ()
  "Remove all edebug instrumentation by visiting each function
definition and running `eval-defun`."
  (interactive)
  (mapatoms (lambda (symbol)
              (when-let (pos (car-safe (get symbol 'edebug)))
                (with-current-buffer (marker-buffer pos)
                  (goto-char (marker-position pos))
                  (eval-defun nil))))))

(define-hook! lisp|minibuffer-setup (minibuffer-setup-hook
                                     minibuffer-exit-hook)
  (if (and (not (bound-and-true-p lispy-mode))
           (memq this-command
                 '(eval-expression
                   pp-eval-expression
                   core/eval-and-replace
                   eval-expression-with-eldoc
                   ibuffer-do-eval
                   ibuffer-do-view-and-eval)))
      (lispy-mode 1)
    (lispy-mode -1)))


(defun lisp|common-setup ()
  "Enable features useful in any Lisp mode."
  (highlight-indentation-mode -1)
  (rainbow-delimiters-mode 1)
  ;; (rainbow-delimiters-count-mode 1)
  (lispy-mode 1)
  (local-set-key (kbd "M-,") 'xref-pop-marker-stack))

;; Hippie-expand
(defun lisp//hippie-expand-setup ()
  "Locally set `hippie-expand' completion functions for use with
Emacs Lisp."
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list
               'try-complete-lisp-symbol
               :append))

(defun lisp|racket-setup ()
  (lisp|common-setup)
  ;; (setq eldoc-documentation-function 'racket-eldoc-function)
  (when (buffer-temporary-p)
    (setq completion-at-point-functions nil))
  (flycheck-mode 1)

  (setq-local flycheck-check-syntax-automatically
              '(save mode-enabled))

  (with-local-minor-mode-map! 'lispy-mode
    (lispy-define-key it "e" #'racket-eval-sexp)
    (lispy-define-key it "i" #'racket-indent-sexp)))

(defun lisp|elisp-setup ()
  (lisp|common-setup)
  (flycheck-mode -1)

  (unless (buffer-temporary-p)
    (auto-compile-on-save-mode)

    (lisp//hippie-expand-setup)
    (checkdoc-minor-mode)))

(add-hook 'emacs-lisp-mode-hook #'lisp|elisp-setup)
(add-hook 'lisp-interaction-mode-hook #'lisp|elisp-setup)
(add-hook 'racket-mode-hook #'lisp|racket-setup)

(dolist (hook '(lisp-mode-hook scheme-mode-hook))
  (add-hook hook #'lisp|common-setup))



(with-eval-after-load 'lisp-mode
  (require 'hippie-exp)
  ;; Add keyword `define-hook!'
  (font-lock-add-keywords
   'emacs-lisp-mode
   '(("(\\(define-hook!\\)\\_>[         '(]*\\(\\(?:\\sw\\|\\s_\\)+\\)?"
      (1 font-lock-keyword-face)
      (2
       (let ((type (get (intern-soft (match-string 1)) 'lisp-define-type)))
         (cond ((eq type 'var)
                font-lock-variable-name-face)
               ((eq type 'type)
                font-lock-type-face)
               (t font-lock-function-name-face)))
       nil t))
     ("\\_<@?\\(-[a-zA-Z]\\(?:\\sw\\|\\s_\\)*\\)"
      (1 'lisp-argument-face nil nil)))))

(with-eval-after-load 'racket-mode
  (add-to-list 'lispy-goto-symbol-alist
               '(racket-mode racket-visit-definition racket-mode))

  (defun racket-indent-sexp ()
    (interactive)
    (unless (region-active-p)
      (lispy-mark-list 1))
    (call-interactively #'indent-region)
    (lispy-different))

  (defun racket-eval-sexp ()
    (interactive)
    (unless (region-active-p)
      (lispy-mark-list 1))
    (call-interactively #'racket-send-region))

  ;; Use `ivy' as default completion backends
  (defun racket--read-identifier (-prompt -default)
    (ivy-read -prompt
              (racket--get-namespace-symbols)
              :initial-input -default
              :preselect -default)))

(with-eval-after-load 'elisp-mode
  (require 'semantic/bovine/el)

  (define-key! :map emacs-lisp-mode-map
    ("C-c e" . macrostep-expand)
    ("C-c C-d" . lisp/describe-at-point))

  (define-key! :map lisp-interaction-mode-map
    ("C-c e" . macrostep-expand)
    ("C-c C-d" . lisp/describe-at-point)))

(with-eval-after-load 'lispy
  (define-key! :map lispy-mode-map ("M-n"))

  (defun lisp*around-goto-symbol (-fn -symbol)
    (if (memq major-mode lispy-elisp-modes)
        (condition-case nil
            (elisp-def)
          (error (lispy-goto-symbol-elisp -symbol)))
      (funcall -fn -symbol)))
  (advice-add 'lispy-goto-symbol :around #'lisp*around-goto-symbol))

(provide 'init-lisp)
