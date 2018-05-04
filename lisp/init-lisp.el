(setvar! lisp-has-racket-p (executable-find "racket"))
(require-packages!
 ;; Auto compile after .el file load or save
 auto-compile
 ;; pair edit
 lispy
 elisp-def
 macrostep
 hl-sexp
 (racket-mode :when lisp-has-racket-p))



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
  (when (buffer-temporary?)
    (setq completion-at-point-functions nil))
  (setq flycheck-check-syntax-automatically
        '(save mode-enabled))
  (with-local-minor-mode-map! 'lispy-mode
    (lispy-define-key it "e" #'racket-eval-sexp)
    (lispy-define-key it "i" #'racket-indent-sexp)))

(defun lisp|elisp-setup ()
  (lisp|common-setup)
  (flycheck-mode -1)

  (unless (buffer-temporary?)
    (require 'semantic/bovine/el nil t)
    (auto-compile-on-save-mode)

    (lisp//hippie-expand-setup)
    (checkdoc-minor-mode)))

(add-hook 'emacs-lisp-mode-hook #'lisp|elisp-setup)
(add-hook 'lisp-interaction-mode-hook #'lisp|elisp-setup)
(add-hook 'racket-mode-hook #'lisp|racket-setup)

(dolist (hook '(lisp-mode-hook scheme-mode-hook))
  (add-hook hook #'lisp|common-setup))



;; Highlight current sexp. Prevent flickery behaviour due to
;; `hl-sexp-mode' un-highlighting before each command
(with-eval-after-load 'hl-sexp
  (defadvice hl-sexp-mode (after unflicker (turn-on) activate)
    (when turn-on
      (remove-hook 'pre-command-hook #'hl-sexp-unhighlight))))

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
     ("\\_<@?\\(\\$\\(?:\\sw\\|\\s_\\)+\\)"
      (1 font-lock-constant-face nil nil)))))

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
  (defun racket--read-identifier ($prompt $default)
    (ivy-read $prompt
              (racket--get-namespace-symbols)
              :initial-input $default
              :preselect $default)))

(with-eval-after-load 'elisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c e") 'macrostep-expand)
  (define-key lisp-interaction-mode-map (kbd "C-c e") 'macrostep-expand))

(with-eval-after-load 'lispy
  (defun lisp*goto-symbol-hack ($fn $symbol)
    (if (memq major-mode lispy-elisp-modes)
        (condition-case nil
            (elisp-def)
          (error (lispy-goto-symbol-elisp $symbol)))
      (funcall $fn $symbol)))
  (advice-add 'lispy-goto-symbol :around #'lisp*goto-symbol-hack))

(define-key!
  ([remap move-beginning-of-line] . lispy-move-beginning-of-line)
  ([remap back-to-indentation] . mark-sexp))

(provide 'init-lisp)
