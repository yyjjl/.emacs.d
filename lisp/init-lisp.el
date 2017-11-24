(require-packages!
 ;; Auto compile after .el file load or save
 auto-compile
 ;; pair edit
 lispy
 macrostep
 geiser
 hl-sexp

 sicp)



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
  (rainbow-delimiters-mode 1)
  (lispy-mode 1)
  (local-set-key (kbd "M-,") 'xref-pop-marker-stack))

;; Hippie-expand
(defun lisp%hippie-expand-setup ()
  "Locally set `hippie-expand' completion functions for use with
Emacs Lisp."
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list
               'try-complete-lisp-symbol
               :append)
  (add-to-list 'hippie-expand-try-functions-list
               'try-complete-lisp-symbol-partially
               :append))

(defun lisp|scheme-setup ()
  (lisp|common-setup)
  (setq-local flycheck-check-syntax-automatically
              '(save mode-enabled)))

(defun lisp|elisp-setup ()
  (lisp|common-setup)
  (flycheck-mode -1)

  (setq company-backends (remove 'company-capf company-backends))
  (add-to-list 'company-backends 'company-elisp)

  (unless (buffer-temporary?)
    (when (require 'eldoc nil t)
      (setq eldoc-idle-delay 0.2)
      (setq eldoc-echo-area-use-multiline-p t))
    (require 'semantic/bovine/el nil t)
    (auto-compile-on-save-mode)

    (lisp%hippie-expand-setup)

    (checkdoc-minor-mode)))

(add-hook 'emacs-lisp-mode-hook #'lisp|elisp-setup)
(add-hook 'scheme-mode-hook #'lisp|scheme-setup)

(let ((hooks '(lisp-mode-hook
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

(with-eval-after-load 'geiser
  (setq geiser-active-implementations '(racket)))

(with-eval-after-load 'elisp-mode
  (define-key emacs-lisp-mode-map (kbd "C-c e") 'macrostep-expand)
  (define-key lisp-interaction-mode-map (kbd "C-c e") 'macrostep-expand))

(defun lisp/transpose-sexp ($n)
  (when (/= $n 0)
    (let ((bounds (bounds-of-thing-at-point 'sexp)))
      (if bounds
          (progn
            (let ((dist (or (and bounds (- (cdr bounds) (point))) 0)))
              (unless dist
                (forward-char 1))
              (transpose-sexps $n)
              (backward-char dist)))
        (message "Not in a sexp")))))

(defun lisp/transpose-sexp-down (&optional $n)
  (interactive "P")
  (unless $n (setq $n 1))
  (lisp/transpose-sexp $n))

(defhydra hydra-sexp (:exit nil)
  "Sexp"
  ("SPC" (if (region-active-p)
             (deactivate-mark)
           (mark-sexp)) "mark")
  ("." lisp/transpose-sexp-down "->")
  ("," lisp/transpose-sexp-up "<-"))

(defun lisp/transpose-sexp-up (&optional $n)
  (interactive "P")
  (unless $n (setq $n 1))
  (lisp/transpose-sexp (- 0 $n)))

(define-key!
  ("C->" . hydra-sexp/lisp/transpose-sexp-down)
  ("C-<" . hydra-sexp/lisp/transpose-sexp-up))

(provide 'init-lisp)
