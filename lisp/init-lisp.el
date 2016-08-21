;; ----------------------------------------------------------------------------
;; Paredit
;; ----------------------------------------------------------------------------
(defvar paredit-minibuffer-commands '(eval-expression
                                      pp-eval-expression
                                      eval-expression-with-eldoc
                                      ibuffer-do-eval
                                      ibuffer-do-view-and-eval)
  "Interactive commands for which paredit should be enabled in the minibuffer.")

(with-eval-after-load 'paredit
  (defun reverse-transpose-sexps (arg)
    (interactive "*p")
    (transpose-sexps (- arg))
    (backward-sexp (1+ arg))
    (forward-sexp 1))

  (bind-keys :map paredit-mode-map
             ("C-M-f" . nil) ("C-M-d" . nil) ("C-M-u" . nil)
             ("C-M-b" . nil) ("C-M-p" . nil) ("C-M-n" . nil)
             ("C-." . paredit-forward)
             ("C-," . paredit-backward)
             ("M-p" . paredit-backward-down)
             ("M-n" . paredit-forward-up)
             ("M-]" . paredit-forward-down)
             ("M-[" . paredit-backward-up)
             ("C-<backspace>" . paredit-backward-kill-word)
             ("M-<backspace>" . backward-kill-word)
             ("C->" . transpose-sexps)
             ("C-<" . reverse-transpose-sexps)))



(defun conditionally-paredit-mode (flag)
  "Enable paredit during lisp-related minibuffer commands."
  (if (memq this-command paredit-minibuffer-commands)
      (paredit-mode flag)))


;; ----------------------------------------------------------------------------
;; ENABLE desired features for all lisp modes
;; ----------------------------------------------------------------------------
;; this function is a part of preclude emacs
(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun my-lisp-setup ()
  "Enable features useful in any Lisp mode."
  (enable-paredit-mode)

  (rainbow-delimiters-mode t)
  (semantic-mode 1)
  (show-paren-mode 1)
  (hl-sexp-mode 1)
  (prettify-symbols-mode 1)

  (local-set-key (kbd "C-c e") 'eval-and-replace)
  (local-set-key (kbd "C-c c") 'comment-region)
  (local-set-key (kbd "C-c u") 'uncomment-region)
  (local-set-key (kbd "C-c l") 'comment-line))


;; ----------------------------------------------------------------------------
;; Hippie-expand
;; ----------------------------------------------------------------------------

(defun set-up-hippie-expand-for-elisp ()
  "Locally set `hippie-expand' completion functions for use with Emacs Lisp."
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol t)
  (add-to-list 'hippie-expand-try-functions-list 'try-complete-lisp-symbol-partially t))

(defun elisp-mode-hooks ()
  "lisp-mode-hooks"
  (unless (is-buffer-file-temp)

    (when (require 'eldoc nil t)
      (setq eldoc-idle-delay 0.2)
      (setq eldoc-echo-area-use-multiline-p t)
      (eldoc-mode))
    (auto-compile-on-save-mode)
    (set-up-hippie-expand-for-elisp)

    (my-lisp-setup)

    (checkdoc-minor-mode)))



(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hooks)

(let ((lispy-hooks '(lisp-mode-hook
                      inferior-lisp-mode-hook
                      lisp-interaction-mode-hook)))
  (dolist (hook lispy-hooks)
    (add-hook hook 'my-lisp-setup)))


;; ----------------------------------------------------------------------------
;; Highlight current sexp
;;----------------------------------------------------------------------------
;;Prevent flickery behaviour due to hl-sexp-mode unhighlighting before each command
(with-eval-after-load 'hl-sexp
  (defadvice hl-sexp-mode (after unflicker (turn-on) activate)
     (when turn-on
       (remove-hook 'pre-command-hook #'hl-sexp-unhighlight))))

(with-eval-after-load 'lisp-mode
  (setq lisp--prettify-symbols-alist
        (append lisp--prettify-symbols-alist
                '(("or" . ?∨)
                  ("and" . ?∧)
                  ("not" . ?¬)
                  (">=" . ?≥)
                  ("<=" . ?≤)))))

(setq-default initial-scratch-message
              (concat ";; Welcome to Emacs " (or user-login-name "")))

;; {{ scheme setup
;; (setq scheme-program-name "guile")
;; (eval-after-load 'scheme-mode
;;   '(progn
;;      (require 'quack)))
;; }}

;; A quick way to jump to the definition of a function given its key binding
(global-set-key (kbd "C-h K") 'find-function-on-key)

(add-to-list 'auto-mode-alist '("\\.emacs-project\\'" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("archive-contents\\'" . emacs-lisp-mode))


(provide 'init-lisp)
