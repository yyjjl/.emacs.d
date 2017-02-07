;; ----------------------------------------------------------------------------
;; Paredit
;; ----------------------------------------------------------------------------
(defvar paredit-minibuffer-commands
  '(eval-expression
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

  (defhydra hydra-paredit ()
    "move"
    ("RET" nil nil)
    ("b" paredit-backward "←")
    ("f" paredit-forward "→")
    ("p" paredit-backward-up "↖")
    ("n" paredit-forward-down "↘")
    ("d" paredit-backward-down "↙")
    ("u" paredit-forward-up "↗")
    ("." transpose-sexps "⇌")
    ("," reverse-transpose-sexps "⇋"))

  (bind-keys :map paredit-mode-map
             ("C-M-f" . hydra-paredit/paredit-forward)
             ("C-M-b" . hydra-paredit/paredit-backward)
             ("C-M-d" . hydra-paredit/paredit-backward-down)
             ("C-M-u" . hydra-paredit/paredit-forward-up)
             ("C-M-n" . hydra-paredit/paredit-forward-down)
             ("C-M-p" . hydra-paredit/paredit-backward-up)
             ("C-<backspace>" . paredit-backward-kill-word)
             ("M-<backspace>" . backward-kill-word)
             ("C-." . hydra-paredit/transpose-sexps)
             ("C-," . hydra-paredit/reverse-transpose-sexps)))

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
      (print (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defun my-lisp-setup ()
  "Enable features useful in any Lisp mode."
  (enable-paredit-mode)
  (rainbow-delimiters-mode 1)
  (when (require 'semantic/bovine/el nil t)
    (try-turn-on-semantic-mode))
  (show-paren-mode 1)
  (hl-sexp-mode 1)
  (prettify-symbols-mode 1)
  (flycheck-mode -1)
  (local-set-key (kbd "M-<RET>") 'srefactor-refactor-at-point)
  (local-set-key (kbd "C-c e") 'eval-and-replace))


;; ----------------------------------------------------------------------------
;; Hippie-expand
;; ----------------------------------------------------------------------------

(defun set-up-hippie-expand-for-elisp ()
  "Locally set `hippie-expand' completion functions for use with Emacs Lisp."
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list
               'try-complete-lisp-symbol t)
  (add-to-list 'hippie-expand-try-functions-list
               'try-complete-lisp-symbol-partially t))

(defun elisp-mode-hooks ()
  "lisp-mode-hooks"
  (unless (is-buffer-file-temp)
    (when (require 'eldoc nil t)
      (setq eldoc-idle-delay 0.2)
      (setq eldoc-echo-area-use-multiline-p t))
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

(provide 'init-lisp)
