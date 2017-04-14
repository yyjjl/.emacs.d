(defun conditionally-enable-lispy (&optional arg)
  (if (and (memq this-command
              '(eval-expression
                pp-eval-expression
                eval-expression-with-eldoc
                ibuffer-do-eval
                ibuffer-do-view-and-eval))
	   arg)
      (lispy-mode 1)
    (lispy-mode -1)))

(defun indent-for-tab-or-close ()
  (interactive)
  (if (looking-at "`\\|\"")
      (forward-char 1)
    (indent-for-tab-command)))

(defun my-lisp-setup ()
  "Enable features useful in any Lisp mode."
  (rainbow-delimiters-mode 1)
  (hl-sexp-mode 1)
  (unless (> (buffer-size) 51200)
    (prettify-symbols-mode 1))
  (lispy-mode 1)
  (if (eq major-mode 'racket-mode)
      (progn (setq flycheck-check-syntax-automatically
                   '(save mode-enabled)
                   eldoc-documentation-function 'racket-eldoc-function)
             (unless (buffer-file-name)
               (setq completion-at-point-functions nil)))
    (flycheck-mode -1))
  (local-set-key (kbd "M-,") 'pop-tag-mark)
  (local-set-key (kbd "<tab>") 'indent-for-tab-or-close))

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
  (my-lisp-setup)
  (unless (is-buffer-file-temp)
    (when (require 'eldoc nil t)
      (setq eldoc-idle-delay 0.2)
      (setq eldoc-echo-area-use-multiline-p t))
    (when (require 'semantic/bovine/el nil t)
      (semantic-mode 1))
    (auto-compile-on-save-mode)
    (set-up-hippie-expand-for-elisp)

    (checkdoc-minor-mode)))



(add-hook 'emacs-lisp-mode-hook 'elisp-mode-hooks)
(let ((lispy-hooks '(lisp-mode-hook
                     ;; scheme-mode-hook
                     racket-mode-hook
                     inferior-lisp-mode-hook
                     lisp-interaction-mode-hook)))
  (dolist (hook lispy-hooks)
    (add-hook hook 'my-lisp-setup)))


;; ----------------------------------------------------------------------------
;; Highlight current sexp
;;----------------------------------------------------------------------------
;;Prevent flickery behaviour due to hl-sexp-mode unhighlighting
;; before each command
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

(with-eval-after-load 'lispy
  (add-to-list 'lispy-goto-symbol-alist
               '(racket-mode racket-visit-definition racket-mode)))

(with-eval-after-load 'racket-mode
  (defun racket-complete-at-point-hack (fn &rest args)
    (when (and (buffer-file-name) (racket--in-repl-or-its-file-p))
      (apply fn args)))
  (advice-add 'racket-complete-at-point
              :around #'racket-complete-at-point-hack)
  (defun racket--read-identifier (prompt default)
    (ivy-read prompt
              (racket--get-namespace-symbols)
              :initial-input default    ;initial
              :preselect default)))

(setq-default initial-scratch-message
              (concat ";; Welcome to Emacs " (or user-login-name "")))

(provide 'init-lisp)
