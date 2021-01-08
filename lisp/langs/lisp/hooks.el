;; -*- lexical-binding:t -*-

(define-hook! ymacs-lisp|minibuffer-setup (minibuffer-setup-hook
                                           minibuffer-exit-hook)
  (if (and (not (bound-and-true-p lispy-mode))
           (memq this-command
                 '(eval-expression
                   pp-eval-expression
                   ymacs-tools/eval-and-replace
                   eval-expression-with-eldoc
                   ibuffer-do-eval
                   ibuffer-do-view-and-eval)))
      (lispy-mode 1)
    (lispy-mode -1)))

(add-hook 'emacs-lisp-mode-hook #'ymacs-lisp|elisp-setup)
(add-hook 'lisp-interaction-mode-hook #'ymacs-lisp|elisp-setup)

(dolist (hook '(lisp-mode-hook scheme-mode-hook))
  (add-hook hook #'ymacs-lisp|common-setup))

(after! macrostep
  (define-hook! ymacs-lisp|macrostep-setup (macrostep-mode-hook)
    (add-to-list 'minor-mode-overriding-map-alist (cons 'macrostep-mode macrostep-keymap))))

(after! lispy
  (define-advice lispy-goto-symbol (:around (-fn -symbol) try-elisp-def)
    (if (memq major-mode lispy-elisp-modes)
        (condition-case nil
            (elisp-def)
          (user-error (lispy-goto-symbol-elisp -symbol)))
      (funcall -fn -symbol))))

(after! auto-compile
  (define-advice auto-compile-byte-compile (:around (-fn &optional -file -start) native)
    (when (and (funcall -fn -file -start)
               (fboundp 'native-compile))
      (with-demoted-errors "%s"
        (message "ELN: %s" (native-compile (or -file (buffer-file-name))))))))