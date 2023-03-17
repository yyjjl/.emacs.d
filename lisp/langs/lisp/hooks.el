;; -*- lexical-binding:t -*-

(define-hook! ymacs-lisp//minibuffer-setup (minibuffer-setup-hook
                                            minibuffer-exit-hook)
  (if (and (not (bound-and-true-p ymacs-lisp-minor-mode))
           (memq this-command
                 '(eval-expression
                   pp-eval-expression
                   ibuffer-do-eval
                   ibuffer-do-view-and-eval)))
      (ymacs-lisp-minor-mode 1)
    (ymacs-lisp-minor-mode -1)))

(add-hook 'emacs-lisp-mode-hook #'ymacs-lisp//elisp-setup)
(add-hook 'lisp-interaction-mode-hook #'ymacs-lisp//elisp-setup)

(dolist (hook '(lisp-mode-hook scheme-mode-hook lisp-data-mode-hook))
  (add-hook hook #'ymacs-lisp//common-setup))

(after! macrostep
  (define-hook! ymacs-lisp//setup-macrostep (macrostep-mode-hook)
    (if macrostep-mode
        (ymacs-lisp-minor-mode -1)
      (ymacs-lisp-minor-mode 1))))

(after! auto-compile
  (define-advice auto-compile-byte-compile (:around (-fn &optional -file -start) native)
    (when (and (funcall -fn -file -start)
               (native-comp-available-p))
      (with-demoted-errors "%s"
        (message "ELN: %s" (native-compile (or -file (buffer-file-name))))))))
