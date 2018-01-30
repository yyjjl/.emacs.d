;; -*- lexical-binding:t -*-
(require-packages!
 lsp-mode
 company-lsp)



(custom-theme-set-faces
 'doom-molokai
 '(lsp-face-highlight-textual ((t :background "#444155"))))

(with-eval-after-load 'lsp-mode
  (require 'lsp-flycheck)
  (require 'lsp-imenu)

  (setq lsp-enable-completion-at-point nil
        lsp-highlight-symbol-at-point nil)

  (define-key!
    ("M-s h h" . lsp-symbol-highlight)
    ("C-c r" . lsp-rename)
    ("C-c ." . lsp-info-under-point)
    ("C-c C-SPC" . lsp-execute-code-action))

  (define-hook! lsp|after-initialize (lsp-after-initialize-hook)
    (when lsp--cur-workspace
      (when-let* ((client (lsp--workspace-client lsp--cur-workspace))
                  (stderr (lsp--client-stderr client))
                  (proc (get-buffer-process stderr)))
        (set-process-query-on-exit-flag proc nil))))

  (defun lsp%flycheck-check ()
    (when (and flycheck-mode
               (not lsp--flycheck-global-idle-timer))
      (setq lsp--flycheck-global-idle-timer
            (run-with-idle-timer 1 nil
                                 (lambda ()
                                   (setq lsp--flycheck-global-idle-timer nil)
                                   (ignore-errors
                                     (when flycheck-mode
                                       (flycheck-buffer))))))))

  (defvar lsp--flycheck-global-idle-timer nil)
  (define-hook! lsp|after-open (lsp-after-open-hook)
    (lsp-enable-imenu)
    (setq lsp-after-diagnostics-hook #'lsp%flycheck-check))

  (defvar lsp--code-action-overlay nil)
  (defun lsp--move-code-action-overlay ($beg $end &optional $buffer)
    (unless (overlayp lsp--code-action-overlay)
      (setq lsp--code-action-overlay (make-overlay $beg $end $buffer))
      (overlay-put lsp--code-action-overlay
                   'face '(:underline (:style wave :color "blue")))
      (overlay-put lsp--code-action-overlay
                   'modification-hooks
                   '((lambda (ov after-change &rest args)
                       (when after-change
                         (delete-overlay ov))))))
    (move-overlay lsp--code-action-overlay $beg $end $buffer))

  (defun lsp--make-code-action-callback (buf)
    (lambda (actions)
      (with-current-buffer buf
        (setq lsp-code-actions actions)
        (when lsp--code-action-overlay
          (delete-overlay lsp--code-action-overlay))
        (when actions
          (-when-let (bounds (bounds-of-thing-at-point 'symbol))
            (lsp--move-code-action-overlay (car bounds)
                                           (cdr bounds)
                                           buf)))))))

(with-eval-after-load 'company-lsp
  (setq company-lsp-async t))

(provide 'init-lsp-mode)
