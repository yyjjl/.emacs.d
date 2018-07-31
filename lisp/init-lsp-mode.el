;; -*- lexical-binding:t -*-
(require-packages!
 lsp-mode
 lsp-ui
 company-lsp)



(autoload 'lsp-ui-flycheck-enable "lsp-ui-flycheck")

(custom-theme-set-faces
 'doom-molokai
 '(lsp-face-highlight-textual ((t :background "#444155"))))


(defvar lsp--code-action-overlay nil)
(defun lsp//move-code-action-overlay ($beg $end &optional $buffer)
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

(defun lsp*after-set-code-action-params ($buffer $actions $params)
  (when (buffer-live-p $buffer)
    (with-current-buffer $buffer
      (when (equal $params (lsp--text-document-code-action-params))
        (when lsp--code-action-overlay
          (delete-overlay lsp--code-action-overlay))
        (setq lsp-code-actions $actions)
        (setq lsp-code-action-params $params)
        (when $actions
          (when-let (bounds (bounds-of-thing-at-point 'symbol))
            (lsp//move-code-action-overlay (car bounds) (cdr bounds) $buffer)))))))

(with-eval-after-load 'lsp-mode
  (require 'lsp-imenu)
  ;; (setq lsp-eldoc-render-all nil)
  (setq lsp-enable-completion-at-point nil)
  (setq lsp-highlight-symbol-at-point t)
  (setq lsp--show-doc-key "C-c C-d")
  (setq lsp-hover-text-function 'lsp--text-document-hover-string)
  ;; (setq lsp--show-doc-extra-message
  ;;       (propertize (format " (`%s' to see more ...)" lsp--show-doc-key)
  ;;                   'face font-lock-comment-face))

  (define-key!
    ("M-s h h" . lsp-symbol-highlight)
    ("C-c r" . lsp-rename)
    ((kbd lsp--show-doc-key) . lsp-describe-thing-at-point)
    ("C-c ." . lsp-info-under-point)
    ("C-c C-SPC" . lsp-execute-code-action))

  (define-hook! lsp|after-open (lsp-after-open-hook)
    (lsp-enable-imenu)
    (lsp-ui-flycheck-enable 1))

  (advice-add 'lsp--set-code-action-params
              :override #'lsp*after-set-code-action-params))

(with-eval-after-load 'company-lsp
  (setq company-lsp-async t))

(provide 'init-lsp-mode)
