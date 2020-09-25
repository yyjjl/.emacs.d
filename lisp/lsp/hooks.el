;; -*- lexical-binding:t -*-

(setq lsp-keymap-prefix nil)
(after! lsp
  (define-hook! ymacs-lsp|after-open (lsp-after-open-hook)
    ;; default to sort and filter by server
    (setq-local company-transformers nil))

  (define-advice lsp-lv-message (:override (-message) escape)
    (if -message
        (progn
          (setq lsp--signature-last-buffer (current-buffer))
          (let ((lv-force-update t))
            (lv-message (replace-regexp-in-string "%" "%%" -message))))
      (lv-delete-window)))

  (define-advice lsp--render-on-hover-content (:around (-fn -contents -render-all) truncate-doc)
    (let ((content (funcall -fn -contents -render-all)))
      (unless (ymacs-popups//help-buffer-p (current-buffer))
        (let ((content-length (length content))
              (split-pos (string-match (rx line-end) content)))
          (when (or (< split-pos content-length)
                    (>= split-pos (frame-width)))
            (setq content
                  (concat (substring content 0 (min split-pos (- (frame-width) 30)))
                          (propertize
                           (format " ... (%s to see more)"
                                   (substitute-command-keys "\\[lsp-describe-thing-at-point]"))
                           'face 'font-lock-comment-face))))))
      content)))

(after! lsp-headerline
  (defun lsp-headerline--filename-with-icon@disable-icon (-file-path)
    (f-filename -file-path))

  (lsp-defun lsp-headerline--symbol-icon@disable-icon ((&DocumentSymbol :kind))
    "Build the SYMBOL icon for headerline breadcrumb."
    (when (require 'lsp-treemacs nil t)
      (format "[%s] " (lsp-treemacs-symbol-kind->icon kind))))

  (advice-add
   #'lsp-headerline--symbol-icon
   :override #'lsp-headerline--symbol-icon@disable-icon)
  (advice-add
   #'lsp-headerline--filename-with-icon
   :override #'lsp-headerline--filename-with-icon@disable-icon))

(after! dap-mode
  (add-hook 'python-mode-hook (lambda () (require 'dap-python)))
  (add-hook 'dap-session-created-hook #'dap-hydra)
  (add-hook 'dap-stopped-hook #'dap-hydra)
  (add-hook 'dap-terminated-hook #'dap-hydra/nil))
