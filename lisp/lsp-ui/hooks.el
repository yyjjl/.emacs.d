;; -*- lexical-binding:t -*-

(after! lsp-mode
  (define-hook! ymacs-lsp-ui|after-open (lsp-after-open-hook)
    (ymacs-lsp-ui//enable (display-graphic-p))))

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

(after! lsp-ui-doc
  (define-advice lsp-ui-doc--mv-at-point
      (:around (-fn -width -height -window-left -window-top) smart)
    (-let* ((point-top (cdr (posn-x-y (posn-at-point (point)))))
            (header-line-height (lsp-ui-doc--line-height 'header-line)))
      (if (> -window-left (+ -width 10))
          ;; Case 1: put doc frame in the -window-left
          (cons (- -window-left -width 10)
                (max (- (+ -window-top point-top header-line-height)
                        (/ -height 2))
                     0))
        ;; Case 2: try find best position
        (-let* (((best-left . best-top)
                 (ymacs-lsp-ui//doc-find-position-smart
                  -height
                  (+ point-top (/ -height 2)))))
          (if (< best-left (* 0.75 (window-pixel-width)))
              (cons (+ -window-left best-left 5) ; with margin of 5
                    (+ -window-top best-top))
            ;; Case 3: fallback
            (funcall -fn -width -height -window-left -window-top)))))))
