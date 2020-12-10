;; -*- lexical-binding:t -*-

(after! lsp-mode
  (define-hook! ymacs-lsp-ui|after-open (lsp-after-open-hook)
    (ymacs-lsp-ui//enable (display-graphic-p))))

(after! dap-mode

  ;; Activate this minor mode when dap is initialized
  (define-hook! (ymacs-lsp|dap-session-created _session) (dap-session-created-hook)
    (unless ymacs-lsp/dap-running-session-mode
      (ymacs-lsp/dap-running-session-mode 1))
    (dap-hydra))

  (define-hook! (ymacs-lsp|dap-stopped _session) (dap-stopped-hook)
    (unless ymacs-lsp/dap-running-session-mode
      (ymacs-lsp/dap-running-session-mode 1))
    (dap-hydra))

  (define-hook! (ymacs-lsp|dap-terminated _session) (dap-terminated-hook)
    (ymacs-lsp/dap-running-session-mode -1)
    (dap-hydra/nil))

  (define-hook! (ymacs-lsp|dap-stack-frame-changed -session) (dap-stack-frame-changed-hook)
    (when (and (dap--session-running -session)
               (not ymacs-lsp/dap-running-session-mode))
      (ymacs-lsp/dap-running-session-mode 1))))

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
