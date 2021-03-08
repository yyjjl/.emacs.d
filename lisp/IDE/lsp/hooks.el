;; -*- lexical-binding:t -*-

(declare-function lsp--render-element 'lsp-mode)

(after! lsp-mode
  ;; FIX the bug which causes flymake diagnostics disappeared 
  (define-advice lsp-eldoc-function (:around (-fn) checker)
    (or (when-let (diags (flymake-diagnostics (point)))
          (mapconcat #'flymake-diagnostic-text diags "\n"))
        (funcall -fn)))

  (define-hook! ymacs-lsp|after-open (lsp-after-open-hook)
    (when (and ymacs-lsp-use-modern-ui-p
               (display-graphic-p))
      (setq-local lsp-eldoc-enable-hover nil))

    (remove-hook 'eldoc-documentation-functions 'flymake-eldoc-function t)

    (setq-local company-minimum-prefix-length 2)
    ;; default to sort and filter by server
    (setq-local company-transformers nil))

  (define-hook! ymacs-lsp//set-lsp-signature-frame-params (lsp-signature-mode-hook)
    (setq lsp-signature-function
          (if (and ymacs-editor-use-childframe-p
                   (display-graphic-p))
              #'lsp-signature-posframe
            #'lsp-lv-message))
    (setq lsp-signature-posframe-params
          (plist-put lsp-signature-posframe-params
                     :width (max 60 (min (/ (frame-width) 2) (window-width))))))

  (define-advice lsp--calculate-root (:around (-fn -session -file-name) use-truename)
    (funcall -fn -session (file-truename -file-name)))

  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)

  (advice-add #'lsp-download-install :override #'lsp-download-install@pretty)
  (advice-add #'lsp-async-start-process :override #'lsp-async-start-process@pretty))

(after! lsp-modeline
  (setq lsp-modeline-code-actions-segments '(count name)))

(eval-when! ymacs-lsp-use-dap-p
  (after! dap-mode
    (define-advice dap-debug (:before (&rest _args) save-window-configuration)
      (ymacs-debug//before-debug))

    ;; Activate this minor mode when dap is initialized
    (define-hook! (ymacs-dap//stopped _session) (dap-stopped-hook)
      (unless ymacs-dap-running-session-mode
        (ymacs-dap-running-session-mode 1)))

    (define-hook! (ymacs-dap//terminated _session) (dap-terminated-hook)
      (ymacs-debug//after-debug #'ymacs-dap-running-session-mode)
      (dap-hydra/nil))

    (define-hook! (ymacs-dap//stack-frame-changed -session) (dap-stack-frame-changed-hook)
      (when (and (dap--session-running -session)
                 (not ymacs-dap-running-session-mode))
        (ymacs-dap-running-session-mode 1)))))
