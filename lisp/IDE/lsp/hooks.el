;; -*- lexical-binding:t -*-

(after! lsp-mode
  (when ymacs-editor-use-childframe-p
    (define-advice keyboard-quit (:before () hide-lsp-doc)
      (when (and lsp-mode
                 (eq eldoc-message-function #'ymacs-lsp//eldoc-message))
        (posframe-hide ymacs-lsp-doc-buffer))))

  (define-hook! ymacs-lsp|after-open (lsp-after-open-hook)
    (setq-local eldoc-documentation-function #'eldoc-documentation-compose-eagerly)
    (setq-local eldoc-message-function #'ymacs-lsp//eldoc-message)

    (remove-hook 'eldoc-documentation-functions #'flymake-eldoc-function t)
    (add-hook 'eldoc-documentation-functions #'flymake-eldoc-function -20 t)
    (when (lsp--capability :hoverProvider)
      (add-hook 'eldoc-documentation-functions #'ymacs-lsp//eldoc-function -10 t))

    (setq ymacs-editor-prefer-imenu-p t)
    (setq-local company-minimum-prefix-length 2))

  (define-hook! ymacs-lsp//set-lsp-signature-frame-params (lsp-signature-mode-hook)
    (setq lsp-signature-function
          (if (and ymacs-editor-use-childframe-p
                   (display-graphic-p))
              #'lsp-signature-posframe
            #'lsp-lv-message))
    (ymacs-lsp//set-lsp-signature-width))

  (define-advice lsp--calculate-root (:around (-fn -session -file-name) use-truename)
    (funcall -fn -session (file-truename -file-name)))

  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)

  (advice-add #'lsp-download-install :override #'lsp-download-install@pretty))

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
