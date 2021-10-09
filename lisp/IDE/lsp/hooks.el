;; -*- lexical-binding:t -*-

(defvar ymacs-dap-running-session-mode)
(defvar ymacs-lsp-clear-leak-timer nil)

(declare-function dap-hydra/nil "ext:dap-mode")

(after! lsp-mode
  (setq ymacs-lsp-clear-leak-timer
        (run-with-timer
         30 30
         (lambda ()
           (mapc #'ymacs-lsp//clear-leak-handlers (hash-table-values lsp-clients)))))

  (define-hook! ymacs-lsp|after-open (lsp-after-open-hook)
    (remove-function (local 'eldoc-documentation-function) #'lsp-eldoc-function)

    (remove-hook 'eldoc-documentation-functions #'flymake-eldoc-function t)
    (add-hook 'eldoc-documentation-functions #'flymake-eldoc-function -20 t)

    ;; Disable lsp eldoc to speed up, use C-c C-d to view doc
    ;; (when (lsp--capability :hoverProvider)
    ;;   (add-hook 'eldoc-documentation-functions #'ymacs-lsp//eldoc-function -10 t))

    (setq-local company-minimum-prefix-length 2))

  (define-advice lsp--calculate-root (:around (-fn -session -file-name) use-truename)
    (funcall -fn -session (file-truename -file-name)))

  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)

  (advice-add #'lsp-download-install :override #'lsp-download-install@pretty))

(after! lsp-modeline
  (setq lsp-modeline-code-actions-segments '(count name)))

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
      (ymacs-dap-running-session-mode 1))))
