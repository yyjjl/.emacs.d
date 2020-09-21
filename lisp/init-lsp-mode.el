;; -*- lexical-binding:t -*-

(require-packages! lsp-mode lsp-ivy dap-mode)

(defcustom lsp-enable-in-project-p t
  "Whether to setup project literally"
  :group 'lsp
  :type 'directory
  :safe #'booleanp)

(ignore-errors
  (require 'lsp))

(cl-defmacro lsp//try-enable (name &key (enable t) (init nil) (fallback nil))
  (declare (indent 1))
  `(add-transient-hook! (hack-local-variables-hook :local t :name ,(intern (format "%s-internal" name)))
     (if (and ,enable
              lsp-enable-in-project-p
              (ignore-errors (lsp))
              (bound-and-true-p lsp-mode))
         ,init
       ,fallback)))

(defun lsp//set-simple-install-fn (client command)
  (setf (lsp--client-download-server-fn (ht-get lsp-clients client))
        (lambda (_client callback error-callback _update?)
          (condition-case err
              (run-command!
               :name (format "Install %s" client)
               :command command
               :callback (lambda (&rest _) (funcall callback))
               :error-callback (lambda (&rest _) (funcall error-callback "failed")))
            (error (funcall error-callback (error-message-string err)))))))

(config! lsp
  :bind
  (:map lsp-mode-map
   ("M-s l" . lsp-lens-mode)
   ("M-s h h" . lsp-document-highlight)
   ("M-s '" . lsp-avy-lens)
   ("M-s d" . dap-hydra)
   ("C-c R" . lsp-rename)
   ("C-c I" . lsp-ivy-workspace-symbol)
   ("C-c G" . lsp-ivy-global-workspace-symbol)
   ("C-c S" . lsp-describe-session)
   ("C-c B" . lsp-format-buffer)
   ("C-c C-d" . lsp-describe-thing-at-point)
   ("C-c C-SPC" . lsp-execute-code-action)
   ("C-S-SPC" . lsp-signature-activate))

  :hook
  (after-open
   :define (lsp-after-open-hook)
    ;; default to sort and filter by server
   (setq-local company-transformers nil))

  :advice
  (:override lsp-lv-message
   :define (message)
   (if message
       (progn
         (setq lsp--signature-last-buffer (current-buffer))
         (let ((lv-force-update t))
           (lv-message (replace-regexp-in-string "%" "%%" message))))
     (lv-delete-window)))

  (:around lsp--render-on-hover-content
   :define (-fn -contents -render-all)
   (let ((content (funcall -fn -contents -render-all)))
     (unless (core-popups//help-buffer-matcher (current-buffer))
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
     content))

  :toggles
  (lsp-mode
   "LSP"
   (("t i" lsp-toggle-trace-io
     "trace io" :toggle lsp-print-io)
    ("t h" lsp-toggle-symbol-highlight
     "highlight" :toggle lsp-enable-symbol-highlighting)
    ("t t" lsp-toggle-on-type-formatting
     "on type formating" :toggle lsp-enable-on-type-formatting)
    ("t s" lsp-toggle-signature-auto-activate
     "signature" :toggle lsp-signature-auto-activate)

    ("w r" lsp-restart-workspace)
    ("w s" lsp-shutdown-workspace)))

  :init
  (setq lsp-keymap-prefix nil)

  :config
  (setq lsp-auto-configure t)
  (setq lsp-auto-guess-root t)
  (setq lsp-eldoc-render-all t)
  (setq lsp-diagnostics-provider :auto)
  (setq lsp-restart 'auto-restart)
  (setq lsp-session-file (expand-var! "lsp-sessions"))
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-signature-doc-lines 2)
  (setq lsp-idle-delay 0.5)

  (setq lsp-enable-snippet t)
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-enable-text-document-color nil)
  (setq lsp-enable-indentation nil)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-enable-semantic-highlighting nil)

  (setq lsp-signature-auto-activate t)
  (setq lsp-signature-doc-lines 2)

  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-modeline-code-actions-enable t))

(config! lsp-headerline
  :config

  (defun lsp*override-lsp-headerline--filename-with-icon (-file-path)
    (f-filename -file-path))

  (lsp-defun lsp*override-lsp-headerline--symbol-icon ((&DocumentSymbol :kind))
    "Build the SYMBOL icon for headerline breadcrumb."
    (when (require 'lsp-treemacs nil t)
      (format "[%s] " (lsp-treemacs-symbol-kind->icon kind))))

  (advice-add #'lsp-headerline--symbol-icon :override
              #'lsp*override-lsp-headerline--symbol-icon)
  (advice-add #'lsp-headerline--filename-with-icon :override
              #'lsp*override-lsp-headerline--filename-with-icon)

  (setq lsp-headerline-breadcrumb-segments '(project symbols)))

(config! dap-mode
  :hook
  (load-debugger
   :define (python-mode-hook)
   (require 'dap-python))

  :hook
  ((show-dap-hydra _)
   :define (dap-session-created-hook dap-stopped-hook)
   (dap-hydra))

  ((hide-dap-hydra _)
   :define (dap-terminated-hook)
   (dap-hydra/nil))

  :config
  (setq dap-auto-configure-features
        '(sessions locals breakpoints expressions controls))

  (config! dap-python
    :config
    (setq dap-python-executable "python3")))

(config! dap-ui
  :bind
  (:map dap-ui-repl-mode-map
   ("M-s d" . dap-hydra))

  :config
  (config! winum
    :config
    (dolist (buffer (list dap-ui--locals-buffer
                          dap-ui--sessions-buffer
                          dap-ui--debug-window-buffer
                          dap-ui--expressions-buffer
                          dap-ui--breakpoints-buffer))
      (add-to-list 'winum-ignored-buffers buffer))))

(config! dap-hydra
  :config

  (defhydra++ dap-hydra ()
    ("q" :delete)
    ("ed" dap-ui-expressions-remove "Remove expr")
    ("el" dap-ui-expressions "List expr")
    ("so" dap-go-to-output-buffer "Output")
    ("sL" dap/go-to-log-buffer "Log")
    ("C-c C-z" dap-ui-repl "Repl")
    ("M-s q" nil "quit" :exit t)))

(config! lsp-cmake
  :config
  (lsp//set-simple-install-fn
   'cmakels
   "pip3 install --user cmake-language-server"))

(config! lsp-python
  :config
  (lsp//set-simple-install-fn
   'pyls
   "pip3 install --user 'python-language-server[all]"))

(config! lsp-go
  :config
  (lsp//set-simple-install-fn
   'gopls
   "GO111MODULE=on go get golang.org/x/tools/gopls@latest && go get -u github.com/fatih/gomodifytags"))

(config! ccls
  :config
  (lsp//set-simple-install-fn
   'ccls
   (expand-etc! "setup/install_ccls.sh")))

(config! lsp-rust
  :config
  (let ((url "https://github.com/rust-analyzer/rust-analyzer/releases/latest/download/rust-analyzer-linux")
        (path (expand-var! "rust-analyzer")))
    (setq lsp-rust-analyzer-server-command (list path))

    (lsp//set-simple-install-fn
     'rust-analyzer
     (format "curl -L %s -o %s && chmod +x %s" url path path))))

(provide 'init-lsp-mode)
