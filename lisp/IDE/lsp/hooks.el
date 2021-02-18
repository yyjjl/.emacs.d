;; -*- lexical-binding:t -*-

(declare-function lsp--render-element 'lsp-mode)

(after! lsp-mode
  (define-hook! ymacs-lsp|after-open (lsp-after-open-hook)
    (when (bound-and-true-p lsp-ui-mode)
      (eldoc-mode -1))

    (setq-local company-minimum-prefix-length 1)
    ;; default to sort and filter by server
    (setq-local company-transformers nil))

  (cl-defmethod lsp-clients-extract-signature-on-hover
    :around (_contents _server-id)
    "Cut signature into a single line"
    (let ((content (string-trim-left (or (cl-call-next-method) ""))))
      (unless (or (equal (buffer-name) "*lsp-help*")
                  (string-prefix-p "!" content))
        (let* ((content-length (length content))
               (max-width (frame-width)))
          (when (>= content-length (- max-width 10))
            (let ((suffix (propertize
                           (format " ... (%s to see more)"
                                   (substitute-command-keys
                                    "\\[lsp-describe-thing-at-point]"))
                           'face 'font-lock-comment-face)))
              (setq content
                    (--> content
                      (substring it 0 (- max-width 30))
                      (replace-regexp-in-string "\n+" "|" it t t)
                      (concat it suffix)))
              (remove-text-properties 0 (length content) '(display) content)))))
      content))

  (cl-defmethod lsp-clients-extract-signature-on-hover (-contents _server-id)
    "Extract a representative line from CONTENTS, to show in the echo area."
    (lsp--render-element -contents))

  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)

  (advice-add #'lsp-download-install :override #'lsp-download-install@pretty)
  (advice-add #'lsp-async-start-process :override #'lsp-async-start-process@pretty))

(after! lsp-modeline
  (setq lsp-modeline-code-actions-segments '(count name)))

(eval-when! ymacs-lsp-use-dap
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
