;; -*- lexical-binding:t -*-

(defun ymacs-lsp/find-other-file ()
  (interactive)
  (unless ymacs-lsp-find-other-file-function
    (user-error "ymacs-lsp-find-other-file-function is not set"))
  (call-interactively ymacs-lsp-find-other-file-function))

(defun ymacs-lsp/format-buffer ()
  (interactive "*")
  (call-interactively ymacs-lsp-format-buffer-function))

(defun ymacs-lsp/organize-imports ()
  (interactive "*")
  (call-interactively ymacs-lsp-organize-import-function))

(after! project
  (when (boundp 'project-prefix-map)
    (define-key! :map project-prefix-map
      ("a" . ymacs-lsp/find-other-file))))


(after! eglot
  (define-key! :map eglot-mode-map
    ("C-c C-d" . eldoc)
    ("C-c R" . eglot-rename)
    ("C-c I" . consult-eglot-symbols)
    ("C-c b" . ymacs-lsp/organize-imports)
    ("C-c C-b" . ymacs-lsp/format-buffer)
    ("C-c C-SPC" . eglot-code-actions)
    ("C-c f x" . eglot-shutdown)
    ("C-c f s" . eglot-stderr-buffer)
    ("C-c f e" . eglot-events-buffer)
    ("C-c f r" . eglot-reconnect)
    ("C-c S" . eglot-show-workspace-configuration)

    ("M-o" . ymacs-lsp/signature-start)
    ("M-s t" . eglot-find-typeDefinition)
    ("M-s i" . eglot-find-implementation)
    ("M-s d" . eglot-find-declaration))

  ;; (add-to-list 'ymacs-editor-view-code-modes '(eglot--managed-mode eglot-inlay-hints-mode))

  (setf (alist-get 'eglot-capf completion-category-overrides)
        '((styles basic orderless)))

  (setq eglot-confirm-server-edits '((t . maybe-summary)))
  (setq eglot-extend-to-xref t)
  (setq eglot-autoshutdown nil)
  (setq eglot-events-buffer-config '(:size 10 :format full))

  (setq eglot-ignored-server-capabilities
        '(:documentOnTypeFormattingProvider
          ;; :documentHighlightProvider
          :documentLinkProvider
          :semanticTokensProvider))
  ;; Keep dynamic file watching inside the project and let Eglot enforce a
  ;; finite watcher budget.  External edits made by LLM tools must still reach
  ;; the language server.
  (setq eglot-watch-files-outside-project-root nil)
  (setq eglot-max-file-watches 10000)

  (define-advice eglot--confirm-server-edits (:around (fn &rest args))
    (if current-prefix-arg
        'diff
      (apply fn args)))

  (define-advice eglot--sig-info (:around (fn sig &optional sig-active briefp) fix-pyright)
    (when (derived-mode-p 'python-base-mode)
      (setq sig-active nil))
    (funcall fn sig sig-active briefp))

  (add-to-list 'mode-line-misc-info '(ymacs-lsp-signature-mode (:propertize "[signature]" face success)))
  (add-to-list 'mode-line-misc-info '(eglot-inlay-hints-mode (:propertize "[inlay]" face eglot-inlay-hint-face)))

  (setf (cdr (ymacs-lsp//eglot-lookup-mode 'sh-mode))
        (list (expand-cache! "lsp/npm/bash-language-server/bin/bash-language-server") "start"))

  (setf (cdr (ymacs-lsp//eglot-lookup-mode 'js-mode))
        (list (expand-cache! "lsp/npm/typescript-language-server/bin/typescript-language-server") "--stdio"))

  (setf (cdr (ymacs-lsp//eglot-lookup-mode 'c-mode))
        (lambda (&optional _interactive _project)
          (let ((clangd-binary (expand-cache! (format "lsp/clangd/clangd_%s/bin/clangd" ymacs-clangd-version))))
            (when (file-executable-p clangd-binary)
              (cons clangd-binary ymacs-clangd-args)))))

  ;; Replace the old watcher-suppressing method with a transparent method.
  ;; Keeping the same specialization also fixes already-running sessions when
  ;; this file is re-evaluated.
  (cl-defmethod eglot-register-capability :around
    (server (method (eql workspace/didChangeWatchedFiles)) id
            &key watchers &allow-other-keys)
    (cl-call-next-method server method id :watchers watchers))

  ;; Some servers add non-standard fields such as :metadata.  Filter those
  ;; fields in an around method and delegate the actual response, including
  ;; applied/failureReason, to Eglot's implementation.
  (cl-defmethod eglot-handle-request :around
    (server (method (eql workspace/applyEdit)) &rest params)
    (cl-call-next-method server method
                         :label (plist-get params :label)
                         :edit (plist-get params :edit)))

  ;; 把错误打印出来方便 debug
  ;; (cl-defmethod eglot-handle-request :around (_server _method &rest _params)
  ;;   "Handle server request workspace/applyEdit."
  ;;   (condition-case err
  ;;       (cl-call-next-method)
  ;;     (error
  ;;      (message "[EGLOT] error %s" err))))

  (setq-default eglot-workspace-configuration #'ymacs-lsp//default-workspace-configuration))
