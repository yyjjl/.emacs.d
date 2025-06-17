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

(after! treemacs
  (define-key!
    ("M-1" . ymacs-lsp/select-window-1)
    ("M-2" . ymacs-lsp/select-window-2)
    ("M-`" . treemacs))

  (setq treemacs-collapse-dirs (if treemacs-python-executable 3 0)
        treemacs-missing-project-action 'remove
        treemacs-follow-after-init t
        treemacs-no-png-images t)

  (ymacs-modeline//def-modeline treemacs (window-number major-mode))

  (setq treemacs-user-mode-line-format '(:eval (ymacs-modeline//format--treemacs)))

  ;; (treemacs-follow-mode 1)
  (treemacs-tag-follow-mode 1)
  (treemacs-filewatch-mode -1))


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

  (cl-defmethod eglot-register-capability :around
    (_server (method (eql workspace/didChangeWatchedFiles)) id &key watchers)
    (message "[eglot] debug: %s %s %s" method id watchers)
    (message "[eglot] debug: skip file-watcher")
    ;; (message "[eglot] file-watcher: %s" (cl-call-next-method))
    )

  ;; 一些 lsp server 会返回 :metadata 字段, 必须要 &allow-other-keys
  (cl-defmethod eglot-handle-request
    (_server (_method (eql workspace/applyEdit)) &key _label edit &allow-other-keys)
    "Handle server request workspace/applyEdit."
    (eglot--apply-workspace-edit edit last-command)
    `(:applied t))

  (defun ymacs-lsp//parse-diagnostics (-diagnostics)
    (cl-loop
     for diag-spec across -diagnostics
     collect (eglot--dbind ((Diagnostic) range code message severity source tags)
                 diag-spec
               (setq message (concat source (and code (format " [%s]" code)) ": " message))
               (pcase-let
                   ((`(,beg . ,end) (eglot-range-region range)))
                 (when (= beg end)
                   (eglot--widening
                    (goto-char (point-min))
                    (setq beg (eglot--bol (1+ (plist-get (plist-get range :start) :line))))
                    (setq end (line-end-position (1+ (plist-get (plist-get range :end) :line))))))
                 (eglot--make-diag
                  (current-buffer) beg end
                  (cond ((null severity) 'eglot-error)
                        ((<= severity 1) 'eglot-error)
                        ((= severity 2)  'eglot-warning)
                        (t          'eglot-note))
                  message `((eglot-lsp-diag . ,diag-spec))
                  (when-let ((faces
                              (cl-loop for tag across tags
                                       when (alist-get tag eglot--tag-faces)
                                       collect it)))
                    `((face . ,faces))))))))

  (cl-defmethod eglot-handle-notification :around
    (_server (_method (eql textDocument/publishDiagnostics)) &key uri diagnostics
             &allow-other-keys)
    (when-let* ((path (expand-file-name (eglot-uri-to-path uri)))
                (buffer (find-buffer-visiting path)))
      (with-current-buffer buffer
        ;; 这里只记录, 不触发解析, 解析 eglot-range-to-region 太耗时了
        (setq eglot--diagnostics diagnostics))))

  (define-advice eglot--report-to-flymake (:override (-diags) fast)
    (save-restriction
      (widen)
      (funcall eglot--current-flymake-report-fn
               (ymacs-lsp//parse-diagnostics -diags)
               :region (cons (point-min) (point-max)))))

  ;; 把错误打印出来方便 debug
  ;; (cl-defmethod eglot-handle-request :around (_server _method &rest _params)
  ;;   "Handle server request workspace/applyEdit."
  ;;   (condition-case err
  ;;       (cl-call-next-method)
  ;;     (error
  ;;      (message "[EGLOT] error %s" err))))

  (setq-default eglot-workspace-configuration #'ymacs-lsp//default-workspace-configuration))
