;; -*- lexical-binding:t -*-

(setq lsp-pyright-multi-root nil)

(setq lsp-keymap-prefix "C-c ;")
(after! lsp-mode
  ;; (lsp-dired-mode 1)

  ;; (when (require 'lsp-treemacs nil t)
  ;;   (lsp-treemacs-sync-mode 1))

  (add-to-list 'lsp-file-watch-ignored "[/\\\\]\\.cache\\'")
  (add-to-list 'lsp-file-watch-ignored
               (eval-when-compile
                 (rx-to-string `(: string-start ,(expand-file-name "~/.local/lib") string-end))))
  (add-to-list 'lsp-file-watch-ignored
               (eval-when-compile
                 (rx-to-string `(: string-start ,(file-truename user-emacs-directory) string-end))))

  (add-to-list
   'mode-line-misc-info
   '(lsp-signature-mode
     (:propertize "[Signature]" face ymacs-modeline-lsp-success)))

  (define-key! :map lsp-command-map
    (";" . lsp-avy-lens)
    ("i" . ymacs-lsp/remove-invalid-folders)
    ("R" . lsp-workspace-folders-remove)
    ("b" . lsp-workspace-blacklist-remove)
    ("o" . lsp-workspace-folders-open)
    ("h" . lsp-document-highlight)
    ("l" . lsp-lens-mode))

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

  (define-key! :map lsp-mode-map
    ("M-s r" . lsp-find-references)
    ("M-s t" . lsp-find-type-definition)
    ("M-s i" . lsp-find-implementation)
    ("M-s d" . lsp-find-declaration)

    ("M-s h h" . lsp-document-highlight)
    ("C-c R" . lsp-rename)
    ("C-c d" . consult-lsp-diagnostics)
    ("C-c I" . consult-lsp-symbols)
    ("C-c E" . ymacs-lsp/open-remote-stderr)
    ("C-c S" . lsp-describe-session)
    ("C-c b" . ymacs-lsp/organize-imports)
    ("C-c C-b" . ymacs-lsp/format-buffer)
    ("C-c C-d" . lsp-describe-thing-at-point)
    ("C-c C-SPC" . lsp-execute-code-action)
    ("M-o" . lsp-signature-activate))

  (define-key! :map lsp-signature-mode-map
    ("M-o" . lsp-signature-stop))

  (setq lsp-signature-doc-lines 5)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-keep-workspace-alive nil)
  (setq lsp-restart 'interactive)

  ;; manually configured
  (setq lsp-completion-provider :none)
  ;; boost performance
  (setq lsp-enable-file-watchers nil)

  ;; (setq lsp-eldoc-render-all nil)
  (setq lsp-display-inline-image nil)

  (setq lsp-imenu-index-function #'lsp-imenu-create-categorized-index)
  ;; (setq lsp-enable-imenu t)
  ;; (setq lsp-enable-links t)
  ;; (setq lsp-enable-xref t)
  ;; (setq lsp-enable-folding t)
  ;; (setq lsp-enable-snippet t)
  ;; boost performance
  (setq lsp-enable-on-type-formatting nil)
  ;; (setq lsp-enable-text-document-color t)
  ;; range formating
  ;; (setq lsp-enable-indentation t)
  (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-symbol-highlighting-skip-current t)

  (setq lsp-before-save-edits nil)

  ;; (setq lsp-semantic-tokens-enable nil)
  (setq lsp-lens-enable nil)

  (setq lsp-progress-prefix "LSP:")

  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-headerline-breadcrumb-icons-enable nil)
  (setq lsp-headerline-breadcrumb-segments '(symbols))
  (setq lsp-headerline-breadcrumb-enable-diagnostics nil)

  ;; (setq lsp-modeline-code-actions-enable t)
  (setq lsp-modeline-code-actions-segments '(name count))

  (setq lsp-modeline-diagnostics-enable nil)
  (setq lsp-modeline-workspace-status-enable nil))

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

(after! lsp-treemacs
  (define-key!
    ("C-x ' '" . lsp-treemacs-errors-list)
    ("C-x ' s" . lsp-treemacs-symbols)
    ("C-x ' r" . lsp-treemacs-references)
    ("C-x ' i" . lsp-treemacs-implementations)
    ("C-x ' c" . lsp-treemacs-call-hierarchy)))

(after! lsp-clangd
  (ymacs-lsp//use-common-download-script (ht-get lsp-clients 'clangd)))

(after! lsp-tex
  (lsp-register-custom-settings
   `(("latex.rootDirectory"
      (lambda ()
        (or (when (stringp TeX-master)
              (file-name-directory TeX-master))
            ".")))
     ("latex.lint.onSave" nil t)))

  (let ((client (ht-get lsp-clients 'texlab)))
    (setf (lsp--client-notification-handlers client)
          (ht ("textDocument/publishDiagnostics" #'ignore)))

    (ymacs-lsp//use-common-download-script client)))
