;; -*- lexical-binding:t -*-

(option! lsp-project-state :enabled
  "Whether to enable lsp in current project"
  :type '(choice
          (const :tag "Enable LSP in current file" :enabled)
          (const :tag "Disable LSP in current file" :disabled))
  :safe #'(lambda (x) (memq x '(:enabled :disabled))))

(executable! lsp-booster :exe (expand-cache! "lsp/emacs-lsp-booster/emacs-lsp-booster"))

(option! python-lsp-server 'pyright
  "Python LSP server type"
  :type 'symbol)

(option! clangd-version "18.1.3"
  "Clangd version."
  :type 'string)

(option! clangd-args '("--background-index" "-j=4" "--all-scopes-completion" "--header-insertion-decorators=0")
  "Clangd command line args"
  :type '(list string)
  :safe (lambda (x) (and (listp x) (cl-every #'stringp x))))

(defvar ymacs-python-lsp-servers '(pyls pyright pylance))

(require-packages!
 eglot ; 需要新版本 > 1.17
 consult-eglot)

(when ymacs-lsp-booster-path
  (when (and (not (package-installed-p 'eglot-booster))
             (fboundp #'package-vc-install))
    (package-vc-install "https://github.com/jdtsmith/eglot-booster")))

(defvar-local ymacs-lsp-format-buffer-function #'eglot-format-buffer)
(defvar-local ymacs-lsp-format-region-function #'eglot-format)
(defvar-local ymacs-lsp-organize-import-function #'eglot-code-action-organize-imports)
(defvar-local ymacs-lsp-find-other-file-function nil)
