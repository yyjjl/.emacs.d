;;; -*- lexical-binding: t; -*-

(require-packages!
 py-autopep8
 py-isort
 pyvenv
 cython-mode
 flycheck-cython)

(executable! ipython3)
(executable! pylint)

(defvar ymacs-python-lsp-servers '(pyls pyright))

(eval-when-has-feature! lsp
  (require-packages! lsp-pyright)

  (defcustom ymacs-python-lsp-server 'pyls
    "Python LSP server type"
    :group 'ymacs
    :type 'symbol
    :safe #'(lambda (x) (memq x ymacs-python-lsp-servers)))

  (autoload #'ymacs-python/change-lsp-server (expand! "commands") nil t)

  (ymacs-lsp//register-client 'pyls :package 'lsp-pyls)
  (ymacs-lsp//register-client 'pyright :package 'lsp-pyright))

(defvar-local ymacs-python--last-buffer nil
  "Help keep track of python buffer when changing to pyshell.")