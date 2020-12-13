;;; -*- lexical-binding: t; -*-

(require-packages!
 py-autopep8
 py-isort
 pyvenv
 cython-mode
 flycheck-cython)

(executable! ipython3)
(executable! pylint)

(eval-when-has-feature! lsp
  (define-option! ymacs-python-lsp-server 'pyright)

  (require-packages! lsp-pyright)

  (autoload #'ymacs-python/change-lsp-server "../lisp/python/commands.el" nil t)

  (ymacs-lsp//register-client 'pyls :package 'lsp-pyls)
  (ymacs-lsp//register-client 'pyright :package 'lsp-pyright))

(defvar-local ymacs-python--last-buffer nil
  "Help keep track of python buffer when changing to pyshell.")
