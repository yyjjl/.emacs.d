;;; -*- lexical-binding: t; -*-

(define-option! ymacs-python-lsp-server 'pyright)

(require-packages!
 lsp-mode
 lsp-pyright
 py-autopep8
 py-isort
 pyvenv
 cython-mode
 flycheck-cython)

(executable! ipython3)
(executable! pylint)

(defvar-local ymacs-python--last-buffer nil
  "Help keep track of python buffer when changing to pyshell.")
