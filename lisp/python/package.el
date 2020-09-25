;;; -*- lexical-binding: t; -*-

(require-packages!
 lsp-mode
 elpy
 py-autopep8
 py-isort
 cython-mode
 flycheck-cython)

(executable! ipython3)
(executable! pylint)
(executable! pyls)

(put 'elpy-shell-use-project-root 'safe-local-variable #'booleanp)
