;;; -*- lexical-binding: t; -*-

(option! python-auto-activate-venv t
  :type 'boolean
  :safe #'booleanp)

(require-packages!
 py-autopep8
 py-isort
 cython-mode)

(executable! ipython3)
(executable! pylint)

(defvar-local ymacs-python--last-buffer nil
  "Help keep track of python buffer when changing to pyshell.")

(defvar-local ymacs-python-execution-root 'unset)

(after-feature! editor
  (add-to-list 'ymacs-editor-project-invalidate-cache-empty-vars
               '(ymacs-python-execution-root . unset)))

(eval-when-has-feature! debug
  (add-to-list 'ymacs-debugger-alist '(python-mode pdb :gud t)))
