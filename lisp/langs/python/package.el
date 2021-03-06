;;; -*- lexical-binding: t; -*-

(option! python-auto-activate-venv nil
  :type 'boolean
  :safe #'booleanp)

(require-packages!
 py-autopep8
 py-isort
 pyvenv
 cython-mode)

(executable! ipython3)
(executable! pylint)

(defvar ymacs-python-lsp-servers '(pyls pyright))

(defvar-local ymacs-python--last-buffer nil
  "Help keep track of python buffer when changing to pyshell.")

(eval-when-has-feature! debug
  (add-to-list 'ymacs-debugger-alist '(python-mode pdb :gud t)))

(eval-when-has-feature! lsp
  (require-packages! lsp-pyright)

  (option! python-lsp-server 'pyright
    "Python LSP server type"
    :type 'symbol)

  (autoload #'ymacs-python/change-lsp-server (expand! "commands") nil t)

  (ymacs-lsp//register-client
   'pyls
   :package 'lsp-pyls
   :enable-fn (lambda () (eq ymacs-python-lsp-server 'pyls)))
  (ymacs-lsp//register-client
   'pyright
   :package 'lsp-pyright
   :enable-fn (lambda () (eq ymacs-python-lsp-server 'pyright))))
