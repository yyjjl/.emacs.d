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
  (define-option! ymacs-python-lsp-server 'pyls
    (add-variable-watcher
     'ymacs-python-lsp-server
     (lambda (symbol new-value op _where)
       (let ((old-value (symbol-value symbol)))
         (when (and (eq op 'set)
                    (not (eq new-value old-value))
                    (memq new-value '(pyls pyright)))
           (require (intern (format "lsp-%s" new-value)) nil t)
           (setq lsp-disabled-clients (cl-delete new-value lsp-disabled-clients))
           (add-to-list 'lsp-disabled-clients old-value))))))

  (put 'ymacs-python-lsp-server 'safe-local-variable
       (lambda (x) (memq x '(pyls pyright))))

  (require-packages! lsp-pyright)

  (autoload #'ymacs-python/change-lsp-server "../lisp/python/commands.el" nil t)

  (ymacs-lsp//register-client 'pyls :package 'lsp-pyls)
  (ymacs-lsp//register-client 'pyright :package 'lsp-pyright))

(defvar-local ymacs-python--last-buffer nil
  "Help keep track of python buffer when changing to pyshell.")
