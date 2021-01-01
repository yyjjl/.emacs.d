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

(defun ymacs-python//set-lsp-server (-server)
  (cl-assert (memq -server ymacs-python-lsp-servers)
             "Not in %s"
             ymacs-python-lsp-servers)

  (require 'lsp-mode nil t)
  (let ((old-server ymacs-python-lsp-server))
    (unless (eq old-server -server)
      (add-to-list 'lsp-client-packages (intern (format "lsp-%s" -server)))

      (setq ymacs-python-lsp-server -server)
      (setq lsp-disabled-clients (cl-delete -server lsp-disabled-clients))
      (dolist (server ymacs-python-lsp-servers)
        (unless (eq server -server)
          (add-to-list 'lsp-disabled-clients server))))))

(eval-when-has-feature! lsp
  (require-packages! lsp-pyright)

  (define-option! ymacs-python-lsp-server 'pyls
    (after! lsp-mode
      (ymacs-python//set-lsp-server -the-value)))

  (put 'ymacs-python-lsp-server 'safe-local-variable
       (lambda (x) (memq x ymacs-python-lsp-servers)))

  (autoload #'ymacs-python/change-lsp-server "../lisp/python/commands.el" nil t)

  (after! lsp-mode
  (ymacs-lsp//register-client 'pyls :package 'lsp-pyls)
  (ymacs-lsp//register-client 'pyright :package 'lsp-pyright)))

(defvar-local ymacs-python--last-buffer nil
  "Help keep track of python buffer when changing to pyshell.")
