;; -*- lexical-binding:t -*-

(require-packages! google-c-style)

(cl-defstruct ymacs-cpp-build-system
  (system-id nil)

  (lsp-enable-fn nil)
  (lsp-enable-handler nil)
  (lsp-disable-handler nil)

  (directory-fn nil)
  (command-fn nil))

(defvar-local ymacs-cpp-current-build-system nil)
(defvar ymacs-cpp-build-systems
  (list
   (make-ymacs-cpp-build-system
    :system-id 'default
    :lsp-enable-fn #'ymacs-cpp//get-dot-clangd-path
    :command-fn #'ymacs-cpp//get-compile-command-from-dot-clangd)))

(eval-when-has-feature! term
  ;; set term default directory
  (add-to-list 'ymacs-term-directory-functions #'ymacs-cpp//build-dir)
  (dolist (mode '(c-mode c++-mode c-ts-mode c++-ts-mode))
    (add-to-list 'ymacs-term-repl-alist
                 `(,mode :program "root" :program-args ("-l" the-file)))))

(eval-when-has-feature! debug
  (dolist (mode '(c-mode c++-mode c-ts-mode c++-ts-mode))
    (add-to-list 'ymacs-debugger-alist `(,mode gdb :gud t))))

(eval-when-has-feature! lsp
  (ymacs-lsp//register-client 'clangd :package 'lsp-clangd))
