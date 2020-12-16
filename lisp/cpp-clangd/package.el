;; -*- lexical-binding:t -*-

(eval-when-compile
  (unless (and (has-feature! 'cpp)
               (has-feature! 'lsp))
    (user-error "cpp-clangd should be loaded after feature cpp and lsp")))

(executable! clangd :exe [(expand-cache! "lsp/clangd/bin/clangd") "clangd"])

(ymacs-lsp//register-client
 'clangd
 :package 'lsp-clangd
 :manual `(:title "Clangd"
           :repo "clangd/clangd"
           :exe ,ymacs-clangd-path))

(add-to-list 'ymacs-cpp-buffer-command-functions
             #'ymacs-cpp-clangd//buffer-compile-command)
(add-to-list 'ymacs-cpp-lsp-checkers
             (lambda () (file-exists-p (ymacs-cpp-clangd//dot-clangd-path))))
