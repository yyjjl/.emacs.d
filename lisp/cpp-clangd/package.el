;; -*- lexical-binding:t -*-

(executable! clangd :exe [(expand-cache! "lsp/clangd/bin/clangd") "clangd"])

(ymacs-lsp//register-client
 'clangd
 :package 'lsp-clangd
 :manual `(:title "Clangd"
           :repo "clangd/clangd"
           :exe ,ymacs-clangd-path))

(after-feature! cpp
  (add-to-list 'ymacs-cpp-buffer-command-functions
               #'ymacs-cpp-clangd//buffer-compile-command)
  (add-to-list 'ymacs-cpp-lsp-checkers
               (lambda () (file-exists-p (ymacs-cpp-clangd//dot-clangd-path))))

  (setq ymacs-cpp-expand-macro-function
        (lambda () (ymacs-cpp-clangd//buffer-compile-command t))))
