;; -*- lexical-binding:t -*-

(executable! clangd :exe [(expand-var! "clangd/bin/clangd") "clangd"])

(after-feature! cpp
  (add-to-list 'ymacs-cpp-buffer-command-functions
               #'ymacs-cpp-clangd//buffer-compile-command)
  (add-to-list 'ymacs-cpp-lsp-checkers
               (lambda () (file-exists-p (ymacs-cpp-clangd//dot-clangd-path))))

  (setq ymacs-cpp-expand-macro-function
        (lambda () (ymacs-cpp-clangd//buffer-compile-command t))))
