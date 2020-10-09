;; -*- lexical-binding:t -*-

(require-packages! lsp-mode ccls)

(defface cpp-variable-face
  `((t :foreground ,(face-attribute 'default :foreground)))
  "Face for variables"
  :group 'ccls-sem)

(defconst ymacs-cpp-ccls--default-template
  "clang \n%c -std=gnu11\n%cpp -std=c++14\n-Wall\n")

(defvar ymacs-cpp-ccls-jump-map
  (define-key! :map (make-sparse-keymap)
    ("b" . ymacs-cpp-ccls/xref-find-bases)
    ("d" . ymacs-cpp-ccls/xref-find-derived)
    ("c" . ymacs-cpp-ccls/xref-find-callers)
    ("e" . ymacs-cpp-ccls/xref-find-callee)
    ("m" . ymacs-cpp-ccls/xref-find-members)
    ("M" . ymacs-cpp-ccls/xref-find-references-macro)
    ("w" . ymacs-cpp-ccls/xref-find-references-write)
    ("r" . ymacs-cpp-ccls/xref-find-references-read)
    ("a" . ymacs-cpp-ccls/xref-find-references-address)
    ("R" . ccls-reload)))

(after-feature! cpp
  (add-to-list 'ymacs-cpp-buffer-command-functions
               #'ymacs-cpp-ccls//buffer-compile-command)
  (add-to-list 'ymacs-cpp-lsp-checkers
               (lambda () (file-exists-p (ymacs-cpp-ccls//dot-ccls-path))))

  (setq ymacs-cpp-expand-macro-function
        (lambda () (ymacs-cpp-ccls//buffer-compile-command t))))
