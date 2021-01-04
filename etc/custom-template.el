;;; -*- lexical-binding: t; -*-

;; (setq ymacs-use-gnutls-p nil)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-keymap-prefix (kbd "C-c f"))
 '(org-download-backend "curl \"%s\" -o \"%s\"")
 '(org-download-screenshot-method "gnome-screenshot -a -f %s")
 '(warning-suppress-log-types '((comp)))
 '(which-key-dont-use-unicode t)
 '(ymacs-term-type 'vterm)
 '(ymacs-python-lsp-server 'pyright))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(diff-hl-change ((t (:foreground "#e2c770" :background nil))))
 '(diff-hl-delete ((t (:background nil))))
 '(diff-hl-insert ((t (:background nil))))
 '(diredp-dir-name ((t (:inherit font-lock-type-face :weight bold))))
 '(eldoc-highlight-function-argument ((t (:foreground "#86DC2F" :weight bold))))
 '(flycheck-error ((t (:underline (:style wave :color "#e74c3c")))))
 '(flycheck-info ((t (:underline (:style wave :color "#b6e63e")))))
 '(flycheck-warning ((t (:underline (:style wave :color "#e2c770")))))
 '(font-lock-builtin-face ((t (:foreground "#749e20"))))
 '(font-lock-constant-face ((t (:foreground "#AE81FF"))))
 '(font-lock-doc-face ((t (:foreground "#777777" :weight bold))))
 '(font-lock-warning-face ((t (:background "black" :weight bold :underline t))))
 '(hl-line ((t (:background "grey8"))))
 '(ivy-current-match ((t (:background "grey0"))))
 '(org-document-info-keyword ((t :inherit font-lock-builtin-face)))
 '(org-latex-and-related ((t (:foreground "orange red" :weight bold))))
 '(org-meta-line ((t :inherit font-lock-doc-face)))
 '(tab-line ((t (:background "#1b1d1e" :foreground "white"))))
 '(tab-line-tab ((t (:inherit warning))))
 '(tab-line-tab-current ((t (:inherit tab-line-tab :inverse-video t))))
 '(tab-line-tab-inactive ((t (:inherit font-lock-comment-face :inverse-video t)))))



(load-core!)

(load-feature! IDE/lsp)
;; (load-feature! IDE/dap)
;; (load-feature! IDE/tree-sitter)

(load-feature! tools/git)
(load-feature! tools/spell)

(load-feature! langs/lisp)
(load-feature! langs/latex)
(load-feature! langs/org)

(load-feature! langs/cpp)
(load-feature! langs/cpp-cmake)

(load-feature! langs/python)

;; Programming modes
(load-feature! langs/sh)
(load-feature! langs/haskell)
(load-feature! langs/rust)
;; (load-feature! langs/java)
(load-feature! langs/web)
;; (load-feature! langs/go)
(load-feature! langs/perl)
(load-feature! langs/racket)
