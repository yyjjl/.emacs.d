;;; -*- lexical-binding: t; -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fill-column 100)
 '(org-download-backend "curl \"%s\" -o \"%s\"")
 '(org-download-screenshot-method "gnome-screenshot -a -f %s")
 '(warning-suppress-log-types '((comp)))
 '(which-key-dont-use-unicode t)
 '(user-full-name "Zet"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eldoc-highlight-function-argument ((t (:foreground "#86DC2F" :weight bold))))
 '(font-lock-builtin-face ((t (:foreground "#749E20"))))
 '(font-lock-doc-face ((t (:foreground "#777777" :weight bold))))
 '(hl-line ((t (:background "grey5"))))
 '(iedit-occurrence ((t (:foreground "#fb2874" :inverse-video t :weight bold))))
 '(ivy-modified-buffer ((t (:inherit ymacs-modeline-buffer-modified))))
 '(ivy-modified-outside-buffer ((t (:inherit ymacs-modeline-urgent))))
 '(tab-line ((t (:background "#1B1D1E" :foreground "white"))))
 '(tab-line-tab ((t (:inherit warning))))
 '(tab-line-tab-current ((t (:inherit tab-line-tab :inverse-video t))))
 '(tab-line-tab-inactive ((t (:inherit font-lock-comment-face :inverse-video t))))
 '(tooltip ((t (:background "#49483E"))))
 '(whitespace-indentation ((t (:foreground "dim gray" :weight bold)))))



(load-feature! core/default)
(load-feature! core/popup)
(load-feature! core/editor)
(load-feature! core/term)
(load-feature! core/debug)

;; (load-feature! IDE/lsp)
;; (load-feature! IDE/tree-sitter)

(load-feature! tools/git)
;; (load-feature! tools/spell)
(load-feature! tools/extra-modes)
;; (load-feature! tools/tabnine)

(load-feature! langs/lisp)
(load-feature! langs/latex)
(load-feature! langs/org)

(load-feature! langs/cpp)
(load-feature! langs/cpp-cmake)

(load-feature! langs/python)

;; Programming modes
(load-feature! langs/sh)
;; (load-feature! langs/haskell)
;; (load-feature! langs/rust)
;; (load-feature! langs/java)
;; (load-feature! langs/web)
;; (load-feature! langs/go)
(load-feature! langs/perl)
;; (load-feature! langs/racket)
