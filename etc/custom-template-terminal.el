;;; -*- lexical-binding: t; -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fill-column 120)
 '(org-download-backend "curl \"%s\" -o \"%s\"")
 '(package-archives
   '(("melpa" . "http://melpa.org/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/")
     ("nongnu" . "http://elpa.nongnu.org/nongnu/")))
 '(safe-local-variable-values
   '((python-shell-interpreter . "python3")
     (python-shell-interpreter . "python")))
 '(shell-file-name "/bin/zsh")
 '(split-width-threshold 160)
 '(treemacs-width 60)
 '(user-full-name "Zet")
 '(warning-suppress-log-types '((comp)))
 '(which-key-dont-use-unicode t)
 '(ymacs-clangd-version "18.1.3")
 '(ymacs-lsp-enable-remote-server-p nil)
 '(ymacs-only-in-terminal-p t)
 '(ymacs-package-use-gnutls-p nil)
 '(ymacs-python-lsp-server 'pylance)
 '(ymacs-system-name "M2")
 '(ymacs-term-type 'vterm)
 '(ymacs-use-native-treesit-p nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-background-face ((t (:foreground "white"))))
 '(eglot-diagnostic-tag-unnecessary-face ((t (:inherit shadow :underline t))))
 '(eldoc-highlight-function-argument ((t (:foreground "#86DC2F" :weight bold))))
 '(flymake-error ((t (:inherit error :underline t))))
 '(flymake-note ((t (:foreground "green" :underline t))))
 '(flymake-warning ((t (:inherit warning :inverse-video t))))
 '(font-lock-builtin-face ((t (:foreground "#749E20"))))
 '(font-lock-doc-face ((t (:foreground "#777777" :weight bold))))
 '(hl-line ((t (:extend t :background "color-234"))))
 '(iedit-occurrence ((t (:foreground "#fb2874" :inverse-video t :weight bold))))
 '(lsp-lens-face ((t (:inherit lsp-details-face :underline t))))
 '(mode-line-highlight ((t (:foreground "#FD971F" :inverse-video t))))
 '(tab-bar ((t (:inherit variable-pitch :background "#1B1D1E" :foreground "white"))))
 '(tab-line ((t (:background "#1B1D1E" :foreground "white"))))
 '(tab-line-tab ((t (:inherit warning))))
 '(tab-line-tab-current ((t (:inherit tab-line-tab :inverse-video t))))
 '(tab-line-tab-inactive ((t (:inherit font-lock-comment-face :inverse-video t))))
 '(tooltip ((t (:background "#49483E"))))
 '(wgrep-face ((((type graphic)) (:box (:line-width (1 . 1) :color "grey75"))) (((type tty)) (:underline t))))
 '(whitespace-indentation ((t (:foreground "dim gray" :weight bold)))))



(load-feature! core/default)
(load-feature! core/popup)
(load-feature! core/editor)
(load-feature! core/term)
(load-feature! core/debug)

(load-feature! IDE/lsp)
;; (load-feature! IDE/tree-sitter)
;; (load-feature! IDE/childframe)

(load-feature! tools/git)
;; (load-feature! tools/spell)
(load-feature! tools/extra-modes)
;; (load-feature! tools/tabnine)

(load-feature! langs/lisp)
;; (load-feature! langs/latex)
;; (load-feature! langs/org)

(load-feature! langs/cpp)
(load-feature! langs/cpp-cmake)
(load-feature! langs/cpp-blade)

(load-feature! langs/python)

;; Programming modes
(load-feature! langs/sh)
;; (load-feature! langs/haskell)
;; (load-feature! langs/rust)
(load-feature! langs/java)
(load-feature! langs/web)
;; (load-feature! langs/go)
;; (load-feature! langs/perl)
;; (load-feature! langs/racket)
