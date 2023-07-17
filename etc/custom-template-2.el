;;; -*- lexical-binding: t; -*-

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fill-column 100)
 '(mouse-wheel-progressive-speed nil)
 '(native-comp-async-jobs-number 2)
 '(org-download-backend "curl \"%s\" -o \"%s\"")
 '(package-archives
   '(("melpa" . "http://melpa.org/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/")
     ("nongnu" . "http://elpa.nongnu.org/nongnu/")))
 '(safe-local-variable-values
   '((python-shell-interpreter . "python3")
     (python-shell-interpreter . "python")))
 '(user-full-name "Zet")
 '(warning-suppress-log-types '((comp)))
 '(which-key-dont-use-unicode t)
 '(ymacs-python-lsp-server 'pylance)
 '(ymacs-term-path-alist
   '(("byteide" . "/bin/bash")
     ("ts" . "/bin/bash")
     ("m2" . "/usr/bin/zsh")
     ("m1" . "/bin/bash")))
 '(ymacs-use-native-treesit-p nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "#1B1D1E" :foreground "#F8F8F2" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight regular :height 130 :width normal :foundry "nil" :family "Monaco"))))
 '(eldoc-highlight-function-argument ((t (:foreground "#86DC2F" :weight bold))))
 '(flymake-error ((t (:inherit error :underline t))))
 '(flymake-note ((t (:foreground "green" :underline t))))
 '(flymake-warning ((t (:inherit warning :inverse-video t))))
 '(font-lock-builtin-face ((t (:foreground "#749E20"))))
 '(font-lock-doc-face ((t (:foreground "#777777" :weight bold))))
 '(hl-line ((t (:background "grey5"))))
 '(iedit-occurrence ((t (:foreground "#fb2874" :inverse-video t :weight bold))))
 '(ivy-modified-buffer ((t (:inherit ymacs-modeline-buffer-modified))))
 '(ivy-modified-outside-buffer ((t (:inherit ymacs-modeline-urgent))))
 '(mode-line-highlight ((t (:foreground "#FD971F" :inverse-video t))))
 '(tab-bar ((t (:background "#1B1D1E" :foreground "white"))))
 '(tab-bar-tab ((t (:inherit warning))))
 '(tab-bar-tab-inactive ((t (:inherit font-lock-comment-face :inverse-video t))))
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
(load-feature! IDE/childframe)

(load-feature! tools/git)
(load-feature! tools/extra-modes)

(load-feature! langs/lisp)
(load-feature! langs/latex)
(load-feature! langs/org)

(load-feature! langs/cpp)
(load-feature! langs/cpp-cmake)
(load-feature! langs/cpp-blade)

(load-feature! langs/python)

;; Programming modes
(load-feature! langs/sh)
;; (load-feature! langs/haskell)
;; (load-feature! langs/rust)
(load-feature! langs/java)
;; (load-feature! langs/web)
;; (load-feature! langs/go)
(load-feature! langs/perl)
;; (load-feature! langs/racket)
