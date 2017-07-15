(require 'package-utils)

;; We include the org repository for completeness, but don't use it.
;; Lock org-mode temporarily:

;; (setq package-archives '(("melpa" . "http://melpa.org/packages/")
;;                          ("melpa-stable" . "http://stable.melpa.org/packages/")
;;                          ;; uncomment below line if you need use GNU ELPA
;;                          ("gnu" . "http://elpa.gnu.org/packages/")))
;; use mirror in china
(setq package-archives
      '(("melpa-stable" . "https://elpa.emacs-china.org/melpa-stable/")
        ("melpa" . "https://elpa.emacs-china.org/melpa/")
        ("gnu"   . "https://elpa.emacs-china.org/gnu/")))

;; core packages
(add-packages '(yasnippet
                auto-yasnippet
                company company-statistics
                session
                multi-term
                flycheck
                counsel hydra ivy swiper tiny
                counsel-projectile
                smex ;; counsel-M-x need smex to get history
                package-safe-delete
                bind-key
                ;; show key bindings while pressing
                which-key
                window-numbering
                htmlize
                ibuffer-vc
                easy-kill  f
                fcitx))
;; theme
(add-packages '(color-theme
                ;; helper control fringe(edge of a window) style
                fringe-helper
                hlinum
                ;; highlight braces with their depth
                rainbow-delimiters
                ;; spaceline
                rainbow-mode ;;  required by css-mode
                ;; ^L beautifier
                page-break-lines
                ;; header-line
                stickyfunc-enhance))
;; latex
(add-packages '(auctex
                company-auctex))

(add-packages '(popwin
                expand-region
                restclient company-restclient
                dired+
                buffer-move
                macrostep
                ;; clipbord tools
                simpleclip
                ;; quick switch window
                ace-window
                dropdown-list
                visual-regexp
                ;; find-by-pinyin-dired
                zzz-to-char
                zeal-at-point
                ;; provide tree style search jump
                avy
                wgrep))

(add-packages '(gitignore-mode
                gitconfig-mode
                git-messenger
                git-gutter-fringe git-gutter
                git-link
                git-timemachine))

(add-packages '( ;; auto compile after .el file load or save
                auto-compile
                ;; pair edit
                lispy
                racket-mode
                ;; slime
                ;; slime-company
                hl-sexp))

(add-packages '( ;; js company backend
                company-tern
                js-doc
                web-mode web-beautify
                company-web
                ;; optional package add support for angluar 1.x
                ac-html-angular
                ac-html-bootstrap
                js2-mode
                js-comint
                js2-refactor
                multiple-cursors ;; js2-refactor
                tern
                css-eldoc
                emmet-mode))

(add-packages '(elpy
                ob-ipython
                py-isort))

(add-packages '(company-ghc
                company-ghci
                ghc
                haskell-mode
                shm hindent
                company-cabal
                idris-mode))

(add-packages '(irony-eldoc
                irony
                company-irony company-irony-c-headers
                flycheck-irony clang-format
                ggtags
                rtags
                ivy-rtags))

(add-packages '(sql-indent
                ;; yaml format
                yaml-mode
                ;; haml format
                haml-mode
                markdown-mode
                crontab-mode
                csv-mode
                sass-mode
                less-css-mode
                scss-mode
                glsl-mode
                lua-mode
                go-mode
                groovy-mode
                cmake-mode cmake-font-lock cmake-ide
                php-mode
                gnuplot-mode
                csharp-mode
                graphviz-dot-mode))

(with-eval-after-load 'package-safe-delete
  (setq package-safe-delete-required-packages
        (hash-table-keys required-packages)))

(package-utils-initialize)
