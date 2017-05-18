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
                fcitx))
;; theme
(add-packages '(color-theme
                all-the-icons all-the-icons-dired
                ;; helper control fringe(edge of a window) style
                fringe-helper
                ;; highlight braces with their depth
                rainbow-delimiters
                ;; spaceline
                rainbow-mode ;;  required by css-mode
                ;; make buffer even line and odd line having different color
                stripe-buffer
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
                dired+ dired-k
                buffer-move
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

(add-packages '(magit
                gitignore-mode
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
                py-isort))

(add-packages '(company-ghc
                company-ghci
                ghc
                haskell-mode
                shm hindent
                company-cabal))

(add-packages '(irony-eldoc irony
               company-irony company-irony-c-headers
               flycheck-irony
               rtags))

(add-packages '(company-shell
                sql-indent
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
                graphviz-dot-mode))

(package-utils-initialize)
