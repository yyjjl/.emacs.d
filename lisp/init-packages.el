(require 'package-utils)

;; add site-package's path to `load-path'
(if (fboundp 'normal-top-level-add-to-load-path)
    (dolist (dir (directory-files emacs|site-packages-directory))
      (unless (string-match "^\\." dir)
        (push (expand-file-name dir emacs|site-packages-directory) load-path))))

;; (setq package-archives '(("melpa" . "http://melpa.org/packages/")
;;                          ("melpa-stable" . "http://stable.melpa.org/packages/")
;;                          ;; uncomment below line if you need use GNU ELPA
;;                          ("gnu" . "http://elpa.gnu.org/packages/")))
;; Use mirror in China
;; The index of archive represent its priority
(setq package-archives
      '(("melpa-stable" . "https://elpa.emacs-china.org/melpa-stable/")
        ("melpa" . "https://elpa.emacs-china.org/melpa/")
        ("gnu"   . "https://elpa.emacs-china.org/gnu/")))

;; core packages
(package|add '(cmake-ide    flycheck) "melpa-stable")

(package|add
 '(yasnippet
   auto-yasnippet
   ;; code completion framework
   company
   company-statistics
   ;; save session to disk
   session

   multi-term

   hydra
   ivy
   swiper
   counsel
   counsel-projectile
   ;; counsel-M-x need smex to get history
   smex

   ;; delete packages safely
   ;; emacs 25 has builtin support
   ;; package-safe-delete
   ;; show key bindings while pressing
   which-key
   dired+
   info+
   ibuffer-vc))

;; edit, mark and jump
(package|add
 '( ;; mark tools
   visual-regexp
   multiple-cursors
   fcitx
   expand-region
   ;; wgrep allows you to edit a grep buffer and apply those changes
   ;; to the file buffer.
   wgrep
   ;; provide tree style search jump
   avy
   tiny
   easy-kill
   zzz-to-char))

;; note
(package|add
 '( ;; ipython notebook feature in `org-mode'
   ob-ipython
   org-present
   org-sticky-header
   ;; export colorful src block in `org-mode'
   htmlize
   zeal-at-point))

;; outlooking
(package|add
 '(color-theme
   ;; Highlight braces with their depth
   rainbow-delimiters
   ;; Numbering windows
   window-numbering
   ;; Highlight indentation
   highlight-indentation
   ;; Colorize strings that represent colors
   rainbow-mode
   ;; ^L beautifier
   page-break-lines
   ;; Show information in header-line for `semantic-mode'
   stickyfunc-enhance
   unicode-fonts))

;; latex
(package|add
 '(auctex
   company-auctex))

;; buffer and window
(package|add
 '(;; Manage popup windows
   popwin
   ;; Move buffers between windows
   buffer-move
   ;; Quick switch window
   ace-window))

(package|add
 '(gitignore-mode
   gitconfig-mode
   git-messenger
   git-gutter git-gutter-fringe
   git-link
   git-timemachine))

(package|add
 '( ;; auto compile after .el file load or save
   auto-compile
   ;; pair edit
   lispy
   racket-mode
   macrostep
   ;; slime
   ;; slime-company
   hl-sexp))

(package|add
 '( ;; js company backend
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
   tern
   css-eldoc
   emmet-mode

   restclient
   company-restclient))

(package|add
 '(elpy
   ob-ipython
   py-isort))

(package|add
 '(company-ghc
   ghc
   haskell-mode
   shm hindent
   company-cabal
   idris-mode))

(package|add
 '(irony-eldoc
   irony
   company-irony company-irony-c-headers
   flycheck-irony clang-format
   ggtags
   rtags
   ivy-rtags))

(package|add
 '(sql-indent
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
   cmake-mode cmake-font-lock
   php-mode
   gnuplot-mode
   csharp-mode
   graphviz-dot-mode))

(package|load-all)

(provide 'init-packages)
