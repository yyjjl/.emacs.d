(require 'package)

;;---------------------------------------
;; Patch up annoying package.el quirks
;;---------------------------------------

(defun close-autoloads (name pkg-dir)
  "Stop package.el from leaving open autoload files lying around."
  (let ((path (expand-file-name (concat
                                 ;; name is string when emacs <= 24.3.1,
                                 (if (symbolp name) (symbol-name name) name)
                                 "-autoloads.el") pkg-dir)))
    (with-current-buffer (find-file-existing path)
      (kill-buffer nil))))

(advice-add 'package-generate-autoloads :after #'close-autoloads)

;;---------------------------------------
;; On-demand installation of packages
;;---------------------------------------
(defun require-package (package &optional min-version no-refresh)
  "Ask elpa to install given PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

(defun package-set-right-archive (pkg)
  "get right archive content by priority"
  (let ((pkg-info (cdr pkg))
        cur-archive
        (cur-priority -1))
    (dolist (info pkg-info)
      (let ((pri (assoc-string (package-desc-archive info)
                               package-archive-priority)))
        (when (> (if pri (cdr pri) 0) cur-priority)
          (setq cur-archive info)
          (setq cur-priority (if pri (cdr pri) 0)))))
    (setf (cdr pkg) (list cur-archive))))

(when package-use-priority
  (defun manager-packages-with-priority ()
    (dolist (pkg package-archive-contents)
      (package-set-right-archive pkg)))
  (advice-add 'package-read-all-archive-contents :after
              #'manager-packages-with-priority))

;;---------------------------------------
;; Standard package repositories
;;---------------------------------------

;; We include the org repository for completeness, but don't use it.
;; Lock org-mode temporarily:
;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ;; uncomment below line if you need use GNU ELPA
                         ("gnu" . "http://elpa.gnu.org/packages/")))

(defvar package-archive-priority
  '(("melpa" . 1)
    ("melpa-stable" . 2)
    ("gnu" . 0))
  "package archive priority")

;;---------------------------------------
;; Fire up package.el and ensure the following packages are installed.
;;---------------------------------------

(package-initialize)

(defvar required-packages
  '(
    ;;---------------------------------------
    ;; core packages
    ;;---------------------------------------
    yasnippet
    company company-statistics
    slime slime-company
    ;; keyfreq
    session
    multi-term
    flycheck flycheck-pos-tip
    counsel hydra ivy swiper tiny
    counsel-projectile
    smex ;; counsel-M-x need smex to get history
    bind-key
    fcitx
    ;;---------------------------------------
    ;; theme packages
    ;;---------------------------------------
    color-theme
    ;; helper control fringe(edge of a window) style
    fringe-helper
    ;; highlight braces with their depth
    rainbow-delimiters
    ;; spaceline
    ;; give very variable a special color
    rainbow-mode ;;  required by css-mode
    ;; make buffer even line and odd line having different color
    stripe-buffer
    ;; ^L beautifier
    page-break-lines
    ;;---------------------------------------
    ;; latex packages
    ;;---------------------------------------
    auctex
    company-auctex
    ;;---------------------------------------
    ;; tool packages
    ;;---------------------------------------
    ;; provide functions to build jump action
    restclient company-restclient
    package-safe-delete
    jump ;;
    dired+ dired-filter dired-k
    bookmark+
    ;; select bigger region contain current region or point
    expand-region
    ;; make a scratch buffer
    scratch
    ;; get system environment
    exec-path-from-shell
    ;; inverse of fill region and paragraph
    unfill
    ;; move whole buffer
    buffer-move
    ;; clipbord tools
    simpleclip
    ;; quick switch window
    ace-window
    dropdown-list
    ;; show key bindings while pressing
    which-key
    visual-regexp
    window-numbering
    ;; find-by-pinyin-dired
    zzz-to-char
    stickyfunc-enhance
    zeal-at-point
    ;;---------------------------------------
    ;; search  packages
    ;;---------------------------------------
    ;; provide tree style search jump
    avy
    wgrep
    ;;---------------------------------------
    ;; c/c++  packages
    ;;---------------------------------------
    clang-format
    irony-eldoc
    irony
    company-irony
    flycheck-irony
    rtags
    ;;---------------------------------------
    ;; git packages
    ;;---------------------------------------
    gitignore-mode
    gitconfig-mode
    git-messenger
    git-gutter-fringe git-gutter
    git-link
    git-timemachine
    ;;---------------------------------------
    ;; lisp  packages
    ;;---------------------------------------
    ;; auto compile after .el file load or save
    auto-compile
    ;; pair edit
    lispy
    racket-mode
    hl-sexp
    ;;---------------------------------------
    ;; web  packages
    ;;---------------------------------------
    ;; js company backend
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
    ;; typescript
    typescript-mode
    tide
    css-eldoc
    emmet-mode
    ;;---------------------------------------
    ;; python packages
    ;;---------------------------------------
    anaconda-mode
    company-anaconda
    yapfify
    py-isort
    ;;---------------------------------------
    ;; org  mode  packages
    ;;---------------------------------------
    htmlize
    org-bullets
    ;;---------------------------------------
    ;; haskell  packages
    ;;---------------------------------------
    company-ghc company-ghci
    ghc
    haskell-mode
    shm hindent
    company-cabal
    ;;---------------------------------------
    ;; ibuffer  packages
    ;;---------------------------------------
    ibuffer-vc
    ;;---------------------------------------
    ;; other mode  packages
    ;;---------------------------------------
    company-shell
    sql-indent
    ;; yaml format
    yaml-mode
    ;; haml format
    haml-mode ;;  ass-mode
    markdown-mode
    crontab-mode
    csv-mode
    sass-mode
    less-css-mode
    scss-mode
    glsl-mode lua-mode
    go-mode
    groovy-mode
    cmake-mode cmake-font-lock cmake-ide
    php-mode
    graphviz-dot-mode)
  "packages required")

;; install missing packages
(let (freshed-p)
  (dolist (pkg required-packages)
    (when (consp pkg)
      (setq pkg (car pkg)))
    (unless (package-installed-p pkg)
      (unless freshed-p
        (setq freshed-p t)
        (package-refresh-contents))
      (package-install pkg))))
