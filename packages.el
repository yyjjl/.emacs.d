(defvar package-use-priority nil
  "whether to use priority")

(defvar required-packages
  '(
    ;;---------------------------------------
    ;; basic packages
    ;;---------------------------------------
    dash dash-functional
    s
    ;;---------------------------------------
    ;; core packages
    ;;---------------------------------------
    yasnippet
    company company-statistics
    slime slime-company
    keyfreq
    session
    multi-term
    flycheck flycheck-pos-tip
    counsel hydra ivy swiper
    smex ;; counsel-M-x need smex to get history
    flyspell-correct-ivy flyspell-correct
    bind-key
    fcitx
    ;; define working groups
    workgroups2
    benchmark-init
    idle-require
    ;;---------------------------------------
    ;; theme packages
    ;;---------------------------------------
    color-theme
    ;; helper control fringe(edge of a window) style
    fringe-helper
    hideshowvis
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
    jump ;;
    dired+ dired-filter
    direx
    bookmark+
    ;; select bigger region contain current region or point
    expand-region
    ;; make a scratch buffer
    scratch
    ;; get system environment
    exec-path-from-shell
    ;; inverse of fill region and paragraph
    unfill
    ;; regex expression evaluation tool
    regex-tool
    ;; edit string as origin format in a new buffer
    string-edit
    ;; move whole buffer
    buffer-move
    ;; clipbord tools
    simpleclip
    ;; quick switch window
    ace-window
    dropdown-list
    ;; auto add license
    legalese
    ;; show key bindings while pressing
    guide-key
    visual-regexp
    window-numbering
    find-by-pinyin-dired
    zzz-to-char
    ;;---------------------------------------
    ;; search  packages
    ;;---------------------------------------
    ;; provide tree style search jump
    avy
    ;; find files in project (in directory which has .git)
    find-file-in-project ;;
    wgrep
    ;;---------------------------------------
    ;; c/c++  packages
    ;;---------------------------------------
    clang-format
    company-c-headers
    srefactor
    c-eldoc
    ggtags
    bison-mode
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
    paredit
    ;; ( quack) ;; for scheme
    hl-sexp
    ;;---------------------------------------
    ;; web  packages
    ;;---------------------------------------
    ;; js company backend
    company-tern
    js-doc
    web-mode
    company-web
    ;; optional package add support for angluar 1.x
    ac-html-angular
    ac-html-bootstrap
    js2-mode
    js-comint
    js2-refactor
    multiple-cursors ;; s2-refactor
    tern
    ;; typescript
    typescript-mode
    tide
    sass-mode
    scss-mode
    emmet-mode
    less-css-mode
    ;;---------------------------------------
    ;; python packages
    ;;---------------------------------------
    elpy
    ;;---------------------------------------
    ;; org  mode  packages
    ;;---------------------------------------
    htmlize
    org-bullets
    ;;---------------------------------------
    ;; haskell  packages
    ;;---------------------------------------
    company-ghc
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
    ;; yaml format
    yaml-mode
    ;; haml format
    haml-mode ;;  ass-mode
    markdown-mode
    crontab-mode
    csv-mode
    glsl-mode
    go-mode
    groovy-mode
    cmake-mode
    cpputils-cmake
    gnuplot graphviz-dot-mode)
  "packages required")