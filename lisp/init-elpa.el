(require 'package)

;;------------------------------------------------------------------------------
;; Patch up annoying package.el quirks
;;------------------------------------------------------------------------------
(defadvice package-generate-autoloads (after close-autoloads (name pkg-dir) activate)
  "Stop package.el from leaving open autoload files lying around."
  (let ((path (expand-file-name (concat
                                 ;; name is string when emacs <= 24.3.1,
                                 (if (symbolp name) (symbol-name name) name)
                                 "-autoloads.el") pkg-dir)))
    (with-current-buffer (find-file-existing path)
      (kill-buffer nil))))

;;------------------------------------------------------------------------------
;; Add support to package.el for pre-filtering available packages
;;------------------------------------------------------------------------------
(defvar package-filter-function nil
  "Optional predicate function used to internally filter packages used by package.el.
The function is called with the arguments PACKAGE VERSION ARCHIVE, where
PACKAGE is a symbol, VERSION is a vector as produced by `version-to-list', and
ARCHIVE is the string name of the package archive.")

(defadvice package--add-to-archive-contents
  (around filter-packages (package archive) activate)
  "Add filtering of available packages using `package-filter-function', if non-nil."
  (when (or (null package-filter-function)
      (funcall package-filter-function
         (car package)
         (funcall (if (fboundp 'package-desc-version)
          'package--ac-desc-version
        'package-desc-vers)
            (cdr package))
         archive))
    ad-do-it))

;;------------------------------------------------------------------------------
;; On-demand installation of packages
;;------------------------------------------------------------------------------
(defun require-package (package &optional min-version no-refresh)
  "Ask elpa to install given PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))

;;------------------------------------------------------------------------------
;; Standard package repositories
;;------------------------------------------------------------------------------

;; We include the org repository for completeness, but don't use it.
;; Lock org-mode temporarily:
;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ;; ("melpa-stable" . "http://stable.melpa.org/packages/")
                         ;; uncomment below line if you need use GNU ELPA
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ))

;;------------------------------------------------------------------------------
;; Fire up package.el and ensure the following packages are installed.
;;------------------------------------------------------------------------------

(package-initialize)

;;------------------------------------------------------------------------------
;; basic packages
;;------------------------------------------------------------------------------

(require-package 'dash)
(require-package 's)

;;------------------------------------------------------------------------------
;; core packages
;;------------------------------------------------------------------------------

(require-package 'yasnippet)
(require-package 'company)
(require-package 'company-statistics)
(require-package 'slime)
(require-package 'keyfreq)
(require-package 'session)
(require-package 'multi-term)
(require-package 'flycheck)
(require-package 'counsel)
(require-package 'ivy)
(require-package 'smex) ;; counsel-M-x need smex to get history
(require-package 'flyspell-correct-ivy)
(require-package 'bind-key)
(require-package 'hydra)
(require-package 'fcitx)

;;------------------------------------------------------------------------------
;; theme packages
;;------------------------------------------------------------------------------

(require-package 'color-theme)
;; helper control fringe(edge of a window) style
(require-package 'fringe-helper)
;; highlight braces with their depth
(require-package 'rainbow-delimiters)
(require-package 'spaceline)
;; give very variable a special color
(require-package 'rainbow-mode) ;; required by css-mode
;; make buffer even line and odd line having different color
(require-package 'stripe-buffer)
;; ^L beautifier
(require-package 'page-break-lines)

;;------------------------------------------------------------------------------
;; latex packages
;;------------------------------------------------------------------------------

(require-package 'auctex)
(require-package 'company-auctex)

;;------------------------------------------------------------------------------
;; tool packages
;;------------------------------------------------------------------------------

;; provide functions to build jump action
(require-package 'jump) ;; require findr
(require-package 'dired+)
(require-package 'dired-filter)
(require-package 'direx)
(require-package 'bookmark+)
;; select bigger region contain current region or point
(require-package 'expand-region)
;; make a scratch buffer
(require-package 'scratch)
;; define working groups
(require-package 'workgroups2)
;; get system environment
(require-package 'exec-path-from-shell)
;; inverse of fill region and paragraph
(require-package 'unfill)
;; regex expression evaluation tool
(require-package 'regex-tool)
;; edit string as origin format in a new buffer
(require-package 'string-edit)
;; move whole buffer
(require-package 'buffer-move)
;; clipbord tools
(require-package 'simpleclip)
;; quick switch window
(require-package 'ace-window)
(require-package 'dropdown-list)
;; auto add license
(require-package 'legalese)
;; show key bindings while pressing
(require-package 'guide-key)
(require-package 'visual-regexp)
(require-package 'window-numbering)
(require-package 'find-by-pinyin-dired)
(require-package 'zzz-to-char)

;;------------------------------------------------------------------------------
;; search  packages
;;------------------------------------------------------------------------------

;; provide tree style search jump
(require-package 'avy)
;; split regex candidate
(require-package 'swiper) ;; requires ivy
;; find files in project (in directory which has .git)
(require-package 'find-file-in-project) ;; required by elpy

;;------------------------------------------------------------------------------
;; c/c++  packages
;;------------------------------------------------------------------------------

(require-package 'clang-format)
(require-package 'company-c-headers)
(require-package 'srefactor)
(require-package 'c-eldoc)
(require-package 'ggtags)

;;------------------------------------------------------------------------------
;; git packages
;;------------------------------------------------------------------------------

(require-package 'gitignore-mode)
(require-package 'gitconfig-mode)
(require-package 'git-messenger)
(require-package 'git-gutter)
(require-package 'git-link)
(require-package 'git-timemachine)

;;------------------------------------------------------------------------------
;; lisp  packages
;;------------------------------------------------------------------------------

;; auto compile after .el file load or save
(require-package 'auto-compile)
;; pair edit
(require-package 'paredit)
;; (require-package 'quack) ;; for scheme
(require-package 'hl-sexp)

;;------------------------------------------------------------------------------
;; web  packages
;;------------------------------------------------------------------------------

;; js company backend
(require-package 'company-tern)
(require-package 'js-doc)

(require-package 'web-mode)
(require-package 'company-web)
;; optional package add support for angluar 1.x
(require-package 'ac-html-angular)
(require-package 'ac-html-bootstrap)

(require-package 'js2-mode)
(require-package 'js-comint)
(require-package 'js2-refactor)
(require-package 'multiple-cursors) ;;require by js2-refactor
(require-package 'tern)

;; typescript
(require-package 'typescript-mode)
(require-package 'tide)

(require-package 'sass-mode)
(require-package 'scss-mode)

(require-package 'emmet-mode)
(require-package 'less-css-mode)

;;------------------------------------------------------------------------------
;; python packages
;;------------------------------------------------------------------------------

(require-package 'elpy)

;;------------------------------------------------------------------------------
;; org  mode  packages
;;------------------------------------------------------------------------------

(require-package 'htmlize)
(require-package 'org-bullets)
(require-package 'ox-ioslide)

;;------------------------------------------------------------------------------
;; haskell  packages
;;------------------------------------------------------------------------------

(require-package 'company-ghc)
(require-package 'ghc)
(require-package 'haskell-mode)
(require-package 'shm)
(require-package 'company-cabal)

;;------------------------------------------------------------------------------
;; ibuffer  packages
;;------------------------------------------------------------------------------

(require-package 'ibuffer-vc)

;;------------------------------------------------------------------------------
;; other mode  packages
;;------------------------------------------------------------------------------
(require-package 'company-shell)
;; yaml format
(require-package 'yaml-mode)
;; haml format
(require-package 'haml-mode) ;; required by sass-mode
(require-package 'markdown-mode)
(require-package 'crontab-mode)
(require-package 'csv-mode)
(require-package 'glsl-mode)
(require-package 'go-mode)
(require-package 'groovy-mode)
(require-package 'cmake-mode)
(require-package 'cpputils-cmake)
(require-package 'gnuplot)


(provide 'init-elpa)
