(defvar emacs-config-directory
  (expand-file-name "lisp" user-emacs-directory)
  "All configuration in this directory")

;; Add `emacs-config-directory' to `load-path'
(add-to-list 'load-path emacs-config-directory)

;; All packages required in this section are defined in `init-packages'
(require 'core-lib)
;; Set some important variables
(require 'core-vars)
(require 'core-defaults)
(require 'core-ui)
;; (package-initialize)
;; Load core packages
(require 'core-packages)

;; All packages required in this section are defined in their init files
(require 'init-org)

(if (or spelling-has-aspell-p
        spelling-has-hunspell-p)
    (require 'init-spelling))

(when latex-use-latex-p
  (require 'init-latex))

(if git-has-git-p
    (require 'init-git))

(if tags-has-ggtags-p
    (require 'init-tags))

;; Programming modes
(require 'init-cpp)
(require 'init-haskell)
(require 'init-python)
(require 'init-lisp)
(require 'init-web-mode)
(require 'init-sh)
(require 'init-javascript)
(require 'init-css)

;; Some keybindings using when editing
(require 'init-editing)
(require 'init-dired)
(require 'init-ibuffer)
(require 'init-windows)
(require 'init-gud)

;; (require 'init-linum-mode)

;; Other small tools
(require 'init-extra)
