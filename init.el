(defvar emacs-config-directory
  (expand-file-name "lisp" user-emacs-directory)
  "All configuration in this directory")

(defvar emacs-var-direcotry
  (expand-file-name "var" user-emacs-directory)
  "All data and external executable file in this direcotry")

;; Add `emacs-config-directory' to `load-path'
(add-to-list 'load-path emacs-config-directory)

;; All packages required in this section are defined in `init-packages'
(require 'core-lib)
;; Set some important variables
(require 'core-vars)
(require 'core-defaults)
;; Load core packages
(require 'core-packages-lib)
;; (package-initialize)
(require 'core-ui)
(require 'core-packages)
(require 'core-semantic)
(require 'core-hideshow)

;; ----------------------------------------
;; Optional packages
;; ----------------------------------------

;; All packages required in this section are defined in their init files
(require 'init-org)
(require 'init-notes)
(require 'init-org-ipython)

(when (or env-has-aspell-p env-has-hunspell-p)
  (require 'init-spelling))
(when env-has-latex-p
  (require 'init-latex))
(when env-has-git-p
  (require 'init-git))
(when env-has-gtags-p
  (require 'init-tags))

;; Programming modes
(require 'init-lsp-mode)
(require 'init-cpp)
(require 'init-haskell)
(require 'init-python)
(require 'init-lisp)
(require 'init-sh)
(require 'init-web)
(require 'init-rust)
(require 'init-java)
(require 'init-perl5)
(require 'init-idris)

;; Some keybindings using when editing
(require 'init-editing)
(require 'init-dired)
(require 'init-ibuffer)
(require 'init-realgud)

;; Other small tools
;; (require 'init-hydra-extra)
;; (require 'init-prettify-symbols)

(when env-has-mpv-p
  (require 'init-emms))

(require 'init-extra)
(require 'init-tabnine)
(require 'init-play)
