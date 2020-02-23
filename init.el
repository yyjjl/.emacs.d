(when (< emacs-major-version 27)
  (load (expand-file-name "early-init" user-emacs-directory)))

;; All packages required in this section are defined in `init-packages'
(require 'core-lib)
;; Set some important variables
(require 'core-vars)
(require 'core-defaults)
(require 'core-custom)

;; Load core packages
(require 'core-packages-lib)
(require 'core-ui)
(require 'core-features)

(require 'init-editing)
(require 'init-dired)
(require 'init-ibuffer)

;; ----------------------------------------
;; Optional packages
;; ----------------------------------------

;; All packages required in this section are defined in their init files
(require 'init-spelling)
(when emacs-use-latex-p
  (require 'init-latex)
  (require 'init-org)
  (require 'init-notes)
  (require 'init-sage))
(when emacs-use-git-p
  (require 'init-git))
(when emacs-use-gtags-p
  (require 'init-tags))

;; Programming modes
(unless emacs-lite-setup-p
  (require 'init-go)
  (require 'init-haskell)
  (require 'init-idris)
  (require 'init-java)
  (require 'init-rust)
  (require 'init-web))

(require 'init-lsp-mode)
(require 'init-perl5)
(require 'init-cpp)
(require 'init-python)
(require 'init-lisp)
(require 'init-sh)

(require 'init-realgud)

;; Other small tools
;; (require 'init-hydra-extra)
;; (require 'init-prettify-symbols)

(when emacs-use-mpv-p
  (require 'init-emms))

(require 'init-extra)
(unless emacs-lite-setup-p
  (require 'init-tabnine)
  (require 'init-play))
(require 'init-treemacs)
