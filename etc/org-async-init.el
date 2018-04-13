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
;; (package-initialize)
;; Load core packages
(require 'core-packages)
(require 'core-ui)

;; All packages required in this section are defined in their init files
(setq core--buffer-useful nil)
(require 'init-org)
(require 'org)
(require 'ox)

;; Programming modes
(require 'init-haskell)
(require 'init-lisp)
(require 'init-sh)
(require 'init-rust)
(require 'init-extra)

(remove-hook 'after-init-hook 'core|enable-modes-hook)
(setq org-export-async-debug nil)
