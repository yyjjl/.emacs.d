;; -*- lexical-binding: t; -*-

(option! package-use-gnutls t
  :type 'boolean)

(option! only-in-terminal nil
  :type 'boolean)

(option! system-name nil
  :type 'stringp)

(defvar ymacs-modeline-segment-alist ())
(defvar ymacs-modeline-vcs-max-length 12
  "The maximum displayed length of the branch name of version control.")

;;* Ensure modeline is inactive when Emacs is unfocused (and active otherwise)
(declare-function face-remap-remove-relative 'face-remap)
(defvar ymacs-modeline-remap-face-cookie nil)

;;* Project caches
(defvar-local ymacs-modeline--project-detected-p nil)
(defvar-local ymacs-modeline--project-root nil)
(defvar-local ymacs-modeline--project-parent-path nil)

;;* Buffer information
(defvar-local ymacs-modeline--remote-host 'unset)
(defvar-local ymacs-modeline--buffer-file-name nil)

;;* Buffer encoding
(defvar-local ymacs-modeline--buffer-encoding nil)

;;* VCS
(defvar-local ymacs-modeline--vcs-state nil)

;;* Checker
(defvar-local ymacs-modeline--checker-state nil)

;;* LSP
(defvar-local ymacs-modeline--lsp-state nil)

(defface ymacs-modeline-buffer-path
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the dirname part of the buffer path."
  :group 'mode-line-faces)

(defface ymacs-modeline-buffer-file
  '((t (:inherit mode-line-buffer-id)))
  "Face used for the filename part of the mode-line buffer path."
  :group 'mode-line-faces)

(defface ymacs-modeline-buffer-modified
  '((t (:inherit (error bold) :background nil)))
  "Face used for the 'unsaved' symbol in the mode-line."
  :group 'mode-line-faces)

(defface ymacs-modeline-buffer-major-mode
  '((t (:inherit (mode-line-emphasis bold))))
  "Face used for the major-mode segment in the mode-line."
  :group 'mode-line-faces)

(defface ymacs-modeline-project-dir
  '((t (:inherit (font-lock-string-face bold))))
  "Face used for the project directory of the mode-line buffer path."
  :group 'mode-line-faces)

(defface ymacs-modeline-panel
  '((t (:inherit mode-line-highlight)))
  "Face for 'X out of Y' segments, such as `iedit', etc."
  :group 'mode-line-faces)

(defface ymacs-modeline-host
  '((t (:inherit italic)))
  "Face for remote hosts in the mode-line."
  :group 'mode-line-faces)

(defface ymacs-modeline-input-method
  '((t (:inherit (mode-line-emphasis bold))))
  "Face for input method in the mode-line."
  :group 'mode-line-faces)

(defface ymacs-modeline-debug
  '((t (:inherit (font-lock-doc-face bold) :slant normal)))
  "Face for debug-level messages in the mode-line. Used by vcs, checker, etc."
  :group 'mode-line-faces)

(defface ymacs-modeline-info
  '((t (:inherit (success bold))))
  "Face for info-level messages in the mode-line. Used by vcs, checker, etc."
  :group 'mode-line-faces)

(defface ymacs-modeline-warning
  '((t (:inherit (warning bold))))
  "Face for warnings in the mode-line. Used by vcs, checker, etc."
  :group 'mode-line-faces)

(defface ymacs-modeline-urgent
  '((t (:inherit (error bold))))
  "Face for errors in the mode-line. Used by vcs, checker, etc."
  :group 'mode-line-faces)

(defface ymacs-modeline-bar
  '((t (:inherit highlight)))
  "The face used for the left-most bar in the mode-line of an active window."
  :group 'mode-line-faces)

(defface ymacs-modeline-bar-inactive
  `((t (:background ,(face-foreground 'mode-line-inactive))))
  "The face used for the left-most bar in the mode-line of an inactive window."
  :group 'mode-line-faces)

(defface ymacs-modeline-debug-visual
  '((((class color) (background light))
     (:background "#D4843E"))
    (((class color) (background dark))
     (:background "#915B2D")))
  "Face to use for the mode-line while debugging."
  :group 'ymacs-modeline)

(defface ymacs-modeline-lsp-success
  '((t (:inherit success :weight normal)))
  "Face for LSP success state."
  :group 'mode-line-faces)

(defface ymacs-modeline-lsp-warning
  '((t (:inherit warning :weight normal)))
  "Face for LSP warning state."
  :group 'mode-line-faces)

(defface ymacs-modeline-lsp-error
  '((t (:inherit error :weight normal)))
  "Face for LSP error state."
  :group 'mode-line-faces)

(defface ymacs-modeline-buffer-timemachine
  '((t (:inherit (ymacs-modeline-buffer-file italic underline))))
  "Face for timemachine status."
  :group 'mode-line-faces)

(with-no-warnings
  (setq TeX-auto-global (expand-cache! "auctex/"))
  (setq abbrev-file-name (expand-cache! "abbrev.el"))
  (setq backup-directory-alist (list (cons "." (expand-cache! "backups/"))))
  (setq bookmark-default-file (expand-cache! "bookmarks.el"))
  (setq calc-settings-file (expand-cache! "calc.el"))
  (setq dap-breakpoints-file (expand-cache! ".dap-breakpoints"))
  (setq eshell-directory-name (expand-cache! "eshell/"))
  (setq gnus-directory (expand-cache! "gnus"))
  (setq gnus-init-file (expand-etc! "gnus.el"))
  (setq lsp-session-file (expand-cache! "lsp-sessions"))
  (setq mc/list-file (expand-cache! "mc-lists.el"))
  (setq org-export-async-init-file (expand-etc! "org-async-init.el"))
  (setq org-id-locations-file (expand-cache! "org-id-locations"))
  (setq org-preview-latex-image-directory (expand-cache! "ltximg/"))
  (setq org-publish-timestamp-directory (expand-cache! "org-timestamps/"))
  (setq org-templates-directory (expand-etc! "org-templates"))
  (setq package-quickstart-file (expand-cache! "quickstart.el"))
  (setq persistent-scratch-save-file (expand-cache! "scratch"))
  (setq project-list-file (expand-cache! "projects"))
  (setq recentf-save-file (expand-cache! "recentf.el"))
  (setq save-place-file (expand-cache! "places.el"))
  (setq savehist-file (expand-cache! "history.el"))
  (setq semanticdb-default-save-directory (expand-cache! "semanticdb/"))
  (setq srecode-map-save-file (expand-cache! "srecode-map.el"))
  (setq tramp-persistency-file-name (expand-cache! "tramp.el"))
  (setq transient-history-file (expand-cache! "transient/history.el"))
  (setq transient-levels-file (expand-cache! "transient/levels.el"))
  (setq transient-values-file (expand-cache! "transient/values.el"))
  (setq treemacs-persist-file (expand-cache! "treemacs-persist"))
  (setq url-configuration-directory (expand-cache! "url/")))
