;;; -*- lexical-binding: t; -*-

(require-packages!
 hl-todo
 page-break-lines
 doom-themes
 which-key
 winum
 highlight-indent-guides)

(load-theme 'doom-molokai t)
(doom-themes-org-config)

(defvar ymacs-ui-view-code-modes
  '((t display-line-numbers-mode
       view-mode
       line-number-mode
       column-number-mode
       size-indication-mode
       highlight-indent-guides-mode)))

(define-key!
  ("C-x , ," . ymacs-ui/view-code-mode))

(defvar ymacs-modeline-segment-alist ())
(defvar ymacs-modeline-vcs-max-length 12
  "The maximum displayed length of the branch name of version control.")

;;* Ensure modeline is inactive when Emacs is unfocused (and active otherwise)
(declare-function face-remap-remove-relative 'face-remap)
(defvar ymacs-modeline-remap-face-cookie nil)

;;* Project caches
(declare-function projectile-project-root 'projectile)
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
