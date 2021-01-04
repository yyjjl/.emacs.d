;; -*- lexical-binding: t; -*-

(defvar ymacs-default-autoloads-file (expand-cache! "autoloads.el")
  "Autoloads file")

(defvar ymacs-default-autosave-interval 300)
(defvar ymacs-default-autosave-hook
  '(recentf-save-list
    save-place-kill-emacs-hook))

(defvar ymacs-default-input-method-alist ())

(defvar ymacs-default-savehist-exclude-variables
  '(load-history
    register-alist
    vc-comment-ring
    flyspell-auto-correct-ring
    org-mark-ring
    planner-browser-file-display-rule-ring))

(defvar ymacs-default-next-error-buffer-modes
  '(occur-mode
    grep-mode
    ivy-occur-mode
    xref--xref-buffer-mode
    compilation-mode))

(defvar ymacs-default-external-file-regexp
  (eval-when-compile
    (let ((extentions '("pdf" "djvu" "dvi"
                        "odf" "odg" "odp" "ods" "odt"
                        "docx?" "xlsx?" "pptx?"
                        "mkv" "avi" "mp4" "rmvb")))
      (rx-to-string
       `(and "."
             (or ,@extentions ,@(mapcar #'upcase extentions))
             string-end)))))

(autoload 'ansi-color-apply-on-region "ansi-color")
(autoload 'ymacs-default/generate-autoloads (expand! "commands-package") nil t)

(declare-function winner-undo 'winner)
(declare-function winner-redo 'winner)

(unless (file-exists-p ymacs-cache-direcotry)
  (make-directory ymacs-cache-direcotry))

(define-key! :prefix "C-x"
  ("2" . ymacs-window/split-vertically)
  ("3" . ymacs-window/split-horizontally)
  ("|" . ymacs-window/force-split-horizontally)
  ("_" . ymacs-window/force-split-vertically)

  (", o" . recentf-open-files)
  ("C-b" . ibuffer)
  ("C-d" . find-name-dired)

  ("m" . view-echo-area-messages)

  ("c" . ymacs-default/cleanup-buffer-safe)
  (", -" . ymacs-default/copy-file-name)

  ("R" . ymacs-default/rename-this-file-and-buffer)
  ("W" . ymacs-default/copy-this-file)
  ("D" . ymacs-default/delete-this-file)

  ("w [" . winner-undo)
  ("w ]" . winner-redo))

(define-key!
  ("C-c q" . auto-fill-mode)

  ("M-s f" . ymacs-default/font-faces-at-point)
  ("M-s o" . ymacs-default/occur-dwim)

  ("C-<down>" . text-scale-decrease)
  ("C-<up>" . text-scale-increase)

  ("C-x G" . revert-buffer)
  ("C-x I" . clone-indirect-buffer)

  ("RET" . newline-and-indent)

  ("M-/" . hippie-expand)

  ("M-n" . next-error)
  ("M-p" . previous-error)
  ("M-N" . ymacs-default/select-error-buffer))

(define-key! :map indent-rigidly-map
  ("[" . indent-rigidly-left)
  ("]" . indent-rigidly-right)
  ("{" . indent-rigidly-left-to-tab-stop)
  ("}" . indent-rigidly-right-to-tab-stop))

(define-key! :map special-mode-map
  ("u" . scroll-down-command)
  ("y" . scroll-down-line)
  ("e" . scroll-up-line))

(put 'scroll-left 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'list-timers 'disabled nil)

(with-no-warnings
  (setq TeX-auto-global (expand-cache! "auctex/"))
  (setq abbrev-file-name (expand-cache! "abbrev.el"))
  (setq amx-save-file (expand-cache! "amx-items.el"))
  (setq backup-directory-alist (list (cons "." (expand-cache! "backups/"))))
  (setq bookmark-default-file (expand-cache! "bookmarks.el"))
  (setq calc-settings-file (expand-cache! "calc.el"))
  (setq cnfonts-directory (expand-cache! "cnfonts/"))
  (setq company-statistics-file (expand-cache! "company-statistics-cache.el"))
  (setq dap-breakpoints-file (expand-cache! ".dap-breakpoints"))
  (setq desktop-dirname (expand-cache! "desktop"))
  (setq desktop-path (list desktop-dirname))
  (setq desktop-restore-frames nil)
  (setq emms-directory (expand-cache! "emms"))
  (setq eshell-directory-name (expand-cache! "eshell/"))
  (setq features-file (expand-file-name "features.el" user-emacs-directory))
  (setq gnus-directory (expand-cache! "gnus"))
  (setq gnus-init-file (expand-etc! "gnus.el"))
  (setq irony-server-install-prefix (expand-cache! "irony"))
  (setq irony-user-dir (expand-cache! "irony/"))
  (setq ispell-personal-dictionary (expand-cache! "aspell.pws"))
  (setq lsp-session-file (expand-cache! "lsp-sessions"))
  (setq mc/list-file (expand-cache! "mc-lists.el"))
  (setq org-default-notes-file (expand-cache! "org/*note*"))
  (setq org-export-async-init-file (expand-etc! "org-async-init.el"))
  (setq org-preview-latex-image-directory (expand-cache! "ltximg/"))
  (setq org-publish-timestamp-directory (expand-cache! "org-timestamps/"))
  (setq org-templates-directory (expand-etc! "org-templates"))
  (setq package-quickstart-file (expand-cache! "quickstart.el"))
  (setq projectile-cache-file (expand-cache! "projectile-cache.el"))
  (setq projectile-known-projects-file (expand-cache! "projectile-bookmarks.el"))
  (setq pyim-dcache-directory (expand-cache! "pyim/dcache"))
  (setq pyim-directory (expand-cache! "pyim"))
  (setq recentf-save-file (expand-cache! "recentf.el"))
  (setq request-storage-directory (expand-cache! "request"))
  (setq rtags-path (expand-cache! "rtags/bin"))
  (setq save-place-file (expand-cache! "places.el"))
  (setq savehist-file (expand-cache! "history.el"))
  (setq semanticdb-default-save-directory (expand-cache! "semanticdb/"))
  (setq session-save-file (expand-cache! "session.el"))
  (setq skeletor-project-directory (expand-file-name "~/working/"))
  (setq skeletor-user-directory (expand-etc! "project-templates/"))
  (setq smex-save-file (expand-cache! "smex-items.el"))
  (setq srecode-map-save-file (expand-cache! "srecode-map.el"))
  (setq tramp-persistency-file-name (expand-cache! "tramp.el"))
  (setq transient-history-file (expand-cache! "transient/history.el"))
  (setq transient-levels-file (expand-cache! "transient/levels.el"))
  (setq transient-values-file (expand-cache! "transient/values.el"))
  (setq treemacs-persist-file (expand-cache! "treemacs-persist"))
  (setq url-configuration-directory (expand-cache! "url/")))
