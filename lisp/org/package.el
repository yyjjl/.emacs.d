;;; -*- lexical-binding: t; -*-

(require-packages!
 (org :compile (org ob ox-html org-table))
 ;; Export colorful src block in `org-mode'
 htmlize
 poporg
 company-auctex)

(defvar ymacs-org-remove-texfile t)

(defvar ymacs-org-block-key-bindings
  '(("p" . org-previous-block)
    ("\C-p" . org-previous-block)
    ("n" . org-next-block)
    ("\C-n" . org-next-block)))

(defvar ymacs-org-table-extra-map
  (define-key! :map (make-sparse-keymap)
    ("t" . orgtbl-insert-radio-table)
    ("c" . org-table-create)
    ("I" . org-table-import)
    ("e" . org-table-export)
    ("d" . org-table-delete-column)
    ("i" . org-table-insert-column)
    ("r" . org-table-show-reference)))

;; Config for publish site from org files
(defvar ymacs-org-project-base-dir (expand-file-name "org-notes" ymacs-private-directory))
(defvar ymacs-org-project-src-dir (expand-file-name "src" ymacs-org-project-base-dir))
(defvar ymacs-org-project-dst-dir (expand-file-name "dst" ymacs-org-project-base-dir))
(defvar ymacs-org-publish-last-project nil)

(defcustom ymacs-org-project-sync-command nil
  "Command to sync org notes."
  :group 'org
  :type 'directory
  :safe #'stringp)

(defcustom ymacs-org-latex-ignore-internal-label nil
  ""
  :group 'org
  :type 'file
  :safe #'booleanp)

(define-key!
  ("C-c '" . poporg-dwim)
  ("C-x O" . ymacs-org/project-open)
  ([C-f6] . ymacs-org/display-latex-fragment-at-point)
  ([C-f12] . org-capture))

(put 'org-preview-latex-image-directory 'safe-local-variable #'stringp)
