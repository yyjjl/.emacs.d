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
(defvar ymacs-org-project-base-dir "~/project/org")
(defvar ymacs-org-project-src-dir "~/project/org/src")

(defcustom ymacs-org-project-sync-command nil
  "Command to sync org notes."
  :group 'org
  :type 'directory
  :safe #'stringp)

(defcustom ymacs-org-master-file nil
  "master file for org"
  :group 'org
  :type 'file
  :safe #'file-exists-p)

(defcustom ymacs-org-latex-ignore-internal-label nil
  ""
  :group 'org
  :type 'file
  :safe #'booleanp)

(defvar ymacs-org--included-files (make-hash-table :test #'equal))

(define-key!
  ("C-c '" . poporg-dwim)
  ("C-x O" . ymacs-org/project-open)
  ([C-f6] . ymacs-org/display-latex-fragment-at-point)
  ([C-f12] . org-capture))

(put 'org-preview-latex-image-directory 'safe-local-variable #'stringp)
