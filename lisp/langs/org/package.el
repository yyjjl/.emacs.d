;;; -*- lexical-binding: t; -*-

(option! org-latex-ignore-internal-label nil
  :type 'boolean
  :safe #'booleanp)

(option! org-publish-default-project nil
  :type 'string
  :safe #'stringp)

(require-packages!
 org-download
 (auctex :compile (latex tex preview reftex))
 ;; Export colorful src block in `org-mode'
 htmlize)

(ymacs-editor//set-narrow-handler
 :mode org-mode
 :fn1 org-narrow-to-subtree
 :fn2 org-narrow-to-element)

(defvar ymacs-org-remove-texfile t)

(defvar ymacs-org-block-key-bindings
  '(("p" . org-previous-block)
    ("\C-p" . org-previous-block)
    ("n" . org-next-block)
    ("\C-n" . org-next-block)))

(defvar ymacs-org-table-extra-map
  (define-key! :map (make-sparse-keymap)
    ("t" . org-todo)
    ("a" . orgtbl-ascii-plot)
    ("n" . org-table-create)
    ("p" . org-plot/gnuplot)
    ("i" . org-table-import)
    ("e" . org-table-export)
    ("d" . org-table-delete-column)
    ("c" . org-table-insert-column)
    ("r" . org-table-show-reference)))

;; Config for publish site from org files
(defvar ymacs-org-project-base-dir
  (expand-file-name "org-notes" (expand-file-name "home" user-emacs-directory)))
(defvar ymacs-org-project-src-dir (expand-file-name "src" ymacs-org-project-base-dir))
(defvar ymacs-org-project-dst-dir (expand-file-name "dst" ymacs-org-project-base-dir))
(defvar-local ymacs-org-publish-last-project nil)

(define-key!
  ("C-x O" . ymacs-org/project-open)
  ([C-f6] . ymacs-org/display-latex-fragment-at-point))

(autoload #'ymacs-transient/org-download (expand! "commands") nil t)

(put 'org-preview-latex-image-directory 'safe-local-variable #'stringp)
(put 'org-download-image-dir 'safe-local-variable #'stringp)
(put 'org-download-heading-lvl 'safe-local-variable (lambda (-x) (or (null -x) (integerp -x))))
