;;; -*- lexical-binding: t; -*-

(defvar ymacs-editor-project-cache (make-hash-table :test #'equal))
(defvar ymacs-editor-project-invalidate-cache-empty-vars
  '((ymacs-modeline--buffer-file-name . nil)
    (ymacs-modeline--vcs-state . nil)
    (ymacs-modeline--project-detected-p . nil)
    (ymacs-modeline--project-root . nil)))

;; Declare directories with ".project" as a project
(cl-defmethod project-root ((project (head local)))
  (cdr project))

(defun ymacs-editor//project (-directory)
  "Find current project"
  (or ymacs-default-project
      (let* ((key (expand-file-name -directory))
             (value (gethash key ymacs-editor-project-cache)))
        (unless (and value (file-exists-p (project-root value)))
          (setq value (or (project-try-vc -directory)
                          (when-let (root (locate-dominating-file -directory ".project"))
                            (cons 'local root))))
          (puthash key value ymacs-editor-project-cache))
        value)))

(defsubst ymacs-editor//project-root (&optional -directory)
  (when-let (project (project-current nil -directory))
    (project-root project)))

(defsubst ymacs-editor//project-root-or-default ()
  (or (when-let (project (project-current))
        (project-root project))
      default-directory))

(after! project
  (when (boundp 'project-prefix-map)
    (define-key! :map project-prefix-map
      ("E" . ymacs-editor/edit-dir-locals)
      ("R" . ymacs-editor/generate-tags)
      ("C" . ymacs-editor/generate-ctags-for-project)
      ("i" . ymacs-editor/invalid-project-cache)))

  (setq project-find-functions '(ymacs-editor//project)))
