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
  (let* ((key (expand-file-name -directory))
         (value (gethash key ymacs-editor-project-cache)))
    (when (not (eq value 'none))
      (when (not (and value (file-exists-p (project-root value))))
        (setq value (or (when-let (root (locate-dominating-file -directory ".project"))
                          (cons 'local root))
                        (project-try-vc -directory)
                        ;; default is 'none
                        'none))
        (puthash key value ymacs-editor-project-cache))
      (when (not (eq value 'none))
        value))))

(defsubst ymacs-editor//project-root (&optional -directory)
  (when-let (project (project-current nil -directory))
    (project-root project)))

(defsubst ymacs-editor//project-root-or-default ()
  (or (when-let (project (project-current))
        (project-root project))
      default-directory))

(defsubst ymacs-editor//setup-project-internal (-root -bindings)
  (let ((default-directory -root))
    (unless (or (file-exists-p ".project")
                (not (yes-or-no-p (format "create .project file in %s" -root))))
      (f-touch ".project")))
  (cl-loop
   for (var . val) in -bindings
   do (add-dir-local-variable nil var val))
  (save-buffer)
  (hack-dir-local-variables-for-project!))

(after! project
  (when (boundp 'project-prefix-map)
    (define-key! :map project-prefix-map
      ("E" . ymacs-editor/edit-dir-locals)
      ("R" . ymacs-editor/generate-tags)
      ("U" . citre-update-this-tags-file)
      ("C" . citre-edit-tags-file-recipe)
      ("i" . ymacs-editor/invalid-project-cache)))

  (eval-when! (executable-find "fzf")
    (when (boundp 'project-prefix-map)
      (define-key! :map project-prefix-map
        ("f" . ymacs-editor/fzf)))
    (setcar (assoc 'project-find-file project-switch-commands) #'ymacs-editor/fzf))

  (setq project-find-functions '(ymacs-editor//project)))
