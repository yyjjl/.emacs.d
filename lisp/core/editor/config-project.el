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
    (unless (and value
                 (not (eq value 'none))
                 (file-exists-p (project-root value)))
      (setq value (or
                   (when ymacs-default-project
                     (if (file-remote-p -directory)
                         (cons (car ymacs-default-project)
                               (tramp-make-tramp-file-name
                                (tramp-dissect-file-name -directory)
                                (cdr ymacs-default-project)))
                       ymacs-default-project))
                   (project-try-vc -directory)
                   (when-let (root (locate-dominating-file -directory ".project"))
                     (cons 'local root))
                   ;; default is 'none
                   'none))
      (puthash key value ymacs-editor-project-cache))
    (and (not (eq value 'none))
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
      ("U" . citre-update-this-tags-file)
      ("C" . citre-edit-tags-file-recipe)
      ("i" . ymacs-editor/invalid-project-cache)))

  (eval-when! (executable-find "fzf")
    (when (boundp 'project-prefix-map)
      (define-key! :map project-prefix-map
        ("f" . ymacs-editor/fzf)))
    (setcar (assoc 'project-find-file project-switch-commands) #'ymacs-editor/fzf))

  (setq project-find-functions '(ymacs-editor//project)))
