;;; -*- lexical-binding: t; -*-

(autoload #'project-root "project")
(autoload #'project-current "project")

;; Declare directories with ".project" as a project
(cl-defmethod project-root ((project (head local)))
  (cdr project))

(defun ymacs-default//project (-directory)
  "Find current project"
  (or ymacs-default-project
      (unless (file-remote-p -directory)
        (let* ((key (expand-file-name -directory))
               (value (gethash key ymacs-default-project-cache)))
          (unless (and value (file-exists-p (project-root value)))
            (setq value (or (project-try-vc -directory)
                            (when-let (root (locate-dominating-file -directory ".project"))
                              (cons 'local root))))
            (puthash key value ymacs-default-project-cache))
          value))))

(defsubst ymacs-default//project-root (&optional -directory)
  (when-let (project (project-current nil -directory))
    (project-root project)))

(defsubst ymacs-default//project-root-or-default ()
  (or (when-let (project (project-current))
        (project-root project))
      default-directory))

(after! project
  (define-key! :map project-prefix-map
    ("E" . ymacs-default/edit-dir-locals))

  (setq project-find-functions '(ymacs-default//project)))
