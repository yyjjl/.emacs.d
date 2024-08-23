;;; -*- lexical-binding: t; -*-

(defvar ymacs-editor-project-cache (make-hash-table :test #'equal))
(defvar ymacs-editor-project-invalidate-cache-empty-vars
  '((ymacs-modeline--buffer-file-name . nil)
    (ymacs-modeline--vcs-state . nil)
    (ymacs-modeline--project-detected-p . nil)
    (ymacs-modeline--project-root . nil)))

(defmacro ymacs-editor//define-project-local (-method &rest -args)
  (let ((call-args (cl-remove-if (lambda (x) (string-prefix-p "&" (symbol-name x))) -args)))
    `(cl-defmethod ,-method ((project (head local)) ,@-args)
       (if-let (vc-project (nth 2 project))
           (,-method vc-project ,@call-args)
         (,-method (cons 'transient (nth 1 project)) ,@call-args)))))

;; Declare directories with ".project" as a project
(cl-defmethod project-root ((project (head local)))
  (nth 1 project))

(cl-defmethod project-files ((project (head local)) &optional dirs)
  (if-let ((root (project-root project))
           (vc-project (nth 2 project)))
      (project-files vc-project
                     ;; 当请求 local root 的时候, 切换到 vc-root
                     (if (and (= (length dirs) 1)
                              (file-equal-p root (car dirs)))
                         nil
                       dirs))
    (project-files (cons 'transient root) dirs)))

(cl-defmethod ymacs-modeline//project-identity ((project (head local)))
  (if-let ((vc-project (nth 2 project))
           (root (project-root project)))
      (format "local:%s:vc:%s"
              (project-name (cons 'transient root))
              (project-root vc-project))
    (format "local:%s" (project-name project))))

(ymacs-editor//define-project-local project-external-roots)
(ymacs-editor//define-project-local project-ignores dir)
(ymacs-editor//define-project-local project-buffers)
(ymacs-editor//define-project-local project-name)

(defun ymacs-editor//project (-directory)
  "Find current project"
  (let* ((key (expand-file-name -directory))
         (value (gethash key ymacs-editor-project-cache)))
    (when (not (eq value 'none))
      (when (not (and value (file-exists-p (project-root value))))
        (let ((vc-project (project-try-vc -directory))
              (local-root (locate-dominating-file -directory ".project")))
          (setq value
                (cond
                 ((and vc-project local-root)
                  (if (string-prefix-p (file-truename (project-root vc-project))
                                       (file-truename local-root))
                      ;; .project 子目录
                      (list 'local local-root vc-project)
                    vc-project))
                 (vc-project vc-project)
                 (local-root (list 'local local-root nil))
                 ;; default is 'none
                 (t 'none))))
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
      ("i" . ymacs-editor/invalid-project-cache)))

  (when (boundp 'project-prefix-map)
    (define-key! :map project-prefix-map
      ("f" . ymacs-editor/find-file)))
  (setcar (assoc 'project-find-file project-switch-commands) #'ymacs-editor/find-file)

  (setq project-vc-extra-root-markers nil)
  (setq project-find-functions '(ymacs-editor//project)))
