;;; -*- lexical-binding: t; -*-

(require 'widget)

(eval-when-compile
  (require 'wid-edit))

(defvar ymacs-cpp-cmake--widgets nil)
(defvar ymacs-cpp-cmake--apply-button nil)

(defun ymacs-cpp-cmake//render-buttons (-buffer)
  ;; Apply changes
  (widget-create
   'push-button
   :notify
   (lambda (&rest _)
     (with-current-buffer -buffer
       (when (member
              ymacs-cpp-cmake-current-config-name
              '("Release" "Debug"))
         (user-error
          "`Rlease' or `Debug' can not be delete")))
     (when (yes-or-no-p "Are you sure to delete?")
       (with-current-buffer -buffer
         (setq ymacs-cpp-cmake-config-list
               (delq (ymacs-cpp-cmake//current-config) ymacs-cpp-cmake-config-list))
         (setq ymacs-cpp-cmake-current-config-name "Release")
         (save-dir-local-variables! 'ymacs-cpp-cmake-current-config-name
                                    'ymacs-cpp-cmake-config-list))
       (ymacs-cpp-cmake//render-config-buffer -buffer)
       (message "You may need run cmake manually.")))
   "Delete")

  (widget-insert " ")

  (widget-create
   'push-button
   :notify
   (lambda (&rest _)
     (ymacs-cpp-cmake//render-config-buffer -buffer))
   "Reset")

  (widget-insert " ")

  (setq
   ymacs-cpp-cmake--apply-button
   (widget-create
    'push-button
    :notify
    (lambda (&rest _)
      (dolist (widget ymacs-cpp-cmake--widgets)
        (widget-apply widget :apply-change))
      (with-current-buffer -buffer
        (save-dir-local-variables! 'ymacs-cpp-cmake-current-config-name
                                   'ymacs-cpp-cmake-config-list))
      (message "Changes Applied"))
    "Apply")))

(defun ymacs-cpp-cmake//render-root-widget (-buffer)
  (let* ((root (buffer-local-value 'ymacs-cpp-cmake-project-root -buffer))
         (widget (widget-create
                  'directory
                  :tag "Project Root"
                  (or root ""))))
    (widget-put
     widget :apply-change
     (lambda (widget)
       (with-current-buffer -buffer
         (let ((value (widget-value widget)))
           (setq ymacs-cpp-cmake-project-root
                 (unless (string-empty-p value) value))))))

    (push widget ymacs-cpp-cmake--widgets)))

(defun ymacs-cpp-cmake//render-name-widget (-buffer)
  (let* ((choice-list
          (with-current-buffer -buffer
            `(,@(mapcar (lambda (config)
                          `(item :format "%t"
                                 :tag ,(car config)
                                 :value ,(car config)))
                        ymacs-cpp-cmake-config-list)
              (editable-field :menu-tag "Create" "New Config"))))
         (widget
          (apply
           #'widget-create 'choice
           :tag "Configuration"
           :format "%{%t%}: %[Select%]: %v"
           :value (buffer-local-value
                   'ymacs-cpp-cmake-current-config-name
                   -buffer)
           :notify
           (lambda (widget &rest _)
             (with-current-buffer -buffer
               (setq ymacs-cpp-cmake-current-config-name
                     (widget-value widget)))
             (ymacs-cpp-cmake//render-config-buffer -buffer)
             (message "Change to %s" (widget-value widget)))
           choice-list)))

    (widget-put
     widget :apply-change
     (lambda (_widget)
       (with-current-buffer -buffer
         (ymacs-cpp-cmake//new-config ymacs-cpp-cmake-current-config-name))))

    (push widget ymacs-cpp-cmake--widgets)))

(defun ymacs-cpp-cmake//render-build-widget (-buffer)
  (let* ((build (with-current-buffer -buffer
                  (ymacs-cpp-cmake//config-build)))
         (widget (widget-create
                  'directory
                  :tag "Build Directory"
                  (or build ""))))
    (widget-put
     widget :apply-change
     (lambda (widget)
       (with-current-buffer -buffer
         (ymacs-cpp-cmake//set-config-slot 'build (widget-value widget)))))

    (push widget ymacs-cpp-cmake--widgets)))

(defun ymacs-cpp-cmake//render-env-widget (-buffer)
  (widget-insert "Environment Variable (Need manually quote for shell):\n")

  (let* ((env (with-current-buffer -buffer
                (ymacs-cpp-cmake//config-env)))
         (widget (widget-create
                  'editable-list
                  :entry-format "%d %v"
                  :value env
                  '(editable-field "NAME=VALUE"))))
    (widget-put
     widget :apply-change
     (lambda (widget)
       (with-current-buffer -buffer
         (ymacs-cpp-cmake//set-config-slot 'env (widget-value widget)))))

    (push widget ymacs-cpp-cmake--widgets)))

(defun ymacs-cpp-cmake//render-options-widget (-buffer)
  (widget-insert "Current Options ")
  (widget-insert "(Press `enter' to confirm in text fields):\n")
  (let* ((options (with-current-buffer -buffer
                    (ymacs-cpp-cmake//config-options)))
         (widget
          (widget-create
           'editable-list
           :entry-format "%d %v"
           :value (mapcar (lambda (x)
                            (concat (car x) "=" (cdr x)))
                          options)
           `(editable-field :value "NAME=VALUE"))))

    (widget-put
     widget :apply-change
     (lambda (widget)
       (with-current-buffer -buffer
         (let (options)
           (dolist (option (widget-value widget))
             (setq option (split-string option "="))
             (if (< (length option) 2)
                 (user-error "Invalid Option %s" option)
               (push (cons (car option)
                           (string-join (cdr option)))
                     options)))
           (ymacs-cpp-cmake//set-config-slot 'options (nreverse options))))))

    (push widget ymacs-cpp-cmake--widgets)

    (widget-insert "\n")

    (let ((available-options
           (buffer-local-value 'ymacs-cpp-cmake-available-options -buffer)))
      (widget-insert "(run `C-u M-x cpp/run-cmake' to get available options)\n")

      (if (not available-options)
          (widget-insert "No Available Options ")
        (apply
         #'widget-create 'list
         :tag "Available Options (Click to add)"
         (mapcar
          (lambda (ap)
            (let* ((name (nth 0 ap))
                   (type (nth 1 ap))
                   (value (nth 2 ap)))
              `(push-button
                :format ,(concat "%[%v%]\n(" type ")=" value "\n")
                :notify
                ,(lambda (&rest _)
                   (widget-put widget :args
                               `((editable-field :value
                                   ,(concat name "=" value))))
                   (widget-editable-list-insert-before widget nil))
                :value ,name)))
          available-options))))))

(defun ymacs-cpp-cmake//render-config-buffer (-buffer)
  (kill-all-local-variables)

  (setq-local ymacs-cpp-cmake--widgets nil)
  (setq-local ymacs-cpp-cmake--apply-button nil)

  (let ((inhibit-read-only t)
        (old-point (point)))
    (erase-buffer)
    (remove-overlays)

    (widget-insert "CMake Configuartion. ")
    (widget-insert "Press `C-x C-s' or push `[Apply]' to apply changes. \n\n")
    (ymacs-cpp-cmake//render-buttons -buffer)
    (widget-insert (propertize "\n\nProject Settings:\n\n" 'face font-lock-doc-face))
    (ymacs-cpp-cmake//render-root-widget -buffer)
    (widget-insert "\n")
    (ymacs-cpp-cmake//render-name-widget -buffer)
    (widget-insert (propertize "\n\nConfiguration Settings:\n\n" 'face font-lock-doc-face))
    (ymacs-cpp-cmake//render-build-widget -buffer)
    (widget-insert "\n")
    (ymacs-cpp-cmake//render-env-widget -buffer)
    (widget-insert "\n")
    (ymacs-cpp-cmake//render-options-widget -buffer)

    (setq ymacs-cpp-cmake--widgets (nreverse ymacs-cpp-cmake--widgets))

    (use-local-map (copy-keymap widget-keymap))

    (widget-setup)

    (local-set-key (kbd "C-x C-s") (lambda! (widget-apply ymacs-cpp-cmake--apply-button :notify)))
    (local-set-key (kbd "q") (lambda! (kill-this-buffer)))

    (goto-char (min old-point (point-max)))))

;;;###autoload
(defun ymacs-cpp-cmake/config ()
  (interactive)
  (let ((original-buffer (current-buffer))
        (buffer (get-buffer-create "*ymacs-cpp-cmake-config*")))
    (with-current-buffer (pop-to-buffer buffer)
      (ymacs-cpp-cmake//render-config-buffer original-buffer))))
