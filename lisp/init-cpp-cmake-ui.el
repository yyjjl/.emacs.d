;; -*- lexical-binding:t -*-
(require 'widget)

(eval-when-compile
  (require 'wid-edit)
  (require 'init-cpp-cmake))

(defvar cpp-cmake--widgets nil)
(defvar cpp-cmake--save-button nil)

(defun cpp-cmake//render-buttons ($buffer)
  ;; Apply changes
  (widget-create
   'push-button
   :notify
   (lambda (&rest _)
     (with-current-buffer $buffer
       (when (member cpp-cmake-current-config-name
                     '("Release" "Debug"))
         (error "`Rlease' or `Debug' can not be delete")))
     (when (yes-or-no-p "Are you sure to delete?")
       (with-current-buffer $buffer
         (setq cpp-cmake-config-list
               (delq (cpp-cmake//current-config) cpp-cmake-config-list))
         (setq cpp-cmake-current-config-name "Release")
         (cpp-cmake//save-variables 'cpp-cmake-current-config-name
                                   'cpp-cmake-config-list))
       (cpp-cmake//render-config-buffer $buffer)
       (message "You may need run cmake manually.")))
   "Delete")
  (widget-insert " ")
  (widget-create 'push-button
                 :notify
                 (lambda (&rest _)
                   (cpp-cmake//render-config-buffer $buffer))
                 "Reset")
  (widget-insert " ")
  (setq cpp-cmake--save-button
        (widget-create
         'push-button
         :notify
         (lambda (&rest _)
           (dolist (widget cpp-cmake--widgets)
             (widget-apply widget :apply-change))
           (with-current-buffer $buffer
             (cpp-cmake//save-variables 'cpp-cmake-current-config-name
                                       'cpp-cmake-config-list))
           (message "Changes Applied"))
         "Apply")))

(defun cpp-cmake//render-root-widget ($buffer)
  (let* ((root (or (buffer-local-value 'cpp-cmake-project-root $buffer) ""))
         (widget (widget-create 'directory :tag "Project Root" root)))
    (widget-put widget :apply-change
                (lambda (widget)
                  (with-current-buffer $buffer
                    (let ((value  (widget-value widget)))
                      (setq cpp-cmake-project-root (unless (string= value "")
                                                     value))))))
    (push widget cpp-cmake--widgets)))

(defun cpp-cmake//render-name-widget ($buffer)
  (let* ((choice-list (with-current-buffer $buffer
                        `(,@(mapcar (lambda (config)
                                      `(item :format "%t"
                                             :tag ,(car config)
                                             :value ,(car config)))
                                    cpp-cmake-config-list)
                          (editable-field :menu-tag "Create" "New Config"))))
         (widget
          (apply #'widget-create 'choice
                 :tag "Configuration"
                 :format "%{%t%}: %[Select%]: %v"
                 :value (buffer-local-value
                         'cpp-cmake-current-config-name
                         $buffer)
                 :notify
                 (lambda (widget &rest _)
                   (with-current-buffer $buffer
                     (setq cpp-cmake-current-config-name
                           (widget-value widget)))
                   (cpp-cmake//render-config-buffer $buffer)
                   (message "Change to %s" (widget-value widget)))
                 choice-list)))
    (widget-put widget :apply-change
                (lambda (_widget)
                  (with-current-buffer $buffer
                    (cpp-cmake//new-config cpp-cmake-current-config-name))))
    (push widget cpp-cmake--widgets)))

(defun cpp-cmake//render-build-widget ($buffer)
  (let* ((build (or (with-current-buffer $buffer
                      (cpp-cmake//config-build))
                    ""))
         (widget (widget-create 'directory :tag "Build Directory" build)))
    (widget-put widget :apply-change
                (lambda (widget)
                  (with-current-buffer $buffer
                    (cpp-cmake//set-config-slot 'build (widget-value widget)))))
    (push widget cpp-cmake--widgets)))

(defun cpp-cmake//render-env-widget ($buffer)
  (widget-insert "Environment Variable (Need manually quote for shell):\n")
  (let* ((env (with-current-buffer $buffer
                (cpp-cmake//config-env)))
         (widget (widget-create 'editable-list
                                :entry-format "%d %v"
                                :value env
                                '(editable-field "NAME=VALUE"))))
    (widget-put widget :apply-change
                (lambda (widget)
                  (with-current-buffer $buffer
                    (cpp-cmake//set-config-slot 'env (widget-value widget)))))
    (push widget cpp-cmake--widgets)))

(defun cpp-cmake//render-options-widget ($buffer)
  (widget-insert "Current Options ")
  (widget-insert "(Press `enter' to confirm in text fields):\n")
  (let* ((options (with-current-buffer $buffer
                    (cpp-cmake//config-options)))
         (widget (widget-create 'editable-list
                                :entry-format "%d %v"
                                :value (mapcar (lambda (x)
                                                 (concat (car x) "=" (cdr x)))
                                               options)
                                `(editable-field :value "NAME=VALUE"))))
    (widget-put widget :apply-change
                (lambda (widget)
                  (with-current-buffer $buffer
                    (let (options)
                      (dolist (option (widget-value widget))
                        (setq option (split-string option "="))
                        (if (< (length option) 2)
                            (error "Invalid Option %s" option)
                          (push (cons (car option)
                                      (string-join (cdr option)))
                                options)))
                      (cpp-cmake//set-config-slot 'options (nreverse options))))))
    (push widget cpp-cmake--widgets)

    (widget-insert "\n")
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
      (buffer-local-value 'cpp-cmake-available-options $buffer)))))

(defun cpp-cmake//render-config-buffer ($buffer)
  (kill-all-local-variables)
  (set (make-local-variable 'cpp-cmake--widgets) nil)
  (set (make-local-variable 'cpp-cmake--save-button) nil)
  (let ((inhibit-read-only t)
        (old-point (point)))
    (erase-buffer)
    (remove-overlays)

    (widget-insert "CMake Configuartion. ")
    (widget-insert "Press `C-x C-s' or push `[Apply]' to apply changes. \n\n")
    (cpp-cmake//render-buttons $buffer)
    (widget-insert (propertize "\n\nProject Settings:\n\n"
                               'face font-lock-doc-face))
    (cpp-cmake//render-root-widget $buffer)
    (widget-insert "\n")
    (cpp-cmake//render-name-widget $buffer)
    (widget-insert (propertize "\n\nConfiguration Settings:\n\n"
                               'face font-lock-doc-face))
    (cpp-cmake//render-build-widget $buffer)
    (widget-insert "\n")
    (cpp-cmake//render-env-widget $buffer)
    (widget-insert "\n")
    (cpp-cmake//render-options-widget $buffer)

    (setq cpp-cmake--widgets (nreverse cpp-cmake--widgets))
    (use-local-map (copy-keymap widget-keymap))
    (widget-setup)
    (local-set-key (kbd "C-x C-s")
                   (lambda! (widget-apply cpp-cmake--save-button :notify)))
    (goto-char (min old-point (point-max))))
  (special-mode))

(provide 'init-cpp-cmake-ui)
