(defvar emacs-config-directory
  (expand-file-name "lisp" user-emacs-directory)
  "All configuration in this directory")

(defvar emacs-var-direcotry
  (expand-file-name "~/.emacs.d/var/i3/")
  "All data and external executable file in this direcotry")

;; Add `emacs-config-directory' to `load-path'
(add-to-list 'load-path emacs-config-directory)

;; All packages required in this section are defined in `init-packages'
(require 'core-lib)
;; Set some important variables
(require 'core-vars)
(require 'core-defaults)
;; Load core packages

(require 'package)

(when (>= emacs-major-version 25)
  ;; Do not save to init.el
  (fset 'package--save-selected-packages
        (lambda ($value)
          (when $value (setq package-selected-packages $value)))))

(fset 'require! 'ignore)

(package-initialize)

(require 'core-ivy)
(require 'core-misc)
(require 'core-ui)

(eval-when-compile
  (require 'cl))
(require 'dash)
(require 'json)

(recentf-mode 1)
(session-initialize)
(winner-mode 1)
(ivy-mode 1)
(counsel-mode 1)
;; global-modes
(global-subword-mode 1)
(show-paren-mode 1)
;; Auto insert closing pair
(electric-pair-mode 1)
(electric-layout-mode 1)
(electric-indent-mode 1)

(with-current-buffer (get-buffer-create "*scratch*")
  (setq cursor-type nil)
  (erase-buffer)
  (insert (concat ";; Welcome to Emacs " (or user-login-name "") " !!!"))
  (special-mode))

(setq-default mode-line-format nil)
(setq-default window-min-height 1)
(setq-default default-frame-alist
              `((name . "Minibuffer")
                (width . 80)
                (height . ,(+ 4 ivy-height))
                (menu-bar-lines . 0)
                (tool-bar-lines . 0)
                (vertical-scroll-bars . nil)
                (unsplittable . t)))

(defun i3-msg%command (&rest $args)
  (let ((result (with-output-to-string
                  (with-current-buffer standard-output
                    (apply #'call-process "i3-msg" nil t nil $args))))
        (json-array-type 'list)
        (json-object-type 'hash-table)
        (json-false nil))
    (condition-case err
        (json-read-from-string result)
      (error (error result)))))

(defun i3-msg%workspaces (&optional $property)
  (--filter (or (eq $property t)
                (gethash $property it))
            (i3-msg%command "-t" "get_workspaces")))

(defun i3-msg%get-workspaces-from-tree ($tree $names &optional $window-list)
  (let ((name (gethash "name" $tree)))
    (if (member name $names)
        (push (cons name $tree) $window-list)
      (dolist (node (gethash "nodes" $tree))
        (setq $window-list
              (i3-msg%get-workspaces-from-tree node $names $window-list)))))
  $window-list)

(defun i3-msg%windows-of-tree ($tree &optional $window-list)
  (-if-let (nodes (gethash "nodes" $tree))
      (dolist (node nodes)
        (setq $window-list (i3-msg%windows-of-tree node $window-list)))
    (unless (or (equal "dockarea" (gethash "layout" $tree))
                (string-prefix-p "i3bar for output"
                                 (gethash "name" $tree ""))
                (not (gethash "window" $tree)))
      (push $tree $window-list)))
  $window-list)

(defun i3-msg%windows (&optional $workspace-property)
  (let ((names (mapcar
                (lambda (ws) (gethash "name" ws))
                (i3-msg%workspaces $workspace-property)))
        (tree (i3-msg%command "-t" "get_tree")))
    (cl-loop for (name . tree) in
             (i3-msg%get-workspaces-from-tree tree names)
             nconc
             (cl-loop for window in (i3-msg%windows-of-tree tree)
                      collect (cons name window)))))

(defun i3-msg%focus-window ($window)
  (i3-msg%command (format "[id=%s]" (gethash "window" $window)) "focus"))

(defun i3%window-candidates (&optional $workspace-property)
  (cl-loop for (name . window) in (i3-msg%windows $workspace-property)
           for properies = (gethash "window_properties" window)
           for trimed-name = (string-join (cdr (split-string name ":")) ":")
           collect (cons
                    (concat (propertize trimed-name
                                        'face font-lock-builtin-face)
                            " "
                            (propertize (gethash "class" properies "Unknown")
                                        'face font-lock-constant-face)
                            " "
                            (gethash "title" properies ""))
                    window)))

(defmacro i3%run-command (&rest body)
  `(unwind-protect
       (progn ,@body)
     (save-buffers-kill-terminal t)))

(defvar i3-document-regexp
  (concat (regexp-opt '(".pdf" ".djvu" ".ps" ".dvi"))
          "\\'"))
(defvar i3-document-directories '("~/documents/books"
                                  "~/documents/paper"
                                  "~/working/"
                                  "~/project/org/src/"
                                  "~/desktop"))

(defun i3%index-document-files ()
  (let ((case-fold-search t))
    (dolist (directory i3-document-directories)
      (dolist (file (directory-files-recursively directory
                                                 i3-document-regexp))
        (recentf-push file))))
  (recentf-save-list)
  (message "Indexing done"))

(define-hook! i3%do-index-files (midnight-hook)
  (i3%index-document-files))


(i3%do-index-files)



(defun i3/goto-window ()
  (interactive)
  (ivy-read "Goto Window: " (i3%window-candidates t)
            :require-match t
            :action (lambda (x)
                      (i3-msg%focus-window (cdr x)))))

(defun i3/open-document ()
  (interactive)
  (ivy-read "Open: " (mapcar #'substring-no-properties recentf-list)
            :action (lambda (file)
                      (with-ivy-window
                        (recentf-push file)
                        (call-process "gtk-launch" nil nil nil
                                      "evince.desktop"
                                      file)))
            :caller 'i3/open-document))

(defun i3%open-document-transformer ($file)
  "Transform STR to more readable format."
  (concat
   (propertize (file-name-nondirectory $file)
               'face font-lock-string-face)
   " "
   (propertize (abbreviate-file-name (file-name-directory $file))
               'face font-lock-doc-face)))

(ivy-set-display-transformer 'i3/open-document
                             #'i3%open-document-transformer)

(provide 'init-i3)
