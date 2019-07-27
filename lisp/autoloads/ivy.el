;;; -*- lexical-binding: t; -*-

(defun counsel//truncate-string (-string -width)
  (when (> (length -string) -width)
    (setq -string
          (concat (substring
                   (replace-regexp-in-string "\n" "\\\\n" -string)
                   0 -width)
                  " ... ")))
  -string)

(defun counsel//semantic--clean-tag (-tag)
  (let ((default-value (semantic-tag-get-attribute -tag :default-value))
        (name (semantic-tag-name -tag)))
    (when (stringp default-value)
      (semantic-tag-put-attribute -tag :default-value
                                  (counsel//truncate-string default-value 75)))
    (when (stringp name)
      (semantic-tag-set-name -tag (counsel//truncate-string name 75)))))

(defun counsel//semantic-or-imenu--relative-buffers (-buffer)
  (let* ((projectile-require-project-root nil)
         (project-buffers (ignore-errors (projectile-project-buffers))))
    (cl-loop for buffer in (buffer-list)
          when (or (eq -buffer buffer)
                   (and (buffer-file-name buffer)
                        (or (eq (buffer-local-value 'major-mode buffer)
                                (buffer-local-value 'major-mode -buffer))
                            (member buffer project-buffers))))
          collect buffer)))

(defun counsel//semantic-or-imenu--candidates (-buffers)
  (cl-loop for buffer in -buffers
        nconc
        (mapcar
         (lambda (-candidate)
           (if (null (cdr-safe -buffers))
               -candidate
             (cons (concat (buffer-name buffer) ": " (car -candidate))
                   (cdr -candidate))))
         (with-current-buffer buffer
           ;; Use semantic first
           (if (and (semantic-active-p)
                    (not (bound-and-true-p lsp-mode))) ;; use imenu when lsp-mode is enabled
               (mapcar (lambda (x)
                         (counsel//semantic--clean-tag x)
                         (cons (counsel-semantic-format-tag x) x))
                       (counsel-semantic-tags))
             (let* ((inhibit-message t)
                    (imenu-auto-rescan t)
                    (imenu-auto-rescan-maxout (if current-prefix-arg
                                                  (buffer-size)
                                                imenu-auto-rescan-maxout))
                    (items (ignore-errors (imenu--make-index-alist :noerror)))
                    (items (delete (assoc "*Rescan*" items) items)))
               (counsel-imenu-get-candidates-from items)))))))

(defun counsel//semantic-or-imenu--goto (-candidate)
  (let ((place (cdr -candidate))
        buffer goto-function)
    (if (semantic-tag-p place)
        (setq buffer (semantic-tag-buffer place)
              goto-function 'semantic-go-to-tag)
      (setq goto-function 'imenu
            buffer (let ((pos (cdr-safe place)))
                     (cond ((overlayp pos) (overlay-buffer pos))
                           ((markerp pos) (marker-buffer pos))
                           (t (current-buffer))))))
    (when buffer
      (switch-to-buffer buffer))
    (funcall goto-function place)))

;;;###autoload
(defun counsel/semantic-or-imenu* (-arg)
  "Jump to a semantic tag in the current buffer."
  (interactive "P")
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (let* ((buffer (current-buffer))
         (buffers (if -arg
                      (counsel//semantic-or-imenu--relative-buffers buffer)
                    (list buffer)))
         (candidates (counsel//semantic-or-imenu--candidates buffers)))
    (ivy-read (if -arg "All Imenu Items: " "Imenu Items: ")
              candidates
              :preselect (thing-at-point 'symbol)
              :require-match t
              :action 'counsel//semantic-or-imenu--goto
              :caller 'counsel-semantic-or-imenu
              ;; If search for all buffers, do not jump when selecting
              ;; candidate
              :keymap (if -arg
                          counsel-imenu-map
                        (define-key! :map (make-sparse-keymap)
                          ("C-n" . ivy-next-line-and-call)
                          ("C-p" . ivy-previous-line-and-call))))))

(defvar counsel--kill-buffers nil)
(defun counsel//kill-buffer-action (x)
  (if (member x counsel--kill-buffers)
      (unless (memq this-command '(ivy-done
                                   ivy-alt-done
                                   ivy-immediate-done))
        (setq counsel--kill-buffers (delete x counsel--kill-buffers)))
    (unless (equal x "")
      (setq counsel--kill-buffers (append counsel--kill-buffers (list x)))))
  (let ((prompt (counsel//kill-buffer-prompt)))
    (setf (ivy-state-prompt ivy-last) prompt)
    (setq ivy--prompt (concat ivy-count-format prompt))))

(defun counsel//kill-buffer-prompt (&optional -hard)
  (format "%sKill buffers (%s): "
          (if -hard "[hard] " "")
          (string-join counsel--kill-buffers ", ")))

;;;###autoload
(defun counsel/kill-buffer (-arg)
  "Kill buffer with ivy backends."
  (interactive "P")
  (setq counsel--kill-buffers nil)
  (let ((ivy-use-virtual-buffers nil))
    (ivy-read (format "%s%sKill buffers (%s): "
                      (if (eq -arg '(16)) "<hard> " "")
                      (if -arg (concat "[" (projectile-project-name) "] ") "")
                      (string-join counsel--kill-buffers ", "))
              (if -arg
                  (mapcar #'buffer-name (projectile-project-buffers))
                'internal-complete-buffer)
              :preselect (buffer-name (current-buffer))
              :action #'counsel//kill-buffer-action
              :keymap ivy-switch-buffer-map
              :caller 'counsel/kill-buffer
              :require-match t))
  (dolist (buffer counsel--kill-buffers)
    (kill-buffer buffer)))

(defun counsel//sudo-edit-file (filename &optional old-point)
  (let ((buffer (find-file
                 (if-let ((remote (file-remote-p filename)))
                     (format "%s|sudo:%s:%s"
                             (substring remote 0 (1- (length remote)))
                             (file-remote-p filename 'host)
                             (file-remote-p filename 'localname))
                   (concat "/sudo:root@localhost:" filename)))))
    (when (and (buffer-live-p buffer) old-point)
      (with-current-buffer buffer
        (goto-char old-point)))))

;;;###autoload
(defun counsel/sudo-edit (&optional -arg)
  "Edit currently visited file as root.
With a prefix ARG prompt for a file to visit.  Will also prompt
for a file to visit if current buffer is not visiting a file."
  (interactive "P")
  (if (or -arg
          (and (not buffer-file-name)
               (not (eq major-mode 'dired-mode))))
      (ivy-read "Find file(as sudo): :" 'read-file-name-internal
                :matcher #'counsel--find-file-matcher
                :initial-input default-directory
                :action
                (lambda (x)
                  (with-ivy-window
                    (counsel//sudo-edit-file (expand-file-name x ivy--directory))))
                :keymap counsel-find-file-map
                :caller 'counsel/sudo-edit)
    (counsel//sudo-edit-file (or buffer-file-name
                                 (expand-file-name default-directory))
                             (point))))

;;;###autoload
(defun swiper/dispatch (&optional -arg)
  (interactive "P")
  (call-interactively
   (cond
    ((equal -arg 0) #'isearch-forward-regexp)
    ((equal -arg 9) #'isearch-backward-regexp)
    ((equal -arg '(4)) #'swiper-isearch)
    ((equal -arg '(16)) #'swiper-multi)
    ((equal -arg '(64)) #'swiper-all)
    (t #'counsel-grep-or-swiper))))

;;;###autoload
(defun ivy-occur-filter-lines ()
  (interactive)
  (unless (string-prefix-p "ivy-occur" (symbol-name major-mode))
    (error "Current buffer is not in ivy-occur mode"))
  (let ((inhibit-read-only t)
        (regexp (read-regexp "Regexp(! for flush)"))
        (start (next-single-property-change (point-min) 'font-lock-face)))
    (funcall (if (string-prefix-p "!" regexp)
                 #'flush-lines
               #'keep-lines)
             (substring regexp 1) start (point-max) t)
    (save-excursion
      (goto-char (point-min))
      (forward-line 1)
      (if (looking-at "stack> ")
          (forward-char 7)
        (insert "stack> "))
      (insert (format "[%s] " regexp)))))

;;;###autoload
(defun ivy-occur-undo ()
  (interactive)
  (let ((inhibit-read-only t))
    (if (save-excursion
          (goto-char (point-min))
          (forward-line 1)
          (looking-at "stack> "))
        (undo)
      (error "Filter stack is empty"))))

;;;###autoload
(defun counsel/rg (&optional -arg)
  (interactive "p")
  (cond ((= -arg 16)
         (counsel-rg nil
                     (read-directory-name "rg in directory: "
                                          (or (projectile-project-root)
                                              default-directory))
                     (read-from-minibuffer "rg args: " "--no-ignore")
                     "rg no ignore: "))
        (t (call-interactively 'counsel-rg))))
