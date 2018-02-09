(defun counsel%semantic--clean-tag ($tag)
  (let ((def-val (semantic-tag-get-attribute $tag :default-value)))
    (when (stringp def-val)
      (when-let* ((pos (string-match "\n" def-val)))
        (setq def-val (concat (substring def-val 0 (min pos 80)) " ... ")))
      (semantic-tag-put-attribute $tag :default-value def-val))))

(defun counsel%semantic-or-imenu--relative-buffers ($buffer)
  (let* ((projectile-require-project-root nil)
         (project-buffers (ignore-errors (projectile-project-buffers))))
    (loop for buffer in (buffer-list)
          when (or (eq $buffer buffer)
                   (and (buffer-file-name buffer)
                        (or (eq (buffer-local-value 'major-mode buffer)
                                (buffer-local-value 'major-mode $buffer))
                            (member buffer project-buffers))))
          collect buffer)))

(defun counsel%semantic-or-imenu--candidates ($buffers)
  (loop for buffer in $buffers
        nconc
        (mapcar
         (lambda ($candidate)
           (if (null (cdr-safe $buffers))
               $candidate
             (cons (concat (buffer-name buffer) ": " (car $candidate))
                   (cdr $candidate))))
         (with-current-buffer buffer
           ;; Use semantic first
           (if (semantic-active-p)
               (mapcar (lambda (x)
                         (counsel%semantic--clean-tag x)
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

(defun counsel%semantic-or-imenu--goto ($candidate)
  (let ((place (cdr $candidate))
        buffer goto-function)
    (if (semantic-tag-p place)
        (setq buffer (semantic-tag-buffer place)
              goto-function 'semantic-go-to-tag)
      (setq goto-function 'imenu
            buffer (let ((pos (cdr-safe place)))
                     (if (overlayp pos)
                         (overlay-buffer pos)
                       (marker-buffer pos)))))
    (switch-to-buffer buffer)
    (funcall goto-function place)))


(defun counsel-semantic-or-imenu* ($arg)
  "Jump to a semantic tag in the current buffer."
  (interactive "P")
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (let* ((buffer (current-buffer))
         (buffers (if $arg
                      (counsel%semantic-or-imenu--relative-buffers buffer)
                    (list buffer)))
         (candidates (counsel%semantic-or-imenu--candidates buffers)))
    (ivy-read (if $arg "All Imenu Items: " "Imenu Items: ")
              candidates
              :preselect (thing-at-point 'symbol)
              :require-match t
              :action 'counsel%semantic-or-imenu--goto
              :caller 'counsel-semantic-or-imenu
              ;; If search for all buffers, do not jump when selecting
              ;; candidate
              :keymap (if $arg
                          counsel-imenu-map
                        (define-key! :map (make-sparse-keymap)
                          ("C-n" . ivy-next-line-and-call)
                          ("C-p" . ivy-previous-line-and-call))))))

(defvar counsel--kill-buffers nil)
(defun counsel%kill-buffer-action (x)
  (if (member x counsel--kill-buffers)
      (unless (memq this-command '(ivy-done
                                   ivy-alt-done
                                   ivy-immediate-done))
        (setq counsel--kill-buffers (delete x counsel--kill-buffers)))
    (unless (equal x "")
      (setq counsel--kill-buffers (append counsel--kill-buffers (list x)))))
  (let ((prompt (counsel%kill-buffer-prompt)))
    (setf (ivy-state-prompt ivy-last) prompt)
    (setq ivy--prompt (concat ivy-count-format prompt))))

(defun counsel%kill-buffers (&optional $hard)
  (dolist (buffer counsel--kill-buffers)
    (let ((kill-buffer-hook (and (not $hard) kill-buffer-hook)))
      (kill-buffer buffer))))

(defun counsel%kill-buffer-prompt ()
  (format "%sKill buffers (%s): "
          (if current-prefix-arg "[hard] " "")
          (string-join counsel--kill-buffers ", ")))

(defun counsel-kill-buffer ($hard)
  "Kill buffer with ivy backends."
  (interactive "P")
  (setq counsel--kill-buffers nil)
  (let ((ivy-use-virtual-buffers nil))
    (ivy-read (counsel%kill-buffer-prompt)
              'internal-complete-buffer
              :preselect (buffer-name (current-buffer))
              :action #'counsel%kill-buffer-action
              :keymap counsel-find-file-map
              :caller 'counsel-kill-buffer
              :require-match t))
  (counsel%kill-buffers $hard))

(defun counsel-projectile-kill-buffer ($hard)
  "Kill buffer with ivy backends."
  (interactive "P")
  (setq counsel--kill-buffers nil)
  (ivy-read (counsel%kill-buffer-prompt)
            (mapcar #'buffer-name (projectile-project-buffers))
            :preselect (buffer-name (current-buffer))
            :action #'counsel%kill-buffer-action
            :keymap counsel-find-file-map
            :caller 'counsel-projectile-kill-buffer
            :require-match t)
  (counsel%kill-buffers $hard))

(defun counsel-sudo-edit (&optional $arg)
  "Edit currently visited file as root.
With a prefix ARG prompt for a file to visit.  Will also prompt
for a file to visit if current buffer is not visiting a file."
  (interactive "P")
  (if (or $arg (not buffer-file-name))
      (ivy-read "Find file(as sudo): :" 'read-file-name-internal
                :matcher #'counsel--find-file-matcher
                :initial-input default-directory
                :action
                (lambda (x)
                  (with-ivy-window
                    (find-file (concat "/sudo:root@localhost:"
                                       (expand-file-name x ivy--directory)))))
                :keymap counsel-find-file-map
                :caller 'counsel-sudo-edit)
    (find-file (concat "/sudo:root@localhost:" buffer-file-name))))

(defun core%ivy-switch-buffer-transformer ($left-str)
  "Transform STR to more readable format."
  (let ((buffer (get-buffer $left-str))
        (right-str ""))
    (cond
     (buffer
      (setq right-str (abbreviate-file-name
                       (buffer-local-value 'default-directory buffer))))
     ((and (eq ivy-virtual-abbreviate 'full)
           (file-name-directory $left-str))
      (setq right-str (abbreviate-file-name
                       (file-name-directory $left-str)))
      (setq $left-str (propertize (file-name-nondirectory $left-str)
                                  'face 'ivy-virtual))))
    (format-line! $left-str right-str)))

(defun core%counsel-bookmark-transformer ($left-str)
  "Transform STR to more readable format."
  (let ((right-str "")
        (bm (bookmark-get-bookmark-record $left-str)))
    (when bm
      (setq right-str
            (concat (file-name-nondirectory (cdr (assoc 'filename bm)))
                    (propertize (format " [%d]"
                                        (cdr (assoc 'position bm)))
                                'face 'warning))))
    (format-line! (propertize $left-str 'face 'font-lock-string-face)
                  right-str)))

(with-eval-after-load 'ivy
  (dolist (caller '(ivy-switch-buffer
                    counsel-kill-buffer
                    internal-complete-buffer
                    counsel-projectile-switch-to-buffer
                    counsel-projectile))
    (ivy-set-display-transformer caller
                                 #'core%ivy-switch-buffer-transformer))

  (ivy-set-display-transformer 'counsel-bookmark
                               #'core%counsel-bookmark-transformer)

  (require 'ivy-hydra)

  (setq ivy-dispatching-done-columns 3)
  (setq ivy-count-format "(%d/%d) ")

  (setq ivy-re-builders-alist
        '( ;; Use regex as default
          ;; (swiper . core/re-builder-pinyin)
          ;; (swiper-multi . core/re-builder-pinyin)
          ;; (counsel-find-file . core/re-builder-pinyin)
          (t . ivy--regex-plus)))
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-virtual-abbreviate 'full)
  (setq ivy-use-selectable-prompt t)

  (define-hook! ivy|occur-mode-setup (ivy-occur-mode-hook)
    (toggle-truncate-lines 1))

  (define-key!
    :map ivy-minibuffer-map
    ("C-j" . ivy-immediate-done)
    ("C-M-j" . ivy-done)))

(defun swiper/dispatch (&optional $arg)
  (interactive "P")
  (call-interactively
   (cond
    ((equal $arg 0) #'isearch-forward-regexp)
    ((equal $arg 9) #'isearch-backward-regexp)
    ((equal $arg '(4)) #'swiper-multi)
    ((equal $arg '(16)) #'swiper-all)
    (t #'counsel-grep-or-swiper))))

(with-eval-after-load 'counsel
  (define-key!
    ("C-x j j" . counsel-bookmark)
    ("C-s" . swiper/dispatch)
    ("C-x C-f" . counsel-find-file)
    ("C-x k" . counsel-kill-buffer)
    ("C-x w -" . ivy-pop-view)
    ("C-x w =" . ivy-push-view))

  (define-key! :prefix "C-c i"
    ("r" . ivy-resume)
    ("l l" . counsel-load-library)
    ("l t" . counsel-load-theme)
    ("l p" . counsel-list-processes)
    ("l f" . counsel-find-library)
    ("u" . counsel-unicode-char)
    ("i" . counsel-semantic-or-imenu*)
    ("x" . counsel-linux-app)
    ("v" . counsel-set-variable)
    ("j" . counsel-file-jump)
    ("g t" . counsel-git)
    ("g l" . counsel-git-log)
    ("g g" . counsel-git-grep)
    ("g q" . counsel-git-grep-query-replace)
    ("g s" . counsel-git-stash)
    ("h" . counsel-minibuffer-history)
    ("m" . counsel-mark-ring)
    ("a" . counsel-ag)
    ("/" . counsel-grep)
    ("L" . counsel-locate)
    ("f" . counsel-describe-face)
    ("S" . counsel-find-symbol)
    ("F" . counsel-faces)
    ("W" . counsel-colors-web)
    ("E" . counsel-colors-emacs)
    ("e" . counsel-sudo-edit)
    ("O" . counsel-outline)
    ("o" . counsel-org-goto-all)
    ("t" . counsel-tmm))

  (setq counsel-yank-pop-separator
        "\n------------------------------------------------------------\n")
  (setq counsel-find-file-at-point t)
  (setq counsel-find-file-ignore-regexp
        (concat
         ;; file names beginning with # or .
         "\\(?:\\`[#]\\)"
         ;; file names ending with # or ~
         "\\|\\(?:[#~]\\'\\)"))

  (ivy-set-actions
   'counsel-find-file
   `(("x"
      (lambda (x) (delete-file (expand-file-name x ivy--directory)))
      ,(propertize "delete" 'face 'font-lock-warning-face))))
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history))

(with-eval-after-load 'counsel-projectile
  (define-key! :map projectile-command-map
    ("p" . counsel-projectile)
    ("k" . counsel-projectile-kill-buffer)
    ("K" . projectile-kill-buffers)
    ("w" . projectile-switch-project)))

(provide 'core-ivy)
