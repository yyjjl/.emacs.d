(defun core%semantic--clean-tag ($tag)
  (let ((def-val (semantic-tag-get-attribute $tag :default-value)))
    (when (stringp def-val)
      (when-let ((pos (string-match "\n" def-val)))
        (setq def-val (concat (substring def-val 0 (min pos 80)) " ... ")))
      (semantic-tag-put-attribute $tag :default-value def-val))))

(defun core/semantic--create ($tags $depth &optional $class $result)
  "Write the contents of TAGS to the current buffer."
  (let ((class $class)
        cur-type
        formated-tag)
    (dolist (tag $tags)
      (when (listp tag)
        (core%semantic--clean-tag tag)
        (setq formated-tag
              (propertize (semantic-format-tag-summarize tag nil t) 'semantic-tag tag))
        (case (setq cur-type (semantic-tag-class tag))
          ((function variable type)
           (let ((spaces (make-string (* $depth 2) ?\s))
                 (type-p (eq cur-type 'type)))
             (unless (and (> $depth 0) (not type-p))
               (setq class nil))
             ;; Format tag
             (push (concat (if (and class (not type-p))
                               (format "%s%s(%s) "
                                       spaces (if (> $depth 0) "=> " "") class)
                             spaces)
                           formated-tag)
                   $result)
             (when type-p
               (setq class (car tag)))
             ;; Recurse to children
             (unless (eq cur-type 'function)
               (setq $result
                     (core/semantic--create (semantic-tag-components tag)
                                            (1+ $depth)
                                            class
                                            $result)))))
          ;; Catch-all
          (t (push formated-tag $result))))))
  $result)

(defun counsel-semantic-or-imenu ()
  "Jump to a semantic tag in the current buffer."
  (interactive)
  (unless
      (and (semantic-active-p)
           (ignore-errors
             (when (semantic-parse-tree-needs-update-p)
               (semantic-parse-tree-set-needs-update))
             (ivy-read
              "tag: " (nreverse (core/semantic--create (semantic-fetch-tags) 0))
              :preselect (thing-at-point 'symbol)
              :require-match t
              :action
              (lambda (candidate)
                (let* ((pos (if (string-prefix-p " " candidate)
                                (next-single-property-change
                                 0 'semantic-tag candidate (length candidate))
                              0))
                       (tag (get-text-property pos 'semantic-tag candidate)))
                  (semantic-go-to-tag tag)))
              :caller 'counsel-semantic-or-imenu
              :keymap (define-key! :map (make-sparse-keymap)
                        ("C-n" . ivy-next-line-and-call)
                        ("C-p" . ivy-previous-line-and-call)))))
    (call-interactively #'counsel-imenu)))

(defun counsel-kill-buffer (&optional $arg)
  "Kill buffer with ivy backends."
  (interactive "P")
  (let ((ivy-use-virtual-buffers nil))
    (ivy-read (format "Kill buffer (default %s): " (buffer-name))
              'internal-complete-buffer
              :preselect (buffer-name (current-buffer))
              :action (if $arg
                          (lambda () (let ((kill-buffer-hook nil))
                                       (kill-buffer)))
                        #'kill-buffer)
              :keymap counsel-find-file-map
              :caller 'counsel-kill-buffer)))

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
     (buffer (setq right-str
                   (propertize (format "%s"
                                       (buffer-local-value 'major-mode
                                                           buffer))
                               'face 'warning)))
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
                    internal-complete-buffer))
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

  (define-key!
    :map ivy-minibuffer-map
    ("C-j" . ivy-immediate-done)
    ("C-M-j" . ivy-done)))

(defun swiper/dispatch (&optional $arg)
  (interactive "p")
  (call-interactively
   (cond
    ((eq $arg 4) #'swiper-multi)
    ((eq $arg 16) #'swiper-all)
    (t #'counsel-grep-or-swiper))))

(with-eval-after-load 'counsel
  (define-key!
    ("C-x j j" . counsel-bookmark)
    ("C-s" . swiper/dispatch)
    ("C-x C-f" . counsel-find-file)
    ("C-x k" . counsel-kill-buffer)
    ("C-x w -" . ivy-pop-view)
    ("C-x w =" . ivy-push-view)
    ("C-M-i" . counsel-company))

  (define-key! :prefix "C-c i"
    ("r" . ivy-resume)
    ("l l" . counsel-load-library)
    ("l t" . counsel-load-theme)
    ("l p" . counsel-list-processes)
    ("l f" . counsel-find-library)
    ("u" . counsel-unicode-char)
    ("i" . counsel-semantic-or-imenu)
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
    ("o" . counsel-outline)
    ("t" . counsel-tmm))

  (add-to-list 'counsel-linux-apps-directories "~/.local/share/applications")
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

(provide 'core-ivy)
