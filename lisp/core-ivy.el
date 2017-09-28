;; (autoload 'pinyinlib-build-regexp-char "pinyinlib" nil nil)
;; (defun core/pinyin-regexp-helper (char)
;;   (cond ((equal char ?\s) ".*")
;;         (t (or (pinyinlib-build-regexp-char char) ""))))

;; (defun core%pinyinlib-build-regexp-string (str)
;;   (when (and (> (length str) 0)
;;              (equal (substring str 0 1) "?"))
;;     (mapconcat 'core/pinyin-regexp-helper
;;                (cdr (string-to-list str))
;;                "")))

;; (defun core/re-builder-pinyin (str)
;;   (or (core%pinyinlib-build-regexp-string str)
;;       (ivy--regex-plus str)))

(defun core/semantic--create ($tags $depth &optional $class $result)
  "Write the contents of TAGS to the current buffer."
  (let ((class $class)
        (stylefn #'semantic-format-tag-summarize)
        cur-type)
    (dolist (tag $tags)
      (when (listp tag)
        (case (setq cur-type (semantic-tag-class tag))
          ((function variable type)
           (let ((spaces (make-string (* $depth 2) ?\s))
                 (type-p (eq cur-type 'type)))
             (unless (and (> $depth 0) (not type-p))
               (setq class nil))
             (push (concat
                    (if (and class (not type-p))
                        (format "%s%s(%s) "
                                spaces (if (> $depth 0) "=> " "") class)
                      spaces)
                    ;; Save the tag for later
                    (propertize (funcall stylefn tag nil t)
                                'semantic-tag tag))
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
          (t
           (push (propertize (funcall stylefn tag nil t) 'semantic-tag tag)
                 $result))))))
  $result)

(defun counsel-semantic-or-imenu ()
  "Jump to a semantic tag in the current buffer."
  (interactive)
  (if (semantic-active-p)
      (progn
        (when (semantic-parse-tree-needs-update-p)
          (semantic-parse-tree-set-needs-update))
        (ivy-read
         "tag: " (nreverse (core/semantic--create (semantic-fetch-tags) 0))
         :preselect (thing-at-point 'symbol)
         :require-match t
         :action
         (lambda (candidate)
           (let* ((pos (if (string-prefix-p " " candidate)
                           (next-single-property-change 0
                                                        'semantic-tag candidate
                                                        (length candidate))
                         0))
                  (tag (get-text-property pos
                                          'semantic-tag
                                          candidate)))
             (semantic-go-to-tag tag)))
         :caller 'counsel-semantic-or-imenu))
    (call-interactively #'counsel-imenu)))

(defun counsel-kill-buffer (&optional $arg)
  "Kill buffer with ivy backends."
  (interactive "P")
  (let ((ivy-use-virtual-buffers nil))
    (ivy-read (format "Kill buffer (default %s) :" (buffer-name))
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

(with-eval-after-load 'ivy
  (defun core%ivy-switch-buffer-transformer (str)
    "Transform STR to more readable format."
    (let ((buf (get-buffer str))
          (width (frame-width))
          (right-side ""))
      (cond
       (buf (with-current-buffer buf
              (setq right-side
                    (propertize (format "%s" major-mode)
                                'face 'warning))))
       ((and (eq ivy-virtual-abbreviate 'full)
             (file-name-directory str))
        (setq right-side (abbreviate-file-name (file-name-directory str)))
        (setq str (propertize (file-name-nondirectory str)
                              'face 'ivy-virtual))))
      (concat str
              (make-string (max 2 (- width (+ (length str)
                                              (length right-side))))
                           ?\s)
              right-side)))
  (defun core%counsel-bookmark-transformer (x)
    "Transform STR to more readable format."
    (let ((width (frame-width))
          (right-side "")
          (bm (bookmark-get-bookmark-record x)))
      (when bm
        (setq right-side
              (concat (file-name-nondirectory (cdr (assoc 'filename bm)))
                      (propertize (format " [%d]" (cdr (assoc 'position bm)))
                                  'face 'warning))))
      (concat (propertize x 'face 'font-lock-string-face)
              (make-string (max 2 (- width (+ (length x)
                                              (length right-side))))
                           ?\s)
              right-side)))

  (dolist (caller '(ivy-switch-buffer
                    counsel-kill-buffer
                    internal-complete-buffer))
    (ivy-set-display-transformer caller #'core%ivy-switch-buffer-transformer))

  (ivy-set-display-transformer 'counsel-bookmark
                               #'core%counsel-bookmark-transformer)
  (defun core/ivy-external-file ()
    (interactive)
    (setq ivy--all-candidates
          (ivy--filter core-external-file-regexp ivy--all-candidates))
    (setq ivy--all-candidates
          (ivy--filter ivy-text ivy--all-candidates)))

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
    ("C-M-j" . ivy-done)
    ("!" . core/ivy-external-file)))

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
    ("C-x w =" . ivy-push-view))
  (define-key! :prefix "C-c i"
    ("r" . ivy-resume)
    ("l" . counsel-load-library)
    ("t" . counsel-load-theme)
    ("p" . counsel-list-processes)
    ("u" . counsel-unicode-char)
    ("i" . counsel-semantic-or-imenu)
    ("x" . counsel-linux-app)
    ("v" . counsel-set-variable)
    ("j" . counsel-file-jump)
    ("g t" . counsel-git)
    ("g l" . counsel-git-log)
    ("g g " . counsel-git-grep)
    ("g q" . counsel-git-grep-query-replace)
    ("g s" . counsel-git-stash)
    ("h" . counsel-shell-command-history)
    ("m" . counsel-tmm)
    ("a" . counsel-ag)
    ("/" . counsel-grep)
    ("s" . counsel-semantic)
    ("L" . counsel-locate)
    ("d f" . counsel-describe-face)
    ("f s" . counsel-find-symbol)
    ("f f" . counsel-faces)
    ("f l" . counsel-find-library)
    ("c w" . counsel-colors-web)
    ("c e" . counsel-colors-emacs)
    ("e" . counsel-sudo-edit)
    ("o" . counsel-outline))

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
