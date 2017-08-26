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
  (define-key!
    :map ivy-minibuffer-map
    ("C-j" . ivy-immediate-done)
    ("C-M-j" . ivy-done))
  (require 'ivy-hydra)
  (setq ivy-dispatching-done-columns 3)

  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-re-builders-alist
        '( ;; Use regex as default
          (t . ivy--regex-plus)))
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-use-virtual-buffers t))

(with-eval-after-load 'counsel
  (define-key! ("C-s" . counsel-grep-or-swiper)
    ("C-x C-f" . counsel-find-file)
    ("C-x k" . counsel-kill-buffer)
    ("C-c w -" . ivy-pop-view)
    ("C-c w =" . ivy-push-view))
  (define-key! :prefix "C-c i"
    ("r" . ivy-resume)
    ("l l" . counsel-load-library)
    ("l t" . counsel-load-theme)
    ("u" . counsel-unicode-char)
    ("i" . counsel-semantic-or-imenu)
    ("l p" . counsel-list-processes)
    ("R" . counsel-linux-app)
    ("v" . counsel-set-variable)
    ("j" . counsel-file-jump)
    ("g t" . counsel-git)
    ("g g " . counsel-git-grep)
    ("g r" . counsel-git-grep-recenter)
    ("g q" . counsel-git-grep-query-replace)
    ("g s" . counsel-git-stash)
    ("h" . counsel-shell-command-history)
    ("m" . counsel-tmm)
    ("a" . counsel-ag)
    ("/" . counsel-grep)
    ("s" . counsel-semantic)
    ("L" . counsel-locate)
    ("d b" . counsel-descbinds)
    ("d f" . counsel-describe-face)
    ("f s" . counsel-find-symbol)
    ("f f" . counsel-faces)
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
