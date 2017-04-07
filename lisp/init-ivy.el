(with-eval-after-load 'ivy
  (require 's)
  (defun my-ivy--regex-fuzzy (str)
    "remove space from str"
    (if (stringp str)
        (progn
          (if (s-ends-with-p " " str)
              (setq str (concat str "//")))
          (setq str (s-replace " " "" str))))
    (ivy--regex-fuzzy str))

  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          ;; fuzzy make ivy so slow
          (counsel-unicode-char . ivy--regex-plus)
          ;; fuzzy search doesn't perform well
          (counsel-descbinds . ivy--regex-plus)
          (t . my-ivy--regex-fuzzy)))
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-use-virtual-buffers t)

  (bind-keys :map ivy-minibuffer-map
             ("C-o" . ivy-immediate-done)
             ("C-n" . ivy-next-line)
             ("C-p" . ivy-previous-line)
             ("C-b" . backward-char)
             ("C-f" . forward-char)
             ("C-SPC" . set-mark-command)))

(defun counsel-kill-buffer (&optional arg)
  (interactive "P")
  (ivy-read (format  "Kill buffer (default %s) :" (buffer-name))
            (loop for buffer in  (mapcar #'buffer-name (buffer-list))
                  if (not (string= (substring buffer 0 1) " "))
                  collect buffer)
            :action #'kill-buffer
            :keymap counsel-find-file-map
            :caller 'counsel-kill-buffer))

  (defun counsel-sudo-edit (&optional arg)
    "Edit currently visited file as root.
With a prefix ARG prompt for a file to visit.
Will also prompt for a file to visit if current
buffer is not visiting a file."
    (interactive "P")
    (if (or arg (not buffer-file-name))
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
      (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

  (with-eval-after-load 'counsel
    (setq counsel-find-file-at-point t)
    (setq counsel-find-file-ignore-regexp
          (concat
           ;; file names beginning with # or .
           "\\(?:\\`[#.]\\)"
           ;; file names ending with # or ~
           "\\|\\(?:[#~]\\'\\)"))
    (defadvice counsel-bookmark (before make-sure-bookmark-loaded activate)
      (unless (boundp 'bookmark-maybe-load-default-file)
        (require 'bookmark+))
      (bookmark-maybe-load-default-file))

    (ivy-set-actions
     'counsel-find-file
     `(("x"
        (lambda (x) (delete-file (expand-file-name x ivy--directory)))
        ,(propertize "delete" 'face 'font-lock-warning-face))))
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    (bind-keys ("C-s" . counsel-grep-or-swiper)
               ("C-M-s" .swiper-the-thing)
               ("C-c i r" . ivy-resume)
               ("C-x C-f" . counsel-find-file)
               ("C-x k" . counsel-kill-buffer)
               ("C-c i l l" . counsel-load-library)
               ("C-c i l t" . counsel-load-theme)
               ("C-c i u" . counsel-unicode-char)
               ("C-c i i" . counsel-imenu)
               ("C-c i l p" . counsel-list-processes)
               ("C-c i R" . counsel-linux-app)
               ("C-c i v" . counsel-set-variable)
               ("C-c i j" . counsel-file-jump)

               ("C-c i g t" . counsel-git)
               ("C-c i g g " . counsel-git-grep)
               ("C-c i g r" . counsel-git-grep-recenter)
               ("C-c i g q" . counsel-git-grep-query-replace)
               ("C-c i g s" . counsel-git-stash)

               ("C-c i h" . counsel-shell-command-history)
               ("C-c i m" . counsel-tmm)
               ("C-c i a" . counsel-ag)
               ("C-c i /" . counsel-grep)
               ("C-c i s" . counsel-semantic)
               ("C-c i L" . counsel-locate)
               ("C-c i d b" . counsel-descbinds)
               ("C-c i d f" . counsel-describe-face)
               ("C-c i b" . counsel-bookmark)
               ("C-c i f s" . counsel-find-symbol)
               ("C-c i f f" . counsel-faces)
               ("C-c i c w" . counsel-colors-web)
               ("C-c i c e" . counsel-colors-emacs)
               ("C-c i e" . counsel-sudo-edit)
               ("C-c i o" . counsel-outline)))

  (defun swiper-the-thing ()
    (interactive)
    (swiper (if (region-active-p)
                (buffer-substring-no-properties (region-beginning) (region-end))
              (thing-at-point 'symbol))))



  (provide 'init-ivy)
