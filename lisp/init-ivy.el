(defun counsel-kill-buffer (&optional arg)
  "Kill buffer with ivy backends."
  (interactive "P")
  (ivy-read (format  "Kill buffer (default %s) :" (buffer-name))
            'internal-complete-buffer
            :preselect (buffer-name (current-buffer))
            :action (if arg
                        (lambda () (let ((kill-buffer-hook nil))
                                 (kill-buffer)))
                      #'kill-buffer)
            :keymap counsel-find-file-map
            :caller 'counsel-kill-buffer))

(defun counsel-sudo-edit (&optional arg)
  "Edit currently visited file as root.
With a prefix ARG prompt for a file to visit.  Will also prompt
for a file to visit if current buffer is not visiting a file."
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
    (find-file (concat "/sudo:root@localhost:" buffer-file-name))))

(with-eval-after-load 'ivy
  (defun main|ivy--regex-fuzzy (str)
    "Remove space from STR"
    (ivy--regex-fuzzy (if (stringp str)
                          (replace-regexp-in-string " " "" str)
                        str)))

  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-re-builders-alist
        '((swiper . ivy--regex-plus)
          ;; fuzzy make ivy so slow
          (counsel-unicode-char . ivy--regex-plus)
          (counsel-imenu . ivy--regex-plus)
          ;; fuzzy search doesn't perform well
          (counsel-descbinds . ivy--regex-plus)
          (t . main|ivy--regex-fuzzy)))
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-use-virtual-buffers t)

  (define-keys :map ivy-minibuffer-map
    ("C-o" . ivy-immediate-done)
    ("C-n" . ivy-next-line)
    ("C-p" . ivy-previous-line)
    ("C-b" . backward-char)
    ("C-f" . forward-char)
    ("C-SPC" . set-mark-command)))

(with-eval-after-load 'counsel
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
  (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
  (define-keys
    ("C-s" . counsel-grep-or-swiper)
    ("C-x C-f" . counsel-find-file)
    ("C-x k" . counsel-kill-buffer))
  (define-keys :prefix "C-c i"
    ("r" . ivy-resume)
    ("l l" . counsel-load-library)
    ("l t" . counsel-load-theme)
    ("u" . counsel-unicode-char)
    ("i" . counsel-imenu)
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
    ("b" . counsel-bookmark)
    ("f s" . counsel-find-symbol)
    ("f f" . counsel-faces)
    ("c w" . counsel-colors-web)
    ("c e" . counsel-colors-emacs)
    ("e" . counsel-sudo-edit)
    ("o" . counsel-outline)))

(provide 'init-ivy)
