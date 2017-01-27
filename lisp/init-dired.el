(defun diredext-exec-git-command-in-shell (command &optional arg file-list)
  "Run a shell command `git COMMAND`' on the marked files.
if no files marked, always operate on current line in dired-mode
"
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg)))
     (list
      ;; Want to give feedback whether this file or marked files are used:
      (dired-read-shell-command "git command on %s: " current-prefix-arg files)
      current-prefix-arg
      files)))
  (unless (string-match "[*?][ \t]*\\'" command)
    (setq command (concat command " *")))
  (setq command (concat "git " command))
  (dired-do-shell-command command arg file-list)
  (message command))

(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook '(lambda ()
                                (dired-filter-mode 1)
                                (dired-filter-group-mode 1)
                                (turn-on-stripe-buffer-mode)
                                (stripe-listify-buffer)))
  (setq dired-dwim-target t)
  ;; search file name only when focus is over file
  (setq dired-isearch-filenames 'dwim)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)

  (add-hook 'dired-initial-position-hook 'dired-k)
  (add-hook 'dired-after-readin-hook #'dired-k-no-revert))



(with-eval-after-load 'dired-filter
  (set-face-attribute 'dired-filter-group-header nil
                      :foreground "Orange")
  (setq dired-filter-group-saved-groups
        '(("default"
           ("Document"
            (extension  "pdf" "txt" "doc" "xls" "ppt" "pptx" "ps" "djvu"
                        "docx" "xlsx"))
           ("Org"
            (extension . "org"))
           ("Configuration"
            (regexp . "\\.[a-zA-Z\\-]*\\(rc\\|config\\)\\'"))
           ("Image"
            (extension "svg" "svgz" "jpg" "jpeg" "gif" "tiff" "png" "ico"
                       "bmp"))
           ("Archives"
            (extension "zip" "rar" "gz" "bz2" "tar" "jar" "7z"))
           ("Directory"
            (directory)))))

  (define-key dired-mode-map "[" dired-filter-map)
  (define-key dired-mode-map "]" dired-filter-mark-map)
  (bind-keys :map dired-mode-map
             (";" . avy-goto-subword-1)
             ("\\" . diredext-exec-git-command-in-shell)
             ("/" . swiper)
             ("M-n" . dired-next-subdir)
             ("M-p" . dired-prev-subdir)
             ("C-M-n" . nil)
             ("C-M-p" . nil)))

(with-eval-after-load 'dired+
  (diredp-toggle-find-file-reuse-dir 1))


(provide 'init-dired)

