;; Improve `dired-mode'
(require! 'dired+)
(require! 'all-the-icons-dired)



(defun dired/open-externally ()
  "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference."
  (interactive)
  (let* ((file-list (if (eq major-mode 'dired-mode)
                        (dired-get-marked-files)
                      (list (buffer-file-name))))
         (do-it-p (or (<= (length file-list) 5)
                      (y-or-n-p "Open more than 5 files? "))))
    (when do-it-p
      (cond
       ((eq system-type 'darwin)
        (mapc (lambda (path)
                (shell-command (concat "open " (shell-quote-argument path))))
              file-list))
       ((eq system-type 'gnu/linux)
        (mapc (lambda (path) (let ((process-connection-type nil))
                               (start-process "" nil "xdg-open" path)))
              file-list))))))

(with-eval-after-load 'dired
  (define-hook! dired|setup (dired-mode-hook)
    (setq mode-line-buffer-identification
          '("%b" (dired-omit-mode " (omit)")))
    (all-the-icons-dired-mode 1)
    (dired-hide-details-mode 1))

  (setq dired-dwim-target t)
  ;; search file name only when focus is over file
  (setq dired-isearch-filenames 'dwim)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq wdired-allow-to-change-permissions t)

  (define-key! :map dired-mode-map
    (")" . dired-omit-mode)
    ("E" . dired/open-externally))

  (require 'dired-x)
  (require 'dired+)
  (setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..*$"))

(with-eval-after-load 'dired+
  (diredp-toggle-find-file-reuse-dir 1))


(provide 'init-dired)
