;; Improve `dired-mode'
(require-packages! 
 dired+
 dired-narrow)



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
      (open! file-list))))

(with-eval-after-load 'dired
  (define-hook! dired|setup (dired-mode-hook)
    (setq mode-line-buffer-identification
          '("%b" (dired-omit-mode " (omit)")))
    (dired-hide-details-mode 1))

  (setq dired-dwim-target t)
  ;; search file name only when focus is over file
  (setq dired-isearch-filenames 'dwim)
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always)
  (setq wdired-allow-to-change-permissions t)

  (require 'dired-x)
  (require 'dired+)
  (setq dired-omit-files "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\..*$"))

(with-eval-after-load 'dired+
  (diredp-toggle-find-file-reuse-dir 1)
  (define-key! :map dired-mode-map
    (")" . dired-omit-mode)
    ("E" . dired/open-externally)
    ("/" . dired-narrow-fuzzy)
    ("\\" . dired-narrow)
    ("M-p" . dired-prev-subdir)
    ("M-n" . dired-next-subdir)
    (";" . dired-kill-subdir)))


(provide 'init-dired)
