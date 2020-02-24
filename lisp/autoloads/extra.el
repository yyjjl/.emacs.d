;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun extra/clipboard-copy (-beg -end)
  (interactive "r")
  (if (display-graphic-p)
      (kill-new (buffer-substring-no-properties -beg -end))
    (if emacs-use-xsel-p
        (if (= 0 (shell-command-on-region -beg -end "xsel -ib"))
            (message "Copy finished")
          (message "Error occured !!!"))
      (message "Executable `xsel' not found !!!"))))

;;;###autoload
(defun extra/clipboard-paste ()
  (interactive)
  (if (display-graphic-p)
      (yank 1)
    (if emacs-use-xsel-p
        (shell-command "xsel -ob" t)
      (message "Executable `xsel' not found !!!"))))

(defvar extra-translate-shell-repo "https://github.com/soimort/translate-shell")
(defvar extra-translate-shell-path
  (eval-when-compile (expand-var! "translate-shell")))
(defvar extra-translate-shell-args
  (eval-when-compile (split-string "-I -s zh -t en -e google" " ")))

;;;###autoload
(defun extra/translate-shell ()
  (interactive)
  (let ((exe (expand-file-name "build/trans" extra-translate-shell-path))
        (string (when (region-active-p)
                  (buffer-substring (region-beginning) (region-end)))))
    (cond
     ((not (file-exists-p exe))
      (message "Executable `%s' not found !" exe))
     ((not (file-executable-p exe))
      (message "`%s' can't be executed" exe))
     (t
      (let* ((buffer (get-buffer-create "*translate-shell*"))
             (proc (get-buffer-process buffer)))
        (if (equal (current-buffer) buffer)
            (quit-window)
          (when (not (and proc
                          (process-live-p proc)
                          (eq (buffer-local-value 'major-mode buffer) 'comint-mode)))
            (with-current-buffer buffer
              (let ((buffer-read-only nil))
                (erase-buffer))
              (comint-exec buffer "trans" exe nil extra-translate-shell-args)
              (comint-mode)
              (setq proc (get-buffer-process buffer))))
          (with-current-buffer (pop-to-buffer buffer)
            (comint-send-string proc (concat string "\n")))))))))

;;;###autoload
(defun extra/create-or-update-trans ()
  (interactive)
  (if (file-directory-p extra-translate-shell-path)
      (let ((default-directory extra-translate-shell-path))
        (compilation-start "git pull && git submodule update && make"))
    (compilation-start (format "git clone  %s %s && cd %s && make"
                               extra-translate-shell-repo
                               extra-translate-shell-path
                               extra-translate-shell-path))))
