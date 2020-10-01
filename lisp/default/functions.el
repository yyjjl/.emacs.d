;; -*- lexical-binding: t; -*-

(defsubst ymacs//save-variable (-symbol -file)
  (let ((real-file (expand-var! -file))
        (val (default-value -symbol)))
    (with-temp-buffer
      (insert (format "(setq-default %s " -symbol)
              (if (consp val) "'" "")
              (prin1-to-string (default-value -symbol))
              ")")
      (write-region (point-min) (point-max) real-file))))

(defsubst ymacs//load-variable (_symbol -file)
  (let ((real-file (expand-var! -file)))
    (when (file-exists-p real-file)
      (load real-file :noerror))))

;;* Handle External Files
(defun ymacs//external-file-handler (_op &rest -args)
  (let ((file (expand-file-name (car -args))))
    (cond ((eq system-type 'darwin)
           (shell-command (concat "open " (shell-quote-argument file))))
          ((eq system-type 'gnu/linux)
           (let ((process-connection-type nil))
             (recentf-push file)
             (start-process "external-process" nil "xdg-open" file))))
    (kill-buffer)
    (let (debug-on-error)
      (user-error "Opened %s in external program" (file-name-nondirectory file)))))

(defun ymacs//buffer-predicate (buffer)
  (or (buffer-file-name buffer)
      (not (string-prefix-p "*" (buffer-name buffer)))
      (equal (buffer-name buffer) "*scratch*")))

(defun ymacs//show-process-memory (&optional pid)
  (->
   (format "ps up %s" (or pid (emacs-pid)))
   shell-command-to-string
   (split-string "\n" :omit-nulls)
   (elt 1)
   split-string
   (elt 3)
   string-to-number
   ignore-errors))
