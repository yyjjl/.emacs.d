;; -*- lexical-binding: t; -*-

(cl-defun ymacs//completing-read
    (-prompt -collection -action
     &key
       ((:return-action -return-action))
       ((:return-prompt -return-prompt))
       ((:history -history)))
  (if (or (not (listp -collection))
          (> (length -collection) 10))
      (funcall -action (completing-read -prompt -collection nil t nil -history))

    (let ((map (make-sparse-keymap)))

      (when -return-action
        (define-key map (kbd "RET") (lambda! (funcall -return-action))))

      (--map-indexed
       (define-key map (kbd (format "%d" it-index))
         (lambda!
             (funcall -action it)))
       -collection)

      (lv-message
       "%s\n%s%s"
       -prompt
       (if -return-prompt
           (concat "[RET] => " -return-prompt "\n")
         "")
       (string-join
        (--map-indexed
         (format "[%d] => %s" it-index (or (car-safe it) it))
         -collection)
        "\n"))

      (set-transient-map map nil #'lv-delete-window))))

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
  (let ((file (expand-file-name (car -args)))
        (process-connection-type nil))
    (recentf-push file)
    (start-process "external-process" nil "evince" file)
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
