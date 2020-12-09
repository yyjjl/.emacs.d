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
    (let ((key (unwind-protect
                    (read-key)
                 (lv-delete-window))))
      (cond
        ((and (>= key ?0) (< key ?9))
         (funcall -action (nth (- key ?0) -collection)))

        ((and -return-action (equal key 13))
         (funcall -return-action))

        (t (error "No action for key `%s'" (key-description `[,key])))))))

(cl-defun ymacs//completing-read-simple
    (-prompt -collection
     &key
       ((:return-value -return-value) 'unset)
       ((:return-prompt -return-prompt))
       ((:history -history)))
  (if (or (not (listp -collection))
          (> (length -collection) 10))
      (let ((key (completing-read -prompt -collection nil t nil -history)))
        (if (consp (car -collection))
            (assoc-string key -collection)
          key))
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
    (let ((key (unwind-protect
                    (read-key)
                 (lv-delete-window))))
      (cond
        ((and (>= key ?0) (< key ?9))
         (nth (- key ?0) -collection))

        ((and (not (eq -return-value 'unset)) (equal key 13))
         -return-value)

        ((equal key 7)
         (signal 'quit nil))

        (t (error "No action for key `%s'" (key-description `[,key])))))))

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
    (open! file)
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

(defun ymacs//next-error-find-buffer (&rest -args)
  (or (let ((buffers
             (cl-loop
              for window in (window-list)
              for buffer = (window-buffer window)
              if (and (next-error-buffer-p buffer)
                      (with-current-buffer buffer
                        (apply 'derived-mode-p
                               ymacs-auto-next-error-buffer-derived-modes)))
              collect buffer)))
        (when (= (length buffers) 1)
          (car buffers)))

      (apply #'next-error-buffer-unnavigated-current -args)

      (let ((error-buffer (buffer-local-value 'next-error-buffer (current-buffer))))
        (when (and (buffer-live-p error-buffer)
                   (apply #'next-error-buffer-p error-buffer -args))
          error-buffer))))
