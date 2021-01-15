;;; -*- lexical-binding: t; -*-

(defsubst ymacs-default//filter-ring (-ring)
  (--filter
   (< (length it) 1024)
   (mapcar #'substring-no-properties -ring)))

(defun ymacs-default//external-file-handler (_op &rest -args)
  (let ((file (expand-file-name (car -args)))
        (process-connection-type nil))
    (recentf-push file)
    (kill-buffer)
    (if (fboundp #'counsel-find-file-extern)
        (progn
          (counsel-find-file-extern file)
          (message "Opened %s externally" file))
      (message "Don't know how to open %s" file))))

(defun ymacs-default//buffer-predicate (buffer)
  (or (buffer-file-name buffer)
      (not (string-prefix-p "*" (buffer-name buffer)))
      (equal (buffer-name buffer) "*scratch*")))

(defun ymacs-default//buffer-has-long-lines-p ()
  ;; @see Doom Emacs #2183: `so-long-detected-long-line-p' tries to parse
  ;;      comment syntax, but in some buffers comment state isn't
  ;;      initialized, leading to a wrong-type-argument: stringp error.
  (let ((so-long-skip-leading-comments
         (bound-and-true-p comment-use-syntax)))
    (so-long-detected-long-line-p)))

(defun ymacs-default//bookmark-setup ()
  (unless (file-remote-p default-directory)
    ;; Setup default bookmark
    (setq bookmark-current-bookmark
          (ignore-errors
            (cl-loop for (name . record) in bookmark-alist
                     when (equal (file-truename (buffer-file-name))
                                 (file-truename (bookmark-get-filename name)))
                     return name)))))

(defun ymacs-default//get-error-buffer ()
  (let ((buffers
         (cl-loop
          for window in (window-list)
          for buffer = (window-buffer window)
          if (and (next-error-buffer-p buffer)
                  (with-current-buffer buffer
                    (apply 'derived-mode-p
                           ymacs-default-next-error-buffer-modes)))
          collect buffer)))
    (when (= (length buffers) 1)
      (car buffers))))

(defun ymacs-default//next-error-find-buffer (&rest -args)
  (or (ymacs-default//get-error-buffer)

      (apply #'next-error-buffer-unnavigated-current -args)

      (let ((error-buffer (buffer-local-value 'next-error-buffer (current-buffer))))
        (when (and (buffer-live-p error-buffer)
                   (apply #'next-error-buffer-p error-buffer -args))
          error-buffer))))
