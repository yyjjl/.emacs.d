;;; -*- lexical-binding: t; -*-

(defun ymacs-misc//filter-ring (ring)
  (--filter
   (< (length it) 1024)
   (mapcar #'substring-no-properties ring)))

(defun ymacs-misc//get-occur-buffer ()
  (cl-loop
   for window in (window-list)
   for buffer = (window-buffer window)
   when (with-current-buffer buffer
          (apply 'derived-mode-p
                 ymacs-misc-auto-next-error-buffer-derived-modes))
   return buffer))

(defun ymacs-misc//try-expand-local-snippets ()
  (when-let*
      ((bounds (bounds-of-thing-at-point 'word))
       (template (cdr-safe (assoc-string
                            (buffer-substring-no-properties (car bounds)
                                                            (cdr bounds))
                            ymacs-misc-local-snippets-list))))
    (yas-expand-snippet template (car bounds) (cdr bounds))))

(defun ymacs//buffer-has-long-lines-p ()
  ;; @see Doom Emacs #2183: `so-long-detected-long-line-p' tries to parse
  ;;      comment syntax, but in some buffers comment state isn't
  ;;      initialized, leading to a wrong-type-argument: stringp error.
  (let ((so-long-skip-leading-comments
         (bound-and-true-p comment-use-syntax)))
    (so-long-detected-long-line-p)))
