;;; -*- lexical-binding: t; -*-

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
