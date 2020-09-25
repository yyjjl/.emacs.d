;;; -*- lexical-binding: t; -*-

(defun ymacs-misc//try-expand-local-snippets ()
  (when-let*
      ((bounds (bounds-of-thing-at-point 'word))
       (template (cdr-safe (assoc-string
                            (buffer-substring-no-properties (car bounds)
                                                            (cdr bounds))
                            ymacs-misc-local-snippets-list))))
    (yas-expand-snippet template (car bounds) (cdr bounds))))
