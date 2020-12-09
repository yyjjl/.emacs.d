;;; -*- lexical-binding: t; -*-

(defun ymacs-tools//filter-ring (ring)
  (--filter
   (< (length it) 1024)
   (mapcar #'substring-no-properties ring)))

(defun ymacs-tools//try-expand-local-snippets ()
  (when-let*
      ((bounds (bounds-of-thing-at-point 'word))
       (template (cdr-safe (assoc-string
                            (buffer-substring-no-properties (car bounds)
                                                            (cdr bounds))
                            ymacs-tools-local-snippets-list))))
    (yas-expand-snippet template (car bounds) (cdr bounds))))

(defun ymacs//buffer-has-long-lines-p ()
  ;; @see Doom Emacs #2183: `so-long-detected-long-line-p' tries to parse
  ;;      comment syntax, but in some buffers comment state isn't
  ;;      initialized, leading to a wrong-type-argument: stringp error.
  (let ((so-long-skip-leading-comments
         (bound-and-true-p comment-use-syntax)))
    (so-long-detected-long-line-p)))
