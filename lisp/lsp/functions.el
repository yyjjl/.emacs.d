;; -*- lexical-binding:t -*-

(cl-defmacro ymacs-lsp//try-enable
    (name &key (enable t) (init nil) (fallback nil))
  (declare (indent 1))
  `(add-transient-hook!
       (hack-local-variables-hook
        :local t
        :name ,(intern (format "ymacs-lsp//%s-internal" name)))
     (if (and ,enable
              ymacs-lsp-enable-in-project-p
              (ignore-errors (lsp))
              (bound-and-true-p lsp-mode))
         ,init
       ,fallback)))

(defun ymacs-lsp//set-simple-install-fn (client command)
  (setf
   (lsp--client-download-server-fn (ht-get lsp-clients client))
   (lambda (_client callback error-callback _update?)
     (condition-case err
         (run-command!
          :name (format "Install %s" client)
          :command command
          :callback (lambda (&rest _) (funcall callback))
          :error-callback (lambda (&rest _) (funcall error-callback "failed")))
       (error (funcall error-callback (error-message-string err)))))))

(defun ymacs-lsp//modern-ui-enable (-enable)
  (lsp-ui-doc-mode (if -enable 1 -1))
  (lsp-ui-sideline-mode (if -enable 1 -1))
  (lsp-modeline-code-actions-mode (if -enable -1 1)))

(defun ymacs-lsp//ui-doc-get-line-positions ()
  (save-excursion
    (let* ((num-lines (+ 1
                         (* 2 ymacs-lsp-ui-doc-search-distance)
                         (forward-line (- ymacs-lsp-ui-doc-search-distance))))
           line-positions
           line-top
           line-left)
      (catch 'done
        (dotimes (_ num-lines)
          (setq line-left
                (car (window-text-pixel-size
                      nil
                      (line-beginning-position)
                      (line-end-position))))

          (when (/= (forward-line 1) 0)
            (push (list line-left line-top (nth 3 (window-edges nil t nil t)))
                  line-positions)
            (throw 'done nil))

          (when-let (line-bottom (cdr (posn-x-y (posn-at-point (point)))))
            (push (list line-left line-top line-bottom) line-positions)
            (setq line-top line-bottom))))
      (vconcat (nreverse line-positions)))))

(defun ymacs-lsp//ui-doc-get-best-positions (-positions -top)
  (-max-by ;; sort by left and (abs (- top -top))
   (-lambda ((left1 . top1) (left2 . top2))
     (or (< left1 left2)
         (and (= left1 left2)
              (< (abs (- top1 -top))
                 (abs (- top2 -top))))))
   -positions))

(defun ymacs-lsp//ui-doc-find-position-smart (-max-height -top)
  (let* ((line-positions (ymacs-lsp//ui-doc-get-line-positions))
         (num-positions (length line-positions))
         (possible-positions nil))

    (dotimes (i num-positions)
      (-let ((max-left most-negative-fixnum)
             (line-bottom (nth 2 (aref line-positions i))))

        (while
            (and
             (< (cl-incf i) num-positions)
             (-let (((next-line-left next-line-top _)
                     (aref line-positions i)))
               (when (<= (- next-line-top line-bottom)
                         -max-height)
                 (setq max-left
                       (max next-line-left max-left))))))

        (when (and (> max-left 0)
                   (< i num-positions))
          (push (cons max-left line-bottom) possible-positions))))

    (ymacs-lsp//ui-doc-get-best-positions possible-positions -top)))
