;; -*- lexical-binding:t -*-

;;;###autoload
(defun ymacs-web/smart-kill ()
  "It's a smarter kill function for `web-mode'."
  (interactive)
  (cond
   ;; Kill all content wrap by <% ... %> when right is <%
   ((and (looking-at "<%")
         (when-let (pos (save-excursion (search-forward "%>" nil t)))
           (kill-region (point) pos)
           (message "Kill <%% ... %%>")
           t)))
   ;; Kill element if no content in tag.
   ((and (looking-at "\\s-*?</")
         (looking-back "<[[:alnum:]]+\\s-*?>\\s-*?" (line-beginning-position)))
    (web-mode-element-kill 1)
    (message "Delete element"))
   ;; Kill whitespace in tag.
   ((looking-at "\\s-+>")
    (delete-horizontal-space)
    (message "Delete whitespace"))
   ;; Kill element if in tag.
   ((or (and (looking-at "\\s-*?[[:alnum:]]*>")
             (looking-back "</?[[:alnum:]]*\\s-*?" (line-beginning-position)))
        (eq (point) (web-mode-tag-beginning-position)))
    (web-mode-element-kill 1)
    (message "Kill element"))
   ;; Kill attributes if point in attributes area.
   ((when-let ((beg (web-mode-attribute-beginning-position))
               (end (web-mode-attribute-end-position)))
      (and (>= (point) beg) (<= (point) end)))
    (web-mode-attribute-kill)
    (message "Kill attribute"))
   ;; Kill attributes if only space between point and attributes start.
   ((and (looking-at "\\s-+")
         (save-excursion
           (let ((pos (search-forward-regexp "\\s-+" nil t)))
             (when (equal pos (web-mode-attribute-beginning-position))
               (web-mode-attribute-kill)
               (message "Kill attribute")
               t)))))
   ;; Kill if not inside tag
   ((not (web-mode-tag-beginning-position))
    (kill-line)
    (message "Kill line"))
   (t (message "Nothing to do"))))
