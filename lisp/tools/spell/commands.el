;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun ymacs-spell/langtool-correct-at-point ()
  "Execute interactive correction at point after `langtool-check'"
  (interactive)
  (if-let (ovs (langtool--overlays-region (point) (point)))
      (progn
        (barf-if-buffer-read-only)
        (langtool--correction ovs))
    (message "No LanguageTool check found")))
