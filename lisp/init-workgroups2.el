(defun my-wg-switch-workgroup ()
  (interactive)
  (let (group-names selected-group)
    (unless (featurep 'workgroups2)
      (require 'workgroups2))
    (setq group-names
          (mapcar (lambda (group)
                    ;; re-shape list for the ivy-read
                    (cons (wg-workgroup-name group) group))
                  (wg-session-workgroup-list (read (f-read-text (file-truename wg-session-file))))))
    (ivy-read "work groups" group-names
              :action (lambda (group)
                        (wg-find-session-file wg-default-session-file)
                        (wg-switch-to-workgroup group)))))

(with-eval-after-load 'workgroups2
  (setq wg-prefix-key (kbd "C-c z"))
  (setq wg-mode-line-display-on nil)
  (setq wg-session-file (expand-file-name
                         "~/.emacs.d/data/workgroups"))
  (setq wg-session-load-on-start nil)
  (setq wg-open-this-wg "none")
  ;; make sure wg-create-workgroup always success
  (defadvice wg-create-workgroup (around wg-create-workgroup-hack activate)
    (unless wg-current-session
      ;; code extracted from `wg-open-session'.
      ;; open session but do NOT load any workgroup.
      (let ((session (read (f-read-text (file-truename wg-session-file)))))
        (setf (wg-session-file-name session) wg-session-file)
        (wg-reset-internal (wg-unpickel-session-parameters session))))
    ad-do-it
    ;; save the session file in real time
    (wg-save-session t))

  (defadvice wg-reset (after wg-reset-hack activate)
    (wg-save-session t))

  ;; I'm fine to to override the original workgroup
  (defadvice wg-unique-workgroup-name-p
      (around wg-unique-workgroup-name-p-hack activate)
    (setq ad-return-value t)))

(provide 'init-workgroups2)
