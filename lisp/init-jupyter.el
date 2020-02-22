;;; -*- lexical-binding: t; -*-

(define-variable! :pkg org jupyter sage)

(require-packages!
 ;; IPython notebook feature in `org-mode'
 (jupyter :when org-use-jupyter-p)
 (sage-shell-mode :when org-use-sage-p
                  :compile (sage-shell-mode sage-shell-view)))



(defconst org--sage-overlay-help-template
  "--------------------
Text: %s
LaTeX: %s
--------------------
[R] Regenerate [T] Show text [w] Copy text
[W] Copy LaTeX [=] Zoom in   [-] Zoom out")

(with-eval-after-load 'jupyter-org-client
  (define-key! :map jupyter-org-interaction-mode-map
    ("M-i"))
  (jupyter-org-define-key (kbd "C-c C-d") #'jupyter-inspect-at-point))

(defvar sage-shell:source-buffer nil)
(with-eval-after-load 'sage-shell-mode
  (fset 'sage-shell:-process-sentinel-generator 'identity)
  (advice-add 'sage-shell-view-process-overlay :after 'sage*after-ceare-overlay)

  (define-key! :map sage-shell-mode-map
    ("C-c M-o" . sage/clea-current-buffer)
    ("C-c C-z" . sage/pop-to-source-buffer))

  (defun sage/clea-current-buffer ()
    (interactive)
    (sage-shell:-delete-output (point-min))
    (sage-shell-edit:delete-temp-dir))

  (defun sage/pop-to-source-buffer ()
    (interactive)
    (unless sage-shell:source-buffer
      (setq sage-shell:source-buffer
            (completing-read "Buffer:"
                             (mapcar
                              #'buffer-name
                              (--filter (eq 'sage-shell:sage-mode
                                            (buffer-local-value 'major-mode it))
                                        (buffer-list)))
                             nil :require-match)))
    (when sage-shell:source-buffer
      (pop-to-buffer sage-shell:source-buffer)))

  (defun sage*after-ceare-overlay (-ov)
    (unless (overlay-get -ov 'extra-map)
      (let ((map (overlay-get -ov 'keymap)))
        (define-key! :map map
          ("?" . (lambda! (display-message-or-buffer
                           (format org--sage-overlay-help-template
                                   (overlay-get -ov 'text)
                                   (overlay-get -ov 'math)))))
          ;; Regenerate
          ("R" . (lambda! (sage-shell-view-regenerate -ov)))
          ;; Show text
          ("T" . (lambda! (overlay-put -ov 'display nil)))
          ("w" . (lambda!
                   (sage-shell-view-copy-text -ov)
                   (message "Text copied")))
          ("W" . (lambda!
                   (sage-shell-view-copy-latex -ov)
                   (message "LaTeX copied")))
          ("=" . (lambda! (sage-shell-view--when-overlay-active
                              -ov (sage-shell-view-zoom-in -ov))))
          ("-" . (lambda! (sage-shell-view--when-overlay-active
                              -ov (sage-shell-view-zoom-out -ov)))))
        (overlay-put -ov 'extra-map t))))

  (defun sage-shell-edit:delete-temp-dir ()
    (when (and (stringp sage-shell-edit:temp-directory)
               (string= (file-name-as-directory temporary-file-directory)
                        (file-name-directory sage-shell-edit:temp-directory))
               (file-exists-p sage-shell-edit:temp-directory))
      (message "Delete directory %s" sage-shell-edit:temp-directory)
      (delete-directory sage-shell-edit:temp-directory t)))

  (setq sage-shell-view-scale 1.5)

  (define-hook! sage|setup (sage-shell-after-prompt-hook)
    (when (display-graphic-p)
      (sage-shell-view-mode 1))
    (remove-hook 'sage-shell:process-exit-hook
                 'sage-shell-edit:delete-temp-dir)
    (add-hook 'term-or-comint-process-exit-hook
              (lambda () (run-hooks 'sage-shell:process-exit-hook))
              nil :local)
    (add-hook 'term-or-comint-process-exit-hook
              'sage-shell-edit:delete-temp-dir
              nil :local)))

(with-eval-after-load 'org
  (require 'ob-jupyter)
  (add-to-list 'org-babel-load-languages '(jupyter . t)))

(provide 'init-jupyter)
