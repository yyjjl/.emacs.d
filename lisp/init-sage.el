;;; -*- lexical-binding: t; -*-

(define-variable! :pkg org sage)

(require-packages!
 ;; IPython notebook feature in `org-mode'
 ;; (jupyter :when org-use-jupyter-p)
 (sage-shell-mode :when org-use-sage-p
                  :compile (sage-shell-mode sage-shell-view)))

(defvar sage-shell:source-buffer nil)

(defconst org--sage-overlay-help-template
  "--------------------
Text: %s
LaTeX: %s
--------------------
[R] Regenerate [T] Show text [w] Copy text
[W] Copy LaTeX [=] Zoom in   [-] Zoom out")


(config! sage-shell-mode
  :bind
  (:map sage-shell-mode-map
   ("C-c M-o" . sage/clean-current-buffer)
   ("C-c C-z" . sage/pop-to-source-buffer))

  :advice
  (:override sage-shell:-process-sentinel-generator :name identity)

  (:after sage-shell-view-process-overlay
   :define (-ov)
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

  :hook
  (setup
   :define (sage-shell-after-prompt-hook)
   (when (display-graphic-p)
     (sage-shell-view-mode 1))
   (remove-hook 'sage-shell:process-exit-hook 'sage-shell-edit:delete-temp-dir)
   (add-hook 'term-or-comint-process-exit-hook
             (lambda () (run-hooks 'sage-shell:process-exit-hook))
             nil :local)
   (add-hook 'term-or-comint-process-exit-hook
             'sage-shell-edit:delete-temp-dir
             nil :local))

  :config

  (defun sage-shell-edit:delete-temp-dir ()
    (when (and (stringp sage-shell-edit:temp-directory)
               (string= (file-name-as-directory temporary-file-directory)
                        (file-name-directory sage-shell-edit:temp-directory))
               (file-exists-p sage-shell-edit:temp-directory))
      (message "Delete directory %s" sage-shell-edit:temp-directory)
      (delete-directory sage-shell-edit:temp-directory t)))

  (setq sage-shell-view-scale 1.5))

(provide 'init-sage)
