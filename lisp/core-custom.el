;; -*- lexical-binding: t -*-

;; Load `custome.el' file
;; If it doesn't exist, copy from the template, then load it.
(let* ((default-directory user-emacs-directory)
       (template-file (expand-file-name "custom-template.el"))
       (target-file (expand-file-name "custom.el")))
  (if (and (file-exists-p template-file) (not (file-exists-p target-file)))
      (copy-file template-file target-file)))
;; Load private configuration
(when (file-exists-p custom-file)
  (load (file-name-sans-extension custom-file)))

(provide 'core-custom)
