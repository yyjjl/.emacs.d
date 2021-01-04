;;; -*- lexical-binding: t; -*-

(require-packages! langtool)

(define-key! :prefix "M-s"
  ("l" . ymacs-spell/langtool-correct-at-point)
  ("L" . langtool-check)
  ("c" . langtool-correct-buffer)
  ("d" . langtool-check-done)
  ("s" . langtool-server-stop)
  ("m" . langtool-show-message-at-point))

(setq langtool-mother-tongue "zh")
(setq langtool-language-tool-jar (expand-cache! "LT/languagetool-commandline.jar"))
(setq langtool-language-tool-server-jar (expand-cache! "LT/languagetool-server.jar"))

(when byte-compile-current-file
  (unless (file-exists-p langtool-language-tool-server-jar)
    (message "LanguageTool is not found, please run %s"
             (expand-etc! "setup/install_languagetool.sh"))))

(after! (langtool wunum)
  (add-to-list 'winum-ignored-buffers 'langtool-error-buffer-name))
