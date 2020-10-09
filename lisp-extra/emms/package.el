;;; -*- lexical-binding: t; -*-

(require-packages! emms)

(defvar ymacs-emms-playlist-list
  (eval-when-compile
    (--map (abbreviate-file-name it)
           (directory-files-recursively "~/music" "\\.pls\\|\\.m3u"))))

(after! emms
  (add-hook 'emms-playlist-mode-hook #'emms-mark-mode)
  (advice-add #'emms-lyrics-display-handler :around #'ignore-errors!)

  (require 'emms-player-mpv)

  (emms-all)
  (emms-mode-line -1)
  (emms-playing-time-disable-display)
  (emms-lyrics-disable)

  (setq emms-player-mpv-input-file (expand-var! "emms/mpv-input"))
  (setq emms-mode-line-format "%s")
  (setq emms-lyrics-display-p nil)
  (setq emms-source-file-default-directory "~/music/")

  (setq emms-player-list '(emms-player-mpv))
  (add-to-list 'emms-player-mpv-parameters "--no-video"))

(autoload 'ymacs-hydra/emms/body "emms/commands" nil t)

(global-set-key [f12] #'ymacs-hydra/emms/body)


(provide 'init-emms)
