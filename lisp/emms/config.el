
;;; -*- lexical-binding: t; -*-

(after! emms
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
