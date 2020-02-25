(require-packages! emms)

(defvar emms-playlist-list
  (eval-when-compile
    (--map (abbreviate-file-name it)
           (directory-files-recursively "~/music" "\\.pls\\|\\.m3u"))))

(config! emms
  :hook
  (emms-mark-mode (emms-playlist-mode-hook))

  :advice
  (:around emms-lyrics-display-handler :name ignore-errors!)

  :config
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

(autoload 'hydra-emms/body "autoloads/emms" nil t)
(global-set-key [C-f12] #'hydra-emms/body)


(provide 'init-emms)
