(require-packages!
 (emms :when emacs-has-mpv-p)
 (emms-player-mpv :when emacs-has-mpv-p))



(defvar emms-playlist-list
  (eval-when-compile
    (--map (abbreviate-file-name it)
           (directory-files-recursively "~/music" "\\.pls\\|\\.m3u"))))

(with-eval-after-load 'emms
  (require 'emms-player-mpv)

  (emms-all)
  (emms-mode-line -1)
  (emms-playing-time-disable-display)
  (emms-lyrics-disable)

  (setq emms-mode-line-format "%s")
  (setq emms-lyrics-display-p nil)
  (setq emms-source-file-default-directory "~/music/")

  (setq emms-player-list '(emms-player-mpv))
  (add-to-list 'emms-player-mpv-parameters "--no-video")

  (advice-add 'emms-lyrics-display-handler :around
              (lambda ($fn &rest $args) (ignore-errors (apply $fn $args))))
  (add-hook 'emms-playlist-mode-hook #'emms-mark-mode))

(defun extra/emms-current-name ()
  (concat (propertize
           (abbreviate-file-name (emms-mode-line-playlist-current))
           'face 'font-lock-constant-face)))
(defhydra hydra-emms
  ( ;; options
   :color pink
          :hint nil
          :body-pre
          (unless (and (fboundp 'emms-playlist-current-selected-track)
                       (emms-playlist-current-selected-track))
            (unless (featurep 'emms) (require 'emms))
            (emms-play-playlist (ivy-read "Playlist:" emms-playlist-list))))
  "
%s(extra/emms-current-name)
[_=_/_-_]volume raise/lower  [_d_/_D_]play directory subtree/current
[_p_]rev [_n_]ext            [_t_/_T_]oggle repeat playlist/track
[_s_]top [_P_]ause/Continue  [_r_]andom
[_SPC_]Select playlist     [_RET_]Open playlist buffer                [_q_]uit
"
    ("n" emms-next)
    ("p" emms-previous)
    ("s" emms-stop :exit t)
    ("P" emms-pause)
    ("r" emms-random)
    ("d" emms-play-directory-tree)
    ("D" emms-play-directory)
    ("SPC" (emms-play-playlist
            (ivy-read "Playlist:" emms-playlist-list)))
    ("RET" emms-playlist-mode-go :exit t)
    ("t" emms-toggle-repeat-playlist)
    ("T" emms-toggle-repeat-track)
    ("-" emms-volume-lower)
    ("=" emms-volume-raise)
    ("q" nil nil))
  (global-set-key [f12] #'hydra-emms/body)


(provide 'init-emms)
