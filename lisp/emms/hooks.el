
;;; -*- lexical-binding: t; -*-

(after! emms
  (add-hook 'emms-playlist-mode-hook #'emms-mark-mode)
  (advice-add #'emms-lyrics-display-handler :around #'ignore-errors!))
