;;; -*- lexical-binding: t; -*-

(after! zone
  (setq zone-programs (vconcat zone-programs [zone-pgm-sl zone-pgm-rainbow zone-nyan])))

(after! zone-nyan
  (setq zone-nyan-text-size (/ (frame-width) 2))
  (setq zone-nyan-gui-type 'text)
  (setq zone-nyan-term-type 'ascii))
