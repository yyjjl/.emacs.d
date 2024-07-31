;;; -*- lexical-binding: t; -*-

(option! eldoc-use-childfeame nil
  "use childframe to display eldoc"
  :type 'boolean)

(after! posframe
  (advice-add #'keyboard-quit :before #'posframe-hide-all))
