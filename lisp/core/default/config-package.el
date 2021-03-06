;;; -*- lexical-binding: t; -*-

(setq package-quickstart t)
(with-no-warnings
  (setq package-native-compile t))

(setq package-archives
      '(("gnu" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/")
        ("melpa" . "http://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/")))

(package-initialize)

(unless ymacs-package-use-gnutls-p
  (dolist (item package-archives)
    (setcdr item (replace-regexp-in-string "https:" "http:" (cdr item)))))

(require-packages! dash)
(require 'dash)
