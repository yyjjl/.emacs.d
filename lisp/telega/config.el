;;; -*- lexical-binding: t; -*-

(after! telega
  (setenv "LD_LIBRARY_PATH"
          (format "%s:%s" (expand-cache! "td/build") (getenv "LIBRARY_PATH")))

  (setq telega-proxies
        '((:server "127.0.0.1"
           :port "1080"
           :enable t
           :type (:@type "proxyTypeSocks5")))))
