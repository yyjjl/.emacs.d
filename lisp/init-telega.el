;;; -*- lexical-binding: t; -*-

(config! telega
  :advice
  (:around telega-server-build
   :define (-fn &rest -args)
   (with-temp-env!
       (list
        (concat "LIBRARY_PATH=" (expand-var! "td-build/lib") ":" (getenv "LIBRARY_PATH"))
        (concat "CPATH=" (expand-var! "td-build/include") ":" (getenv "CPATH")))
     (apply -fn -args)))

  :config
  (telega-notifications-mode 1)

  (setenv "LD_LIBRARY_PATH"
          (concat (expand-var! "td-build/lib") ":" (getenv "LD_LIBRARY_PATH")))
  (setq telega-proxies
        '((:server "127.0.0.1"
           :port "1080"
           :enable t
           :type (:@type "proxyTypeSocks5")))))

(provide 'init-telega)
