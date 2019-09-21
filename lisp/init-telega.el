;;; -*- lexical-binding: t; -*-

(defun telega*wround-server-build (-fn &rest -args)
  (with-temp-env!
      (list
       (concat "LIBRARY_PATH=" (expand-var! "td-build/lib") ":" (getenv "LIBRARY_PATH"))
       (concat "CPATH=" (expand-var! "td-build/include") ":" (getenv "CPATH")))
    (apply -fn -args)))

(with-eval-after-load 'telega
  (telega-notifications-mode 1)

  (advice-add 'telega-server-build :around #'telega*wround-server-build)

  (setenv "LD_LIBRARY_PATH"
          (concat (expand-var! "td-build/lib") ":" (getenv "LD_LIBRARY_PATH")))
  (setq telega-proxies
        '((:server "127.0.0.1" :port "1080" :enable t :type (:@type "proxyTypeSocks5")))))

(provide 'init-telega)
