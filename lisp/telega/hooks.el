;;; -*- lexical-binding: t; -*-

(after! telega
  (add-hook 'telega-ready-hook #'telega-notifications-mode)

  (define-advice telega-server-build (:around (-fn &rest -args) set-env)
    (with-temp-env!
        (list
         ymacs-telega-ld-library-path
         (format "LIBRARY_PATH=%s:%s"
                 (expand-var! "td-build/lib")
                 (getenv "LIBRARY_PATH"))
         (format "CPATH=%s:%s"
                 (expand-var! "td-build/include")
                 (getenv "CPATH")))
      (apply -fn -args))))
