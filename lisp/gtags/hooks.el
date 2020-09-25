;;; -*- lexical-binding: t; -*-

(after! ggtags
  (define-advice ggtags-process-string (:around (-fn &rest -args) extra-env)
    (if (stringp ymacs-ggtags-local-libpath)
        (with-temp-env! (list (concat "GTAGSLIBPATH=" ymacs-ggtags-local-libpath))
          (apply -fn -args))
      (apply -fn -args))))
