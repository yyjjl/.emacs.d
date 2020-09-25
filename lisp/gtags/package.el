;;; -*- lexical-binding: t; -*-

(require-packages! ggtags)

(defcustom ymacs-ggtags-local-libpath nil
  "."
  :group 'ggtags
  :type 'directory
  :safe #'stringp)

(provide 'init-tags)
