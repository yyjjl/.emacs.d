;;; -*- lexical-binding: t; -*-

(defvar ymacs-editor-font-lock-keywords [nil nil nil nil])

(cl-defmacro ymacs-editor//set-font-lock-level
    (&key ((:modes -modes))
          ((:level -level))
          ((:keyword-level -keyword-level))
          ((:keywords -font-lock-keywords)))
  (declare (indent 0) (debug t))
  `(progn
     (cond
      ((listp font-lock-maximum-decoration)
       (dolist (mode ',-modes)
         (setf (alist-get mode font-lock-maximum-decoration) ,-level)))
      (t
       (setq font-lock-maximum-decoration
             (append ',(mapcar
                        (lambda (mode) (cons mode -level))
                        -modes)
                     `((t . ,font-lock-maximum-decoration))))))
     ,(when -font-lock-keywords
        `(let ((keywords ,-font-lock-keywords))
           (dolist (mode ',-modes)
             (setf (alist-get mode
                              (aref ymacs-editor-font-lock-keywords
                                    ,(or -keyword-level -level)))
                   keywords))))))

(defsubst ymacs-editor//setup-low-level-font-lock ()
  (let ((level (if (listp font-lock-maximum-decoration)
                   (alist-get major-mode font-lock-maximum-decoration)
                 font-lock-maximum-decoration)))
    (when (and (integerp level)
               (< level (length ymacs-editor-font-lock-keywords)))
      (when-let (keywords (alist-get major-mode (aref ymacs-editor-font-lock-keywords level)))
        (font-lock-add-keywords nil keywords)))))

;; Defer jit font locking slightly to [try to] improve Emacs performance
;; (setq jit-lock-defer-time 0.3)
(setq jit-lock-defer-time nil)
(setq jit-lock-stealth-nice 0.5)
(setq jit-lock-stealth-time 5)
(setq jit-lock-stealth-verbose nil)
