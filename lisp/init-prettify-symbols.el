;;; -*- lexical-binding: t; -*-

;; Set symbol font
(set-fontset-font t '(57600 . 57711) "Fira Code Symbol")
(define-hook! (extra|font-setup _) (after-make-frame-functions)
  (set-fontset-font t '(57600 . 57711) "Fira Code Symbol"))

(defvar fira-code-font-lock-keywords-alist
  (eval-and-compile
    (defun extra//make-fix-width-symbol (-alist)
      (cl-loop for (string . char) in -alist
            for width = (string-width string)
            collect
            (cons string (if (= width 1)
                             char
                           (concat "\t" (list char) "\t")))))
    (extra//make-fix-width-symbol
     '(("!=" . 57614)
       ("-->" . 57619)
       ("->" . 57620)
       ("->>" . 57621)
       ("-<" . 57622)
       ("-<<" . 57623)
       ("|>" . 57653)
       ("===" . 57661)
       ("==>" . 57662)
       ("=>" . 57663)
       ("=>>" . 57664)
       ("<=" . 57665)
       ("=<<" . 57666)
       ("=/=" . 57667)
       (">-" . 57668)
       (">=" . 57669)
       (">=>" . 57670)
       (">>-" . 57672)
       (">>=" . 57673)
       (">>>" . 57674)
       ("<*>" . 57676)
       ("<|" . 57677)
       ("<|>" . 57678)
       ("<$>" . 57680)
       ("<!--" . 57681)
       ("<-" . 57682)
       ("<--" . 57683)
       ("<->" . 57684)
       ("<+>" . 57686)
       ("<=" . 57687)
       ("<==" . 57688)
       ("<=>" . 57689)
       ("<=<" . 57690)
       ("<>" . 57691)
       ("<<-" . 57693)
       ("<<=" . 57694)
       ("<<<" . 57695)
       ("<~" . 57696)
       ("<~~" . 57697)
       ("</>" . 57699)
       ("~=" . 57702)
       ("~>" . 57703)
       ("~~>" . 57705)))))

(define-hook! extra|prettify-symbols-setup (prog-mode-hook)
  (when (display-graphic-p)
    (setq prettify-symbols-alist fira-code-font-lock-keywords-alist)
    (prettify-symbols-mode 1)))

;; (setq prettify-symbols-unprettify-at-point 'right-edge)

(provide 'init-prettify-symbols)
