;;; -*- lexical-binding: t; -*-

(defconst ymacs-editor--punctuation-alist
  '(("." . "。")
    ("," . "，")
    (":" . "：")
    (";" . "；")
    ("?" . "？")
    ("(" . "（")
    (")" . "）")
    ("!" . "！")))

(defconst ymacs-editor-ascii-before-chinese
  (rx (group-n 1 (any (?A . ?Z) (?a . ?z) (?0 . ?9) "-!@#$%^&+|:;?><.,/\\" ")]}"))
      (group-n 2 (category chinese-two-byte))))
(defconst ymacs-editor-non-space-after-punc
  (rx (group-n 1 (in ","))
      (group-n 2 (not blank))))
(defconst ymacs-editor-ascii-after-chinese
  (rx (group-n 1 (category chinese-two-byte))
      (group-n 2 (in (?A . ?Z) (?a . ?z) (?0 . ?9) "-!@#$%^&+|></\\" "([{"))))

;;;###autoload
(defun ymacs-editor/toggle-punctuation-width (-begin -end &optional -target)
  "Convert punctuation from/to English/Chinese characters."
  (interactive
   (list (if (use-region-p)
             (region-beginning)
           (point-min))
         (if (use-region-p)
             (region-end)
           (point-max))
         (completing-read! "Target:" '(half full))))
  (let ((regex (if (eq -target 'full)
                   " ?[.,;?()!] ?"
                 "[。，；？（）！]")))
    (save-excursion
      (perform-replace
       regex
       (list (lambda (&rest _)
               (or (cdr (assoc (string-trim (match-string 0)) ymacs-editor--punctuation-alist))
                   (cdr (rassoc (string-trim (match-string 0)) ymacs-editor--punctuation-alist))
                   (read-string "Replacement: "))))
       t t nil nil nil
       -begin -end
       nil                              ; backward
       (and (use-region-p) (region-noncontiguous-p))))))

;;;###autoload
(defun ymacs-editor/insert-space-around-chinese (&optional -start -end)
  (interactive (cond (current-prefix-arg
                      (list (point-min) (point-max)))
                     ((region-active-p)
                      (list (region-beginning) (region-end)))
                     (t
                      (save-mark-and-excursion
                        (mark-paragraph)
                        (list (region-beginning) (region-end))))))
  (save-excursion
    (goto-char -start)
    (while (re-search-forward ymacs-editor-ascii-before-chinese -end t)
      (replace-match "\\1 \\2" nil nil))
    (goto-char -start)
    (while (re-search-forward ymacs-editor-ascii-after-chinese -end t)
      (replace-match "\\1 \\2" nil nil))
    (goto-char -start)
    (while (re-search-forward ymacs-editor-non-space-after-punc -end t)
      (replace-match "\\1 \\2" nil nil))))
