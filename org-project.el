;; config for publish site from org files
(setq
 org-publish-project-alist
 '(
   ;; These are the main web files
   ("note"
    :base-directory "~/project/org/src" ;; Change this to your local dir
    :base-extension "org"
    :publishing-directory "~/documents/note/src"
    :recursive t
    :publishing-function org-html-publish-to-html
    :headline-levels 4             ; Just the default for this project.
    :auto-preamble nil
    :section-numbers nil
    :table-of-contents t
    :html-head
    "
<link rel='stylesheet' type='text/css' href='../../../css/org-note.css' />
<script src='../../../script/jquery.min.js'></script>
<script src='../../../script/main.js'></script>
"
    :preparation-function my-generate-note-sitemap

    :with-sub-superscript nil
    )

   ;; These are static files (images, pdf, etc)
   ("note-static"
    :base-directory "~/project/org" ;; Change this to your local dir
    :base-extension
    "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|txt\\|asc\\|json"
    :publishing-directory "~/documents/note"
    :recursive t
    :publishing-function org-publish-attachment
    )

   ("html" :components ("note" "note-static"))))

(setq org-html-mathjax-options
      '((path "../../../node_modules/MathJax/MathJax.js")
        (scale "90")
        (align "center")
        (indent "2em")
        (mathml nil)))

(defun my-clean-note ()
  (save-excursion
    (cd "~/project/org")
    (shell-command "make clean")))

(defun my-generate-note-sitemap ()
  (save-excursion
    (cd "~/project/org")
    (shell-command "make sitemap")))