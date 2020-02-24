;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun go/install-tools ()
  (interactive)
  (run-command!
   :name "install gopls"
   :command "GO111MODULE=on go get golang.org/x/tools/gopls@latest")
  (run-command!
   :name "install go-tag"
   :command "go get -u github.com/fatih/gomodifytags"))
