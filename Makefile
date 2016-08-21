
default: backup

backup:
	@echo Begin to backup
	@tar -jcvf data/.emacs.tar.bz2 site-lisp/ snippets/ lisp/ init.el custom.el  README.org
	@echo Finished