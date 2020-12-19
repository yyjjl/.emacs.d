all: config dump

config:
	emacs --batch -l ~/.emacs.d/etc/setup.el

native-compile:
	NATIVE_COMPILE_ELPA=ON emacs --batch -l ~/.emacs.d/etc/setup.el

tarball:
	cd ~ && tar zcf /tmp/emacs.d.tgz --exclude-vcs-ignores .emacs.d

dump:
	emacs --batch -l ~/.emacs.d/etc/dump.el
