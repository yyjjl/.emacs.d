config:
	@emacs -Q --script etc/setup/setup-emacs.el

tarball:
	cd ~ && tar zcf /tmp/emacs.d.tgz --exclude-vcs-ignores .emacs.d

dump:
	emacs --batch -q -l ~/.emacs.d/etc/dump.el
