config:
	@emacs -Q --script etc/setup/setup-emacs.el

tarball:
	cd ~ && tar zcf /tmp/emacs.d.tgz --exclude-vcs-ignores .emacs.d

private:
	git clone https://github.com/manateelazycat/awesome-pair ~/.emacs.d/private/awesome-pair
	git clone https://github.com/emacsmirror/dired-plus ~/.emacs.d/private/dired-plus
