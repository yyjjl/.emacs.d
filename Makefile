config:
	@emacs -Q --script etc/setup/setup-emacs.el

private:
	git clone https://github.com/manateelazycat/awesome-pair ~/.emacs.d/private/awesome-pair
	git clone https://github.com/emacsmirror/dired-plus ~/.emacs.d/private/dired-plus
