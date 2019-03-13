reinstall_packages:
	@echo "Backing up old packages..."
	@rm -rf elpa.old
	@(test -d elpa && mv elpa elpa.old) || true
	@echo "Done."
	@echo "Installing packages..."
	@yes | emacs -q --batch --load init-base.el \
		--eval '(package-refresh-contents)' \
		--eval '(package-install-selected-packages)'

install_external_tools:
	sudo apt install python3-pip git figlet shellcheck aspell-es
	pip3 install ical2orgpy
