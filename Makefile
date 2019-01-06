reinstall_packages:
	@echo "Deleting old packages..."
	@rm -rf elpa.old
	@mv elpa elpa.old
	@echo "Done."
	@echo "Installing packages..."
	@yes | emacs -q --batch --load init-base.el \
		--eval '(package-refresh-contents)' \
		--eval '(package-install-selected-packages)'

install_external_tools:
	sudo apt install python3-pip git figlet shellcheck
	pip3 install ical2orgpy
