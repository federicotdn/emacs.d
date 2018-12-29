reinstall_packages:
	@echo "Deleting old packages..."
	@rm -rf elpa.old
	@mv elpa elpa.old
	@echo "Done."
	@echo "Installing packages..."
	@yes | emacs -q --batch --load init-base.el \
		--eval '(package-refresh-contents)' \
		--eval '(package-install-selected-packages)'
