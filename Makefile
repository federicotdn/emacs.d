reinstall_packages:
	@test ! -f .elpalock || \
		(printf "Previous reinstall failed.\nBack up elpa.old, delete .elpalock and try again.\n\n" && false)
	@touch .elpalock
	@echo "Backing up old packages..."
	@rm -rf elpa.old
	@(test -d elpa && mv elpa elpa.old) || true
	@echo "Done."
	@echo "Installing packages..."
	@yes | emacs -q --batch --load init-base.el \
		--eval '(package-refresh-contents)' \
		--eval '(package-install-selected-packages)'
	@rm .elpalock

install_external_tools:
	sudo apt install python3-pip git figlet shellcheck aspell-es

clean:
	rm -rf auto-save-list/ url/ tramp
