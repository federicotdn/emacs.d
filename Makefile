BACKUP_DIR=elpa-backups

update:
	git pull origin master

reinstall:
	$(eval target_dir := $(BACKUP_DIR)/$(shell date +%Y%m%d_%H%M%S))
	@mkdir -p $(BACKUP_DIR)
	@(test -d elpa && echo "Backing up old packages to $(target_dir)...") || true
	@(test -d elpa && mv elpa $(target_dir)) || true
	@echo "Installing packages..."
	@yes | emacs -q --batch --load init-package.el \
		--eval '(package-refresh-contents)' \
		--eval '(package-install-selected-packages)'
	@echo $(shell git describe --always) > elpa/commit.txt
	@echo "All done."

install_external_tools:
	sudo apt install python3-pip git figlet shellcheck aspell-es gnome-screensaver

clean:
	rm -rf auto-save-list/ url/ tramp

time_startup:
	python3 -m timeit \
		-r 1 -n 5 \
		'__import__("subprocess").call("emacs --eval (kill-emacs)".split())'
