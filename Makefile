BACKUP_DIR=elpa-backups

reinstall:
	$(eval target_dir := $(BACKUP_DIR)/$(shell date +%Y%m%d_%H%M%S))
	@mkdir -p $(BACKUP_DIR)
	@(test -d elpa && echo "Backing up old packages to $(target_dir)...") || true
	@(test -d elpa && mv elpa $(target_dir)) || true
	@echo "Installing packages..."
	@grep '%package' < init.el > temp.el
	@yes | emacs -q --batch --load temp.el \
		--eval '(package-refresh-contents)' \
		--eval '(package-install-selected-packages)'
	@rm temp.el
	@echo $(shell git describe --always) > elpa/commit.txt
	@echo "All done."
