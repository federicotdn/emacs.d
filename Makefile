BACKUP_DIR=elpa-backups

backup-and-clean:
	$(eval target_dir := $(BACKUP_DIR)/$(shell date +%Y%m%d_%H%M%S))
	@mkdir -p $(BACKUP_DIR)
	@(test -d elpa && echo "Backing up old packages to $(target_dir)...") || true
	@(test -d elpa && mv elpa $(target_dir)) || true
