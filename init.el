(package-initialize) ; %package
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t) ; %package
(setq package-selected-packages '(monokai-theme verb magit company markdown-mode pyvenv go-mode yaml-mode exec-path-from-shell dockerfile-mode jsonnet-mode lua-mod debbugs)) ; %package
(load-theme 'monokai t)
(tool-bar-mode -1)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(fido-mode)
(fido-vertical-mode)
(delete-selection-mode)
(save-place-mode)
(electric-pair-mode)
(global-company-mode)
(column-number-mode)
(global-auto-revert-mode)
(pixel-scroll-precision-mode)
(when window-system (exec-path-from-shell-initialize))
(setq confirm-kill-emacs 'yes-or-no-p
      make-backup-files nil
      auto-save-default nil
      dired-listing-switches "-alhv --group-directories-first"
      dired-auto-revert-buffer t
      dired-kill-when-opening-new-dired-buffer t
      magit-slow-confirm t
      icomplete-compute-delay 0
      require-final-newline t
      uniquify-buffer-name-style 'forward
      verb-auto-kill-response-buffers 2
      isearch-lazy-count t
      create-lockfiles nil
      mark-even-if-inactive nil
      project-vc-extra-root-markers '(".project")
      duplicate-line-final-position 1
      eglot-report-progress nil
      warning-minimum-level :error
      org-use-property-inheritance t
      custom-file (concat user-emacs-directory "custom.el")
      source-directory "~/Workspace/emacs")
(setq-default show-trailing-whitespace t
              indent-tabs-mode nil
              tab-width 4
              mode-line-format (delete '(vc-mode vc-mode) mode-line-format))
(load "~/.dotfiles/local.el" t t) ; per-PC configuration
(load custom-file)

(defun backward-delete-word ()
  "Delete (at most) a word backwards without changing the current line.
If the current line is empty, call `backward-delete-char'."
  (interactive)
  (if (zerop (current-column))
      (call-interactively #'backward-delete-char)
    (let ((point-after-bw (save-excursion (backward-word) (point))))
      (if (< (count-lines 1 point-after-bw) (count-lines 1 (point)))
          (delete-region (line-beginning-position) (point))
        (delete-region (point) point-after-bw)))))

(defun pyvenv-activate-poetry ()
  "Activate the venv created by Poetry."
  (interactive)
  (let ((default-directory (project-root (project-current)))
        (path (string-trim
               (shell-command-to-string "env -u VIRTUAL_ENV poetry env info --path"))))
    (pyvenv-activate path)
    (message "project: %s\nactivated: %s" default-directory path)))

(global-set-key (kbd "C-<backspace>") 'backward-delete-word)
(global-set-key (kbd "M-_") 'negative-argument)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-j") 'mode-line-other-buffer)
(global-set-key (kbd "M-k") '(lambda () (interactive) (list-buffers t)))
(global-set-key (kbd "M-l") 'switch-to-buffer)
(global-set-key (kbd "M-'") iso-transl-ctl-x-8-map)
(global-set-key (kbd "C-;") 'comment-line)
(global-set-key (kbd "C-o") 'flymake-goto-next-error)
(global-set-key (kbd "C-x C-d") 'dired-jump)
(global-set-key (kbd "C-c k") 'kill-this-buffer)
(global-set-key (kbd "C-c i") 'indent-region)
(global-set-key (kbd "C-c c") 'project-find-file)
(global-set-key (kbd "C-c d") 'duplicate-dwim)
(global-set-key (kbd "C-c m") (lambda () (interactive) (kill-ring-save (point-min) (point-max))))
(global-set-key (kbd "C-c p p") 'project-switch-project)
(global-set-key (kbd "C-c p s g") 'project-find-regexp)
(global-set-key (kbd "C-c o d") (lambda () (interactive) (find-file "~/Dropbox/org/notes.org")))
(global-set-key (kbd "C-c e i") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-c e p") (lambda () (interactive) (message "%s" (buffer-file-name))))
(global-set-key (kbd "M-<backspace>") (lambda () (interactive) (undo) (undo-redo)))
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(global-unset-key (kbd "C-t"))
(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c e r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c e c") 'eglot-reconnect))
(with-eval-after-load 'org
  (require 'org-tempo) ; restore <s-TAB
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))
