(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-selected-packages '(monokai-theme verb magit company markdown-mode pyvenv go-mode yaml-mode exec-path-from-shell dockerfile-mode jsonnet-mode lua-mode debbugs rg terraform-mode haskell-mode))
(package-install-selected-packages t)
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
(when (and window-system (eq system-type 'gnu/linux)) (exec-path-from-shell-initialize))
(setq confirm-kill-emacs 'yes-or-no-p
      ring-bell-function 'ignore
      make-backup-files nil auto-save-default nil create-lockfiles nil
      dired-listing-switches "-alhv --group-directories-first" dired-auto-revert-buffer t dired-kill-when-opening-new-dired-buffer t
      icomplete-compute-delay 0
      require-final-newline t
      uniquify-buffer-name-style 'forward
      isearch-lazy-count t
      initial-scratch-message nil
      mark-even-if-inactive nil
      project-vc-extra-root-markers '(".gitignore" ".project")
      duplicate-line-final-position 1
      eglot-report-progress nil eglot-ignored-server-capabilities '(:inlayHintProvider :documentOnTypeFormattingProvider)
      eglot-events-buffer-config '(:size 0 :format full)
      company-dabbrev-downcase nil
      warning-minimum-level :error
      org-use-property-inheritance t
      custom-file (concat user-emacs-directory "custom.el")
      compilation-scroll-output t
      js-indent-level 2
      eldoc-echo-area-use-multiline-p 5
      magit-slow-confirm t
      verb-auto-kill-response-buffers 2
      rg-command-line-flags '("--sort=path" "--no-messages"))
(setq-default show-trailing-whitespace t indent-tabs-mode nil tab-width 4
              mode-line-format (delete '(vc-mode vc-mode) mode-line-format))
(load "~/.dotfiles/local.el" t t) ; per-PC configuration
(load custom-file t)

(defun backward-delete-word ()
  "Delete (at most) a word backwards without changing the current line.
If the current line is empty, call `backward-delete-char'."
  (interactive)
  (if (bolp) (backward-delete-char 1)
    (with-restriction (line-beginning-position) (point)
      (delete-region (progn (backward-word) (point)) (point-max)))))

(defun search-in-project (regexp)
  "Find all matches for REGEXP in the current project's roots using ripgrep."
  (interactive (list (read-regexp "Find regexp" nil project-regexp-history-variable)))
  (rg regexp "*" (project-root (project-current))))

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
(global-set-key (kbd "C-c k") 'kill-current-buffer)
(global-set-key (kbd "C-c i") 'indent-region)
(global-set-key (kbd "C-c c") 'project-find-file)
(global-set-key (kbd "C-c d") 'duplicate-dwim)
(global-set-key (kbd "C-c C-n") (lambda () (interactive) (hs-minor-mode) (call-interactively #'hs-toggle-hiding)))
(global-set-key (kbd "C-c m") (lambda () (interactive) (kill-ring-save (point-min) (point-max))))
(global-set-key (kbd "C-c p p") 'project-switch-project)
(global-set-key (kbd "C-c p s g") 'search-in-project)
(global-set-key (kbd "C-c o d") (lambda () (interactive) (find-file "~/Dropbox/org/notes.org")))
(global-set-key (kbd "C-c e i") (lambda () (interactive) (find-file "~/.emacs.d/init.el")))
(global-set-key (kbd "C-c e p") (lambda () (interactive) (message "%s" (buffer-file-name))))
(global-set-key (kbd "M-<backspace>") (lambda () (interactive) (undo) (undo-redo)))
(global-set-key [remap dabbrev-expand] 'hippie-expand)
(dolist (k '("C-t" "C-z" "s-x" "M-c" "M-z" "C-x C-c")) (global-unset-key (kbd k))) ; accidental presses
(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c e r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c e c") 'eglot-reconnect))
(with-eval-after-load 'python
  (define-key python-mode-map (kbd "C-c C-p") nil))
(with-eval-after-load 'org
  (require 'org-tempo) ; restore <s-TAB
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))
