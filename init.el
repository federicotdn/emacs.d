;; init.el -*- lexical-binding: t; -*-
;; Requires: Emacs 26+

;;----------------------------------------------------------------------------
;; Emacs General Config
;;----------------------------------------------------------------------------

;; Configure GC
(defconst gc-default-threshold 800000)
(defconst gc-large-threshold (* gc-default-threshold 10))

;; Set GC threshold to a large value during init
(setq gc-cons-threshold gc-large-threshold)

;; General Emacs config is in init-base.el
(load "~/.emacs.d/init-base.el")

;;----------------------------------------------------------------------------
;; Org Mode
;;----------------------------------------------------------------------------

;; Disable truncate-lines when editing Org files
(add-hook 'org-mode-hook 'visual-line-mode)

;; Configure directories
(setq org-directory "~/Dropbox/org/")
(setq org-agenda-files (list org-directory))

;; Quick capture file
(setq org-default-notes-file (concat org-directory "/notes.org"))

;; TODO lists states, last state used as 'done'
(setq org-todo-keywords '((sequence "TODO" "CURRENT" "DONE")))

;; important tag
(setq org-tag-faces '(("imp" . (:foreground "red" :weight bold))
		      ("easy" . (:foreground "green"))))

;; Configure Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (emacs-lisp . t)
   (shell . t)))

(setq org-confirm-babel-evaluate nil)

;; Refile to any agenda file
(setq org-refile-targets '((org-agenda-files :maxlevel . 2)))

;; Save all Org buffers after refile
(advice-add 'org-refile :after (lambda (&rest r) (org-save-all-org-buffers)))

;; Always refile to top of entry
(setq org-reverse-note-order t)

;; Don't allow TODOs to be completed unless all children tasks are marked as done
(setq org-enforce-todo-dependencies t)

;;----------------------------------------------------------------------------
;; Package Initialization
;;----------------------------------------------------------------------------

;; Set theme
(load-theme 'monokai t)

;; Projectile
(projectile-mode +1)
(setq projectile-mode-line-function
      (lambda ()
	(format " P[%s]" (projectile-project-name))))

;; Magit
(with-eval-after-load 'magit
  (add-to-list 'magit-repository-directories '("~/Workspace/" . 2))
  (setq magit-slow-confirm t))

;; Company
(add-hook 'after-init-hook 'global-company-mode)

;; flymake-shellcheck
(add-hook 'sh-mode-hook 'flymake-shellcheck-load)

;; secret values
(load "~/Dropbox/emacs/secrets.el" t)

;; YAML mode
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; Restclient mode
(require 'restclient)
(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))

;; Avy
(setq avy-all-windows nil)
(setq avy-background t)
(setq avy-keys '(?a ?s ?d ?f ?j ?k ?l ?\;))

;; Add highlighting for Python indentation
(add-hook 'python-mode-hook 'highlight-indentation-mode)

;; LSP
(require 'lsp)
(add-hook 'python-mode-hook #'lsp)

;; Company completion for LSP
(require 'company-lsp)
(push 'company-lsp company-backends)

;;----------------------------------------------------------------------------
;; Custom Functions
;;----------------------------------------------------------------------------

(defun find-file-general (&optional arg)
  "If in a Projectile project, call projectile-find-file. Otherwise,
call ido-find-file."
  (interactive)
  (if (projectile-project-p)
      (projectile-find-file)
    (ido-find-file)))

(defun comment-really-dwim ()
  "Toggle comment on line (or region if active)."
  (interactive)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(defun move-line-up ()
  "Move current line up."
  (interactive)
  (when (> (line-number-at-pos) 1)
    (transpose-lines 1)
    (previous-line)
    (previous-line)))

(defun move-line-down ()
  "Move current line down."
  (interactive)
  (when (< (line-number-at-pos) (count-lines (point-min) (point-max)))
    (next-line)
    (transpose-lines 1)
    (previous-line)))

(defun swap-window-pair-buffers ()
  "When two windows are open, swap their buffers."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((w1 (elt (window-list) 0))
	     (w2 (elt (window-list) 1))
	     (b1 (window-buffer w1))
	     (b2 (window-buffer w2)))
	(set-window-buffer w1 b2)
	(set-window-buffer w2 b1))
    (user-error "This function only works with exactly two windows")))

(defun close-response-and-request ()
  "Close last HTTP response buffer and send a new request from current
restclient URL (if region isn't active) or from URL contained by the
region (if it's active). Always display results on a separate window
to the right."
  (interactive)
  (save-buffer)
  (let ((name "*HTTP Response**"))
    (while (get-buffer name)
      (kill-buffer name)))
  (when (= (count-windows) 1)
    (split-window-right))
  (if (use-region-p)
      (let ((text (buffer-substring (region-beginning) (region-end))))
	(with-temp-buffer
	  (insert text)
	  (restclient-http-send-current-stay-in-window)))
    (restclient-http-send-current-stay-in-window)))

(defun backward-delete-word ()
  "Delete a word backwards. Delete text from previous line only when
current line is empty. This behaviour is similar to the one used by
SublimeText/Atom/VSCode/etc."
  (interactive)
  (if (= 0 (current-column))
      (call-interactively #'backward-delete-char-untabify)
    (let ((point-after-bw (save-excursion (backward-word) (point))))
      (if (< (count-lines 1 point-after-bw) (count-lines 1 (point)))
	  (delete-region (line-beginning-position) (point))
	(delete-region (point) point-after-bw)))))

(defun delete-whole-line ()
  "Delete current line."
  (interactive)
  (goto-char (line-beginning-position))
  (delete-region (point) (line-end-position))
  (delete-forward-char 1))

(defun shell-with-name ()
  "Create a shell with a specific name."
  (interactive)
  (let ((name (read-string "Shell name: ")))
    (shell (concat "*shell"
		   (if (string= name "") "" (concat " " name))
		   "*"))))

(defun edit-init ()
  "Edit init.el in a buffer."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun duplicate-line ()
  "Duplicate a line, and move point to it (maintain current column)."
  (interactive)
  (let ((val (buffer-substring (line-beginning-position) (line-end-position))))
    (save-excursion
      (move-end-of-line 1)
      (newline)
      (insert val)))
  (next-line))

(defun create-scratch-buffer ()
  "Create a new scratch buffer in Fundamental mode."
  (interactive)
  (let* ((name (read-string "Scratch buffer name: "))
	 (fullname (concat "*scratch"
			   (if (string= name "") "" (concat " " name))
			   "*")))
    (switch-to-buffer (get-buffer-create fullname))
    (fundamental-mode)))

(defun dired-org-agenda ()
  "Open org-directory with dired."
  (interactive)
  (dired org-directory "-l")
  (dired-hide-details-mode))

(defun print-buffer-file-name ()
  "Print the current buffer's file path."
  (interactive)
  (let ((name (buffer-file-name)))
    (if name
	(message name)
      (user-error "Buffer is not visiting any file"))))

(defun rename-file-buffer ()
  "Rename the current buffer's file, and the buffer itself to match
the new file name."
  (interactive)
  (let ((current-file-name (buffer-file-name)))
    (if (and current-file-name (not (buffer-modified-p)))
	(let ((new-file-name (read-file-name "New file name:" nil current-file-name 'confirm)))
	  (if (and (not (file-exists-p new-file-name))
		   (not (get-file-buffer new-file-name)))
	      (progn
		(rename-file current-file-name new-file-name)
		(set-visited-file-name new-file-name)
		(set-buffer-modified-p nil))
	    (user-error "File already exists!")))
      (user-error "Current buffer is not visiting any file or has unsaved changes"))))

(defun import-icalendar-url (url dest)
  "Download an iCalendar file from URL (asynchronously) and convert it
to a Org mode file, using ical2orgpy. The created file will be placed
in file DEST, inside the current org-directory."
  (interactive "sEnter URL: \nsEnter filename: ")
  (unless (executable-find "ical2orgpy")
    (user-error "Could not find ical2orgpy executable"))
  (let ((ical-file (make-temp-file "emacs-ical"))
	(org-file (expand-file-name (concat org-directory dest))))
    (with-temp-file ical-file
      (url-insert-file-contents url))
    (if (= 0 (call-process "ical2orgpy" nil nil nil ical-file org-file))
	(message "iCal exported to: %s" org-file)
      (error "ical2orgpy process error"))
    (delete-file ical-file)))

(defun import-google-calendar ()
  "Import calendar from Google Calendar."
  (interactive)
  (import-icalendar-url gcal-url "gcal.org"))

(defun wrap-region (c)
  "Wrap point or active region with character C and its corresponding
pair."
  (interactive (list (read-char-exclusive "Wrap region with: ")))
  (let* ((char-pairs '(("{" . "}")
		       ("(" . ")")
		       ("[" . "]")
		       ("<" . ">")
		       ("¿" . "?")
		       ("¡" . "!")))
	 (s (char-to-string c))
	 (pair (catch 'loop
		 (dolist (p char-pairs)
		   (when (or (string= s (car p))
			     (string= s (cdr p)))
		     (throw 'loop p)))
		 (cons s s))))
    (if (use-region-p)
	(let ((region-end-pos (region-end)))
	  (insert-pair nil (car pair) (cdr pair))
	  (goto-char (+ region-end-pos 2)))
      (insert (car pair) (cdr pair))
      (backward-char))))

(defun kill-ring-save-whole-buffer ()
  "Save the entire buffer as if killed, but don't kill it."
  (interactive)
  (kill-ring-save (point-min) (point-max))
  (message "Buffer copied to kill ring."))

(defun json-pretty-print-dwim ()
  "Prettify JSON in region if it is active, otherwise on whole buffer."
  (interactive)
  (let ((json-encoding-default-indentation (make-string js-indent-level ? )))
    (if (use-region-p)
	(json-pretty-print (region-beginning) (region-end))
      (json-pretty-print-buffer))))

(defun goto-last-edit ()
  "Go to the last edit made in the current buffer."
  (interactive)
  (unless (or (consp buffer-undo-list)
	      (not buffer-undo-list))
    (user-error "Can't go to last edit: invalid undo list"))
  (let ((pos (catch 'loop
	       (dolist (item buffer-undo-list)
		 (when (and (consp item)
			    (or (integerp (car item))
				(stringp (car item))))
		   (throw 'loop (abs (cdr item))))))))
    (unless (or (null pos)
		(= (point) pos))
      (push-mark)
      (goto-char pos))))

(defun goto-end-clear-screen ()
  "Go to the end of the buffer and then move current buffer line to
window line 0."
  (interactive)
  (end-of-buffer '(4))
  (recenter-top-bottom 0))

(defun dired-on-default-directory ()
  "Open dired on the directory contained in `default-directory'."
  (interactive)
  (dired default-directory))

(define-derived-mode long-lines-mode fundamental-mode "Long-Lines"
  "Simple mode to allow editing files with very long lines."
  (setq bidi-display-reordering nil)
  (buffer-disable-undo))

(defun open-file-external (filename)
  "Open a file or directory using the user's preferred application."
  (interactive "G")
  (unless (executable-find "xdg-open")
    (user-error "Could not find xdg-open executable"))
  (unless (file-exists-p filename)
    (user-error "Invalid file path"))
  (call-process "xdg-open" nil nil nil (file-truename filename)))

(defun activate-pyvenv-lsp (arg)
  "Activate a Python virtual environment and Language Server using
pyvenv and lsp-mode. By default, use virtual environment in
(project-current)/env. If called with a prefix argument, prompt for
virtual environment path instead."
  (interactive "P")
  (unless (derived-mode-p 'python-mode)
    (user-error "Current buffer's major mode must be python-mode"))
  (let* ((proj-dir (cdr (project-current)))
	 (venv-name (if arg (read-from-minibuffer "Venv name: " "env") "env"))
	 (venv-path (concat proj-dir venv-name)))
    (message "venv: %s" venv-path)
    (unless (file-exists-p venv-path)
      (user-error "Path not found: %s" venv-path))
    (pyvenv-activate (concat proj-dir venv-name))
    (when (= (call-process "pip" nil nil nil "show" "python-language-server") 1)
      (pyvenv-deactivate)
      (user-error "Python Language Server (pyls) not installed in venv"))
    (lsp)))

(defun deactivate-pyvenv-lsp ()
  "Deactivate currently enabled Python virtual environment and LSP server."
  (interactive)
  (unless (derived-mode-p 'python-mode)
    (user-error "Current buffer's major mode must be python-mode"))
  (lsp-shutdown-workspace)
  (pyvenv-deactivate))

;;----------------------------------------------------------------------------
;; Keybindings
;;----------------------------------------------------------------------------

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x C-d") 'dired-on-default-directory)

(global-set-key (kbd "C-o") 'flymake-goto-next-error)
(global-set-key (kbd "C-j") 'avy-goto-char-timer)
(global-set-key (kbd "C-;") 'comment-really-dwim)
(global-set-key (kbd "C-<") 'scroll-right)
(global-set-key (kbd "C->") 'scroll-left)
(global-set-key (kbd "C-,") 'query-replace-regexp)
(global-set-key (kbd "C-=") 'mark-word)
(global-set-key (kbd "C-M-=") 'wrap-region)
(global-set-key (kbd "C-<backspace>") 'backward-delete-word)
(global-set-key (kbd "C-S-<backspace>") 'delete-whole-line)

(global-set-key (kbd "M-l") 'ido-switch-buffer)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-i") 'imenu)
(global-set-key (kbd "M-<backspace>") 'goto-last-edit)

(global-set-key (kbd "C-c w") 'swap-window-pair-buffers)
(global-set-key (kbd "C-c d") 'duplicate-line)
(global-set-key (kbd "C-c f") 'flymake-mode)
(global-set-key (kbd "C-c s SPC") 'spotify-playpause)
(global-set-key (kbd "C-c s s") 'spotify-next)
(global-set-key (kbd "C-c s p") 'spotify-previous)
(global-set-key (kbd "C-c s c") 'spotify-current)
(global-set-key (kbd "C-c c") 'find-file-general)
(global-set-key (kbd "C-c k") 'kill-current-buffer)
(global-set-key (kbd "C-c j") 'json-pretty-print-dwim)
(global-set-key (kbd "C-c i") 'indent-region)
(global-set-key (kbd "C-c h") 'shell-with-name)
(global-set-key (kbd "C-c e e") 'eval-buffer)
(global-set-key (kbd "C-c e i") 'edit-init)
(global-set-key (kbd "C-c e r") 'rename-file-buffer)
(global-set-key (kbd "C-c e d") 'debbugs-gnu)
(global-set-key (kbd "C-c e p") 'print-buffer-file-name)
(global-set-key (kbd "C-c e o") 'open-file-external)
(global-set-key (kbd "C-c q") 'quick-calc)
(global-set-key (kbd "C-c y") 'browse-kill-ring)
(global-set-key (kbd "C-c b") 'create-scratch-buffer)
(global-set-key (kbd "C-c <tab>") 'ibuffer)
(global-set-key (kbd "C-c m") 'kill-ring-save-whole-buffer)
(global-set-key (kbd "C-c z") 'apropos)

(global-set-key (kbd "C-c o a") 'org-agenda)
(global-set-key (kbd "C-c o d") 'dired-org-agenda)
(global-set-key (kbd "C-c o r") 'org-archive-to-archive-sibling)
(global-set-key (kbd "C-c o t") 'org-force-cycle-archived)
(global-set-key (kbd "C-c o g") 'import-google-calendar)

(global-set-key (kbd "ESC ESC ESC") 'keyboard-quit)

(global-set-key [remap dabbrev-expand] 'hippie-expand)

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(define-key restclient-mode-map (kbd "C-c C-c") 'close-response-and-request)
(define-key shell-mode-map (kbd "C-r") 'comint-history-isearch-backward-regexp)
(define-key shell-mode-map (kbd "C-l") 'goto-end-clear-screen)
(define-key shell-mode-map (kbd "C-c C-l") 'comint-clear-buffer)
(define-key python-mode-map (kbd "M-[") 'python-indent-shift-left)
(define-key python-mode-map (kbd "M-]") 'python-indent-shift-right)

(define-key org-mode-map (kbd "M-n") 'outline-next-visible-heading)
(define-key org-mode-map (kbd "M-p") 'outline-previous-visible-heading)
(define-key org-mode-map (kbd "C-j") 'avy-goto-char-timer)
(define-key org-mode-map (kbd "C-,") 'query-replace-regexp)
(define-key global-map (kbd "M-'") iso-transl-ctl-x-8-map)

;; Free keys:
;; C-c SPC
;; C-.
;; M-j

;;----------------------------------------------------------------------------
;; Remove default keybindings
;;----------------------------------------------------------------------------

;; Disable some default keys that get hit by accident

(global-unset-key (kbd "C-x f"))
(global-unset-key (kbd "M-;"))
(global-unset-key (kbd "C-z"))

(define-key org-mode-map (kbd "C-c [") nil)
(define-key org-mode-map (kbd "C-'") nil)
(define-key shell-mode-map (kbd "C-c C-l") nil)
(define-key python-mode-map (kbd "C-c C-c") nil)

;;----------------------------------------------------------------------------
;; Cleanup
;;----------------------------------------------------------------------------

;; Restore GC threshold
(setq gc-cons-threshold gc-default-threshold)
