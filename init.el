;; Requires: Emacs 26+

;;----------------------------------------------------------------------------
;; Emacs General Config
;;----------------------------------------------------------------------------

;; Set GC threshold at 20 Mb
(defconst gc-threshold 20000000)
(setq gc-cons-threshold gc-threshold)

(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")))

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

;; Customize
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Disable tool bar and scroll bar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; Sort help by relevance
(setq apropos-sort-by-scores t)

;; Disable welcome screen
(setq inhibit-startup-screen t)

;; Show column number
(column-number-mode t)

;; IDO
(ido-mode 1)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-separator "\n")
(setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*" "*Messages*" "*Flymake log*" "*Compile-Log*" "*Help*"))

;; Move between windows using S-<left> S-<right> etc.
(windmove-default-keybindings)

;; Set theme
(load-theme 'monokai t)

;; Activate side scroll
(put 'scroll-left 'disabled nil)
(put 'scroll-right 'disabled nil)
(set-default 'truncate-lines t)

;; Maximize at start
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Move backup and autosave to /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Highlight current line
(global-hl-line-mode +1)
(set-face-attribute hl-line-face nil :underline nil)

;; Show matching parenthesis
(show-paren-mode 1)

;; Insert matching parenthesis
(electric-pair-mode 1)

;; Indent automatically on RET
(electric-indent-mode 1)

;; Save position in buffer
(save-place-mode 1)

;; Smaller cursor
(setq-default cursor-type 'bar)

;; Save/load desktop automatically
(desktop-save-mode 1)

;; Disable truncate-lines when editing Markdown files
(add-hook 'markdown-mode-hook 'visual-line-mode)

;; Dired
(setq dired-listing-switches "-alh")
(setq dired-auto-revert-buffer t)

;; Set up uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Shrink left fringe
(fringe-mode '(2 . nil))

;; Highlight long lines in python-mode
(require 'whitespace)
(setq-default whitespace-style '(face empty tabs lines-tail trailing spaces)
	      whitespace-line-column 79)
(add-hook 'python-mode-hook #'whitespace-mode)

;; Make frame title nicer
(setq frame-title-format "%b - Emacs 26")

;; Set ibuffer groups
(setq ibuffer-saved-filter-groups
      (quote (("default"
	       ("Python" (mode . python-mode))
	       ("REST" (mode . restclient-mode))
	       ("Shell" (mode . shell-mode))
	       ("Dired" (mode . dired-mode))
	       ("Emacs Lisp" (mode . emacs-lisp-mode))
	       ("Git" (name . "^magit"))
	       ("JSON" (name . "\\.json\\'"))
	       ("Org" (or (name . "\\.org\\'")
			  (mode . org-mode)
			  (mode . org-agenda-mode)))))))

(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-switch-to-saved-filter-groups "default")))

;; Enable auto revert
(global-auto-revert-mode)

;; Hi-Lock mode
(global-hi-lock-mode 1)

;; TRAMP
;; Use C-x C-f /ssh:etc...
(require 'tramp)
(setq tramp-default-method "ssh")
(tramp-set-completion-function "ssh" '((tramp-parse-sconfig "~/.ssh/config")))

;; Registers
(setq register-preview-delay 0)

(defun my-register-preview-function (r)
  "A custom register-previewing function which tries to be more legible."
  (format " %s  %s\n"
	  (propertize (single-key-description (car r)) 'face '(:foreground "deep pink"))
	  (register-describe-oneline (car r))))

(setq register-preview-function 'my-register-preview-function)

;;----------------------------------------------------------------------------
;; Org Mode
;;----------------------------------------------------------------------------

;; Configure directories
(setq org-directory "~/Dropbox/org")
(setq org-agenda-files (list org-directory))

;; Quick capture file
(setq org-default-notes-file (concat org-directory "/notes.org"))

;; TODO lists states, last state used as 'done'
(setq org-todo-keywords '((sequence "TODO" "CANCELLED" "DONE")))

;; Two week agenda
(setq org-agenda-span 14)

;;----------------------------------------------------------------------------
;; Package Initialization
;;----------------------------------------------------------------------------

;; Projectile
(projectile-global-mode)
(setq-default projectile-mode-line
	      '(:eval (format " Proj[%s]" (projectile-project-name))))

;; Elpy
(elpy-enable)

;; Company
(add-hook 'after-init-hook 'global-company-mode)

;; Spotify controls
(load "~/.emacs.d/spotify.el")

;; rcirc
(load "~/.emacs.d/rcirc.el" t)

;; YAML mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; Restclient mode
(add-to-list 'auto-mode-alist '("\\.http\\'" . restclient-mode))

;; Avy
(setq avy-all-windows nil)
(setq avy-background t)
(setq avy-keys '(?a ?s ?d ?f ?j ?k ?l ?\;))

;;----------------------------------------------------------------------------
;; Custom Functions
;;----------------------------------------------------------------------------

(defun comment-really-dwim ()
  "Toggle comment on line (or region if active)."
  (interactive)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(defun move-line-up ()
  "Move current line up."
  (interactive)
  (if (> (line-number-at-pos) 1)
      (progn
	(transpose-lines 1)
	(previous-line)
	(previous-line))))

(defun move-line-down ()
  "Move current line down."
  (interactive)
  (if (< (line-number-at-pos) (count-lines (point-min) (point-max)))
      (progn
	(next-line)
	(transpose-lines 1)
	(previous-line))))

(defun swap-window-pair-buffers ()
  "When two windows are open, swap their buffers."
  (interactive)
  (if (eq (count-windows) 2)
      (let* ((w1 (elt (window-list) 0))
	     (w2 (elt (window-list) 1))
	     (b1 (window-buffer w1))
	     (b2 (window-buffer w2)))
	(set-window-buffer w1 b2)
	(set-window-buffer w2 b1))
    (message "This function only works with exactly two windows.")))

(defun find-file-general-maybe-other-window (&optional arg)
  "If in a projectile project, use projectile-find file. Otherwise use ido-find-file.
When passed a prefix argument, do it on the other window."
  (interactive "P")
  (if (null arg)
      (if (projectile-project-p)
	  (projectile-find-file)
	(ido-find-file))
    (save-selected-window
      (if (projectile-project-p)
	  (projectile-find-file-other-window)
	(ido-find-file-other-window)))))

(defun switch-buffer-maybe-other-window (&optional arg)
  "Switch buffer using IDO. When passed a prefix argument, do it on the other window."
  (interactive "P")
  (if (null arg)
      (ido-switch-buffer)
    (save-selected-window
      (ido-switch-buffer-other-window))))

(defun kill-current-buffer-maybe-other-window (&optional arg)
  "Kill current buffer. When passed a prefix argument, do it on the other window."
  (interactive "P")
  (if (null arg)
      (kill-current-buffer)
    (when (> (count-windows) 1)
      (save-selected-window
	(other-window 1)
	(kill-current-buffer)))))

(defun delete-line-prefix ()
  "Delete chars from line start to point."
  (interactive)
  (delete-region (line-beginning-position) (point)))

(defun close-respose-and-request ()
  "Close last HTTP response buffer and send a new request."
  (interactive)
  (while (get-buffer "*HTTP Response*")
      (kill-buffer "*HTTP Response*"))
  (restclient-http-send-current-stay-in-window))

(defun yank-pop-verbose ()
  "Call yank-pop and show kill ring pointer index value."
  (interactive)
  (progn
    (call-interactively #'yank-pop)
    (unless (window-minibuffer-p)
      (let* ((ring-len (length kill-ring))
	     (pos (+ (- ring-len
			(length kill-ring-yank-pointer))
		     1)))
	(message "Yanked element %d of %d." pos ring-len)))))

(defun toggle-window-dedicated ()
  "Toggles the selected window's dedicated flag."
  (interactive)
  (let ((win (get-buffer-window)))
    (set-window-dedicated-p win (not (window-dedicated-p win)))
    (message "Window dedicated value is now: %s." (window-dedicated-p win))))

(defun backward-delete-word ()
  "Delete a word backwards. Delete text from previous line only when current line is empty.
This behaviour is similar to the one used by SublimeText/Atom/VSCode/etc."
  (interactive)
  (if (eq 0 (current-column))
      (call-interactively #'backward-delete-char-untabify)
    (let ((point-after-bw (save-excursion (backward-word) (point))))
      (if (< (count-lines 1 point-after-bw) (count-lines 1 (point)))
	  (delete-region (line-beginning-position) (point))
	(delete-region (point) point-after-bw)))))

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
  (kill-ring-save (line-beginning-position) (line-end-position))
  (save-excursion
    (move-end-of-line 1)
    (newline)
    (yank))
  (next-line))

(defun parse-timestamp ()
  "Read date and time from UNIX timestamp in region."
  (interactive)
  (let* ((selection (buffer-substring-no-properties (mark) (point)))
	 (timestamp (string-to-number selection)))
    (if (eq timestamp 0)
	(message "Selected value is not an integer value.")
      (message (format-time-string "%B %e, %Y - %T (UTC)" timestamp t)))))

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

(defun clear-all-highlights ()
  "Clears all highlighted items using hi-lock-mode."
  (interactive)
  (unhighlight-regexp t))

(defun print-buffer-file-name ()
  "Print the current buffer's file name/path."
  (interactive)
  (message "%s" buffer-file-name))

(defun thing-to-register-dwim (&optional arg)
  "If called with a prefix argument, prompt for register and clear its contents.
If last executed action was defining a macro, prompt for a register and save it there.
Otherwise, if region is active, copy it to a register.
Otherwise save point position and current buffer a register."
  (interactive "P")
  (if (null arg)
      (cond ((eq last-command 'kmacro-end-or-call-macro) (call-interactively #'kmacro-to-register))
	    ((use-region-p) (call-interactively #'copy-to-register))
	    (t (call-interactively #'point-to-register)))
    (let ((reg (register-read-with-preview "Delete register: ")))
      (setq register-alist (assq-delete-all reg register-alist)))))

(defun use-register-dwim (reg)
  "Prompt for a register name.
If the selected register contains text, insert its contents into the current buffer.
If the register contains a point position (or file query), jump to it.
If the register contains a keyboard macro, execute it."
  (interactive (list (register-read-with-preview "Register: ")))
  (let ((contents (get-register reg)))
    (cond ((stringp contents) (insert-register reg))
	  ((or (vectorp contents)
	       (and (consp contents) (eq (car contents) 'file-query)))
	   (jump-to-register reg))
	  ((markerp contents)
	   (let ((w (get-buffer-window (marker-buffer contents) t)))
	     (if w
		 (progn
		   (select-frame-set-input-focus (window-frame w))
		   (select-window w))
	       (jump-to-register reg))))
	  (t (message "Unknown type for register '%c'." reg)))))

(defun move-beginning-of-line-dwim ()
  "Move to the current line's first non-whitespace character. If the point
is already at the current line's first non-whitespace character, move the point
to the beginning of the line."
  (interactive)
  (when (> (current-column) 0)
      (let ((point-after-bti (save-excursion (back-to-indentation) (point))))
	(if (eq point-after-bti (point))
	    (move-beginning-of-line nil)
	  (goto-char point-after-bti)))))

;;----------------------------------------------------------------------------
;; Macros
;;----------------------------------------------------------------------------

(defmacro disable-mode-key (mode-hook mode-map key)
  "Set a key to nil for a specific mode."
  `(add-hook ,mode-hook (lambda ()
			  (define-key ,mode-map (kbd ,key) nil))))

(defmacro set-mode-key (mode-hook key func)
  "Set a key for a specific mode."
  `(add-hook ,mode-hook (lambda ()
			  (local-set-key (kbd ,key) ,func))))

(defmacro bind-key-spanish-letter (key letter)
  "Insert a specific letter by using C-c [ and a specified key."
  `(global-set-key (kbd (concat "C-c [ " ,key))
		   (lambda () (interactive) (insert ,letter))))

;;----------------------------------------------------------------------------
;; Keybindings
;;----------------------------------------------------------------------------

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x C-x") 'thing-to-register-dwim)
(global-set-key (kbd "C-a") 'move-beginning-of-line-dwim)

(global-set-key (kbd "C-o") 'flymake-goto-next-error)
(global-set-key (kbd "C-M-o") 'flymake-goto-prev-error)
(global-set-key (kbd "C-M-SPC") 'company-complete)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-j") 'avy-goto-word-1)
(global-set-key (kbd "C-;") 'comment-really-dwim)
(global-set-key (kbd "C-<") 'scroll-right)
(global-set-key (kbd "C->") 'scroll-left)
(global-set-key (kbd "C-<tab>") 'switch-buffer-maybe-other-window)
(global-set-key (kbd "C-,") 'query-replace-regexp)
(global-set-key [C-backspace] 'backward-delete-word)

(global-set-key (kbd "M-y") 'yank-pop-verbose)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-i") 'imenu)
(global-set-key (kbd "M-j") 'use-register-dwim)
(global-set-key (kbd "M-s h c") 'clear-all-highlights)
(global-set-key [M-backspace] 'backward-delete-word)

(global-set-key (kbd "C-c <tab>") 'ibuffer)
(global-set-key (kbd "C-c w") 'swap-window-pair-buffers)
(global-set-key (kbd "C-c d") 'duplicate-line)
(global-set-key (kbd "C-c DEL") 'delete-line-prefix)
(global-set-key (kbd "C-c n") 'display-line-numbers-mode)
(global-set-key (kbd "C-c f") 'flymake-mode)
(global-set-key (kbd "C-c s SPC") 'spotify-play-pause)
(global-set-key (kbd "C-c s s") 'spotify-next)
(global-set-key (kbd "C-c s p") 'spotify-previous)
(global-set-key (kbd "C-c s m") 'spotify-now-playing)
(global-set-key (kbd "C-c c") 'find-file-general-maybe-other-window)
(global-set-key (kbd "C-c k") 'kill-current-buffer-maybe-other-window)
(global-set-key (kbd "C-c j") 'json-pretty-print-buffer)
(global-set-key (kbd "C-c l") 'comint-clear-buffer)
(global-set-key (kbd "C-c i") 'indent-region)
(global-set-key (kbd "C-c h") 'shell-with-name)
(global-set-key (kbd "C-c e e") 'eval-buffer)
(global-set-key (kbd "C-c e i") 'edit-init)
(global-set-key (kbd "C-c t") 'parse-timestamp)
(global-set-key (kbd "C-c b") 'create-scratch-buffer)
(global-set-key (kbd "C-c e d") 'debbugs-gnu)
(global-set-key (kbd "C-c e p") 'print-buffer-file-name)

(global-set-key (kbd "C-c o c") 'org-capture)
(global-set-key (kbd "C-c o a") 'org-agenda)
(global-set-key (kbd "C-c o d") 'dired-org-agenda)
(global-set-key (kbd "C-c o s") 'org-sort)

(set-mode-key 'restclient-mode-hook "C-c C-v" 'close-respose-and-request)

;;----------------------------------------------------------------------------
;; Keys for quick Spanish letters insertion
;;----------------------------------------------------------------------------

(bind-key-spanish-letter "a" "á")
(bind-key-spanish-letter "e" "é")
(bind-key-spanish-letter "i" "í")
(bind-key-spanish-letter "o" "ó")
(bind-key-spanish-letter "u" "ú")
(bind-key-spanish-letter "n" "ñ")
(bind-key-spanish-letter "A" "Á")
(bind-key-spanish-letter "E" "É")
(bind-key-spanish-letter "I" "Í")
(bind-key-spanish-letter "O" "Ó")
(bind-key-spanish-letter "U" "Ú")
(bind-key-spanish-letter "N" "Ñ")
(bind-key-spanish-letter "v" "ü")
(bind-key-spanish-letter "V" "Ü")

;;----------------------------------------------------------------------------
;; Remove default keybindings
;;----------------------------------------------------------------------------

(global-unset-key (kbd "C-x f"))

(disable-mode-key 'elpy-mode-hook elpy-mode-map "<C-return>")
(disable-mode-key 'elpy-mode-hook elpy-mode-map "C-c C-c")
(disable-mode-key 'org-mode-hook org-mode-map "C-<tab>")
(disable-mode-key 'org-mode-hook org-mode-map "C-c [")
(disable-mode-key 'magit-mode-hook magit-mode-map "C-<tab>")
(disable-mode-key 'python-mode-hook python-mode-map "C-c C-c")
