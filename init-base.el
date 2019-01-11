;; init-base.el --- general Emacs config -*- lexical-binding: t; -*-

;; General configuration for Emacs and built-in Emacs packages.
;; All configuration for third-party packages is done in init.el.

;; Configure package sources
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")
	("melpa" . "https://melpa.org/packages/")))

;; Call package-initialize on version 26 only
(when (< emacs-major-version 27)
  (package-initialize))

;; Customize
(setq custom-file "~/.emacs.d/init-custom.el")
(load custom-file)

;; Disable tool bar, scroll bar and menu bar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; More extensive apropos searches
(setq apropos-do-all t)

;; Show column number
(column-number-mode t)

;; Customize scratch buffer
(setq initial-scratch-message nil)
(setq initial-major-mode 'fundamental-mode)

;; IDO
(ido-mode 1)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-default-buffer-method 'selected-window)
(setq ido-separator "\n")
(setq ido-ignore-buffers
      '("^ "
	"*Completions*"
	"*Shell Command Output*"
	"*Flymake log*"
	"*Compile-Log*"
	"*Help*"
	"magit-process"
	"magit-diff"))

;; Move between windows using S-<left> S-<right> etc.
(windmove-default-keybindings)

;; Activate side scroll
(put 'scroll-left 'disabled nil)
(put 'scroll-right 'disabled nil)
(set-default 'truncate-lines t)

;; Maximize at start
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Move backup and autosave to /tmp
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

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

;; Disable truncate-lines when editing Markdown files
(add-hook 'markdown-mode-hook 'visual-line-mode)

;; Dired
(setq dired-listing-switches "-alhv --group-directories-first")
(setq dired-auto-revert-buffer t)

;; Set up uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Highlight long lines in python-mode and sh-mode
(require 'whitespace)
(setq-default whitespace-style '(face tabs lines-tail trailing spaces)
	      whitespace-line-column 79)

(add-hook 'python-mode-hook #'whitespace-mode)
(add-hook 'sh-mode-hook #'whitespace-mode)

;; Set fill-column for Python
(add-hook 'python-mode-hook (lambda () (set-fill-column 79)))

;; Make frame title nicer
(setq frame-title-format (format "%%b - Emacs %s" emacs-version))

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

(setq register-preview-function #'my-register-preview-function)

;; Make scrolling quicker
(setq auto-window-vscroll nil)

;; Dont jump when scrolling by line
(setq scroll-conservatively 10)

;; Load per-PC configuration file
;; local.el is gitignore'd
(load "~/.emacs.d/local.el" t t)

;; Allow auto-resizing windows horizontally on command
(setq fit-window-to-buffer-horizontally t)

;; Start Emacs server
;; This allows using emacsclient as an editor
(server-start)

;; Print yank pointer index after yank-pop
(advice-add 'yank-pop :after
	    (lambda (&rest r)
	      (unless (window-minibuffer-p)
		(let* ((ring-len (length kill-ring))
		       (pos (+ (- ring-len
				  (length kill-ring-yank-pointer))
			       1)))
		  (message "Yanked element %d of %d." pos ring-len)))))

;; When scrolling by page, leave 1 line of continuity instead of 2
(setq next-screen-context-lines 1)

;; In shell mode, don't jump to position after output
(add-hook 'shell-mode-hook
	  (lambda ()
	    (remove-hook 'comint-output-filter-functions
			 'comint-postoutput-scroll-to-bottom)))

;; Ignore duplicate commands in shell mode
(setq comint-input-ignoredups t)

;; Load iso-transl in order to change the C-x 8 prefix later
(require 'iso-transl)

;; Create templates using tempo.el
(require 'tempo)
(load "~/.emacs.d/init-templates.el")

;; Allow hippie-expand to complete tempo tags
(defun try-tempo-complete-tag (old)
  (unless old
    (tempo-complete-tag)))

(add-to-list 'hippie-expand-try-functions-list 'try-tempo-complete-tag)

;; JS indent level
(setq js-indent-level 2)
