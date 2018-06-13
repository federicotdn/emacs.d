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
	("marmalade" . "http://marmalade-repo.org/packages/")
	("melpa-stable" . "https://stable.melpa.org/packages/")))

(package-initialize)

;; Customize
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Show line numbers using new mode (26+)
(global-display-line-numbers-mode 1)

;; Disable tool bar and scroll bar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(scroll-bar-mode -1)

;; Sort help by relevance
(setq apropos-sort-by-scores t)

;; Disable welcome screen
(setq inhibit-startup-screen t)

;; Show column number
(column-number-mode t)

;; Deactivate VC utils
(setq vc-handled-backends nil)

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

;;----------------------------------------------------------------------------
;; Package Initialization
;;----------------------------------------------------------------------------

;; Projectile
(projectile-global-mode)
(setq-default projectile-mode-line
	      '(:eval (format " Proj[%s]" (projectile-project-name)))
	      )

;; Elpy
(elpy-enable)

;; Company
(add-hook 'after-init-hook 'global-company-mode)

;; Purpose
(require 'window-purpose)
(purpose-mode)
(add-to-list 'purpose-user-mode-purposes '(python-mode . py))
(purpose-compile-user-configuration)

;;----------------------------------------------------------------------------
;; Custom Functions
;;----------------------------------------------------------------------------

;; Comment/uncomment line
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

;; Neotree
(defun neotree-project-dir ()
  "Open NeoTree using the Projectile project root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find Projectile project root."))))


;;----------------------------------------------------------------------------
;; Keybindings
;;----------------------------------------------------------------------------

(global-set-key (kbd "C-;") 'toggle-comment-on-line)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-<tab>") 'ido-switch-buffer)
(global-set-key (kbd "C-<") 'scroll-right)
(global-set-key (kbd "C->") 'scroll-left)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key [f8] 'neotree-project-dir)
(global-set-key (kbd "C-c l") 'comint-clear-buffer)
(global-set-key (kbd "C-c d") 'purpose-toggle-window-purpose-dedicated)
(global-set-key (kbd "C-c D") 'purpose-toggle-window-buffer-dedicated)

(global-unset-key (kbd "C-x f"))
