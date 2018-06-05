(require 'package)

(setq package-archives
 '(("gnu" . "http://elpa.gnu.org/packages/")
   ("marmalade" . "http://marmalade-repo.org/packages/")
   ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

;; neotree package
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; apagar toolbar
(when (fboundp 'tool-bar-mode)
 (tool-bar-mode -1))

(setq apropos-sort-by-scores t)
(setq inhibit-startup-screen t)

(global-linum-mode t)
(column-number-mode t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (wombat)))
 '(ido-enable-flex-matching t)
 '(ido-mode (quote both) nil (ido))
 '(winner-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Hacer undo/redo de layouts de ventanas
(global-set-key (kbd "M-o") 'other-window)

;; Moverse entre ventanas con S-<left> S-<right> etc.
(windmove-default-keybindings)
