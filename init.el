(require 'package)

(setq package-archives
 '(("gnu" . "http://elpa.gnu.org/packages/")
   ("marmalade" . "http://marmalade-repo.org/packages/")
   ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

;; Installed packages:
;; projectile

;; Apagar toolbar
(when (fboundp 'tool-bar-mode)
 (tool-bar-mode -1))

;; Apagar scrollbars
(scroll-bar-mode -1)

;; Ordenar help por relevancia
(setq apropos-sort-by-scores t)

;; Sacar welcome screen
(setq inhibit-startup-screen t)

;; Numero de linea y columna
(global-linum-mode t)
(column-number-mode t)
(size-indication-mode t)

;; Activar Projectile minor mode globalmente
(projectile-global-mode)

;; Cosas de Customize
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

;; Activar side scroll
(put 'scroll-left 'disabled nil)
(put 'scroll-right 'disabled nil)
(set-default 'truncate-lines t)

;; Maximizar ventana al inicio
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Mover backup y autosave a /tmp
(setq backup-directory-alist
 `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
 `((".*" ,temporary-file-directory t)))

;; Subrayar linea activa
(global-hl-line-mode +1)
(set-face-attribute hl-line-face nil :underline nil)
