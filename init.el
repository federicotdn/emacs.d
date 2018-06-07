(require 'package)

(setq package-archives
 '(("gnu" . "http://elpa.gnu.org/packages/")
   ("marmalade" . "http://marmalade-repo.org/packages/")
   ("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

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

;; Activar elpy
(elpy-enable)

;; IDO
(ido-mode 1)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)

;; Comment/uncomment line
(defun toggle-comment-on-line ()
 "comment or uncomment current line"
 (interactive)
 (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(global-set-key (kbd "C-x C-;") 'toggle-comment-on-line)

;; Theme
(load-theme 'monokai t)

;; Cosas de Customize
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(delete-selection-mode t)
 '(package-selected-packages
   (quote
    (elpy monokai-theme projectile markdown-mode)))
 '(winner-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Hacer undo/redo de layouts de ventanas
(global-set-key (kbd "M-o") 'other-window)

;; Bindear imenu a M-i
(global-set-key (kbd "M-i") 'imenu)

;; Moverse entre ventanas con S-<left> S-<right> etc.
(windmove-default-keybindings)

;; Activar side scroll
(put 'scroll-left 'disabled nil)
(put 'scroll-right 'disabled nil)
(set-default 'truncate-lines t)

(global-set-key (kbd "C-<") 'scroll-right)
(global-set-key (kbd "C->") 'scroll-left)

;; Maximizar ventana al inicio
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Guardar estado de ventanas
(desktop-save-mode 1)

;; Mover backup y autosave a /tmp
(setq backup-directory-alist
 `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
 `((".*" ,temporary-file-directory t)))

;; Subrayar linea activa
(global-hl-line-mode +1)
(set-face-attribute hl-line-face nil :underline nil)
