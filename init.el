;; Setear el GC threshold a 20 Mb como base
(defconst gc-threshold 20000000)

(setq gc-cons-threshold gc-threshold)

(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")
	("melpa-stable" . "https://stable.melpa.org/packages/")))

(package-initialize)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

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

(setq-default projectile-mode-line
	      '(:eval (format " Proj[%s]" (projectile-project-name)))
	      )

;; Desactivar sistemas VC dentro de Emacs
(setq vc-handled-backends nil)

;; Activar elpy
(elpy-enable)

;; Activar company mode
(add-hook 'after-init-hook 'global-company-mode)

;; IDO
(ido-mode 1)
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-separator "\n")
(setq ido-ignore-buffers '("^ " "*Completions*" "*Shell Command Output*" "*Messages*" "*Flymake log*" "*Compile-Log*" "*Help*"))

;; Comment/uncomment line
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))

(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; Theme
(load-theme 'monokai t)

;; Cambiar protocolo default para TRAMP
(setq tramp-default-method "ssh")

;; Hacer undo/redo de layouts de ventanas
(global-set-key (kbd "M-o") 'other-window)

;; Bindear imenu a M-i
(global-set-key (kbd "M-i") 'imenu)

;; Moverse entre ventanas con S-<left> S-<right> etc.
(windmove-default-keybindings)

;; IDO switch buffer con C-<tab>
(global-set-key (kbd "C-<tab>") 'ido-switch-buffer)

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

;; Mostrar matching parenthesis
(show-paren-mode 1)

;; Insertar matching parenthesis
(electric-pair-mode 1)

;; Indentar autotmaticamente on RET
(electric-indent-mode 1)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

;; Guardar posicion en buffer
(save-place-mode 1)

;; Dedication
(add-to-list 'load-path "~/.emacs.d/dedication")

(require 'dedication)

(bypass-window-dedicated #'ido-switch-buffer)
(bypass-window-dedicated #'ido-find-file)
(enable-dedication-mode-line)

(global-set-key (kbd "C-c d") 'toggle-window-dedicated)

;; Neotree
(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
        (file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
        (if (neo-global--window-exists-p)
            (progn
              (neotree-dir project-dir)
              (neotree-find file-name)))
      (message "Could not find git project root."))))

(global-set-key [f8] 'neotree-project-dir)

;; Disable GC when minibuffer is open
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold gc-threshold))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)
