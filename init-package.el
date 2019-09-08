;; init-base.el --- Package initialization -*- lexical-binding: t; -*-

;;----------------------------------------------------------------------------
;; Custom Variables
;;----------------------------------------------------------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (use-package olivetti elpy pyvenv spotify flymake-shellcheck avy yaml-mode restclient debbugs magit monokai-theme projectile markdown-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;----------------------------------------------------------------------------
;; Package
;;----------------------------------------------------------------------------

;; Configure package sources
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "https://melpa.org/packages/")))

;; Call package-initialize on version 26 only
(when (< emacs-major-version 27)
  (package-initialize))
