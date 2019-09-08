;; init.el -*- lexical-binding: t; -*-
;; Requires: Emacs 26+

;;----------------------------------------------------------------------------
;; GC Config
;;----------------------------------------------------------------------------

;; Set GC threshold to a large value during init
(defconst gc-default-threshold gc-cons-threshold)
(setq gc-cons-threshold (* gc-default-threshold 100))

;;----------------------------------------------------------------------------
;; Initialization
;;----------------------------------------------------------------------------

;; Initialize package management and custom variables
(setq custom-file "~/.emacs.d/init-package.el")
(load custom-file)

;; Enable use-package
(eval-when-compile
  (require 'use-package))

;; Enable delete selection mode
(delete-selection-mode t)

;; Helper macro for different operating systems
(defmacro when-system (os &rest body)
  "Execute the forms in BODY only on operating system OS."
  (declare (indent 1))
  `(when (eq system-type ',os)
     ,@body))

;; Disable tool bar, scroll bar and menu bar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; Show column number
(column-number-mode t)

;; Customize scratch buffer
(setq initial-scratch-message nil)
(setq initial-major-mode 'fundamental-mode)

;; Activate side scroll
(put 'scroll-left 'disabled nil)
(put 'scroll-right 'disabled nil)
(set-default 'truncate-lines t)

;; Maximize at start
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Move backup and autosave to /tmp
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Make frame title nicer
(setq frame-title-format (format "%%b - GNU Emacs %s" emacs-version))

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

;; Load synced secret values
(load "~/Dropbox/emacs/secrets.el" t t)

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

;; Deactivate mark before undo (never do selective undo in region)
(advice-add 'undo :before (lambda (&rest r) (deactivate-mark)))

;; Always confirm quit
(setq confirm-kill-emacs 'yes-or-no-p)

;; Always save bookmarks
(setq bookmark-save-flag 1)

;; Ignore case in autocomplete
(setq completion-ignore-case t)

;; Disable VC mode
(setq vc-handled-backends nil)

;; Setup stuff on macOS
(when-system darwin
  ;; Change behavior of left command key
  (setq mac-command-modifier 'meta)

  ;; Add brew binaries to PATH
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
  (add-to-list 'exec-path "/usr/local/bin")

  ;; Disable bell
  (setq ring-bell-function 'ignore)

  ;; Setup ispell
  (setq ispell-program-name "/usr/local/bin/aspell"))

;; Set theme, but make comments a bit brighter (original value: #75715E)
(use-package monokai-theme
  :config
  (setq monokai-comments "#908E80")
  (load-theme 'monokai t))

;; Save position in buffer
(use-package saveplace
  :config
  (save-place-mode 1))

;; Show matching parenthesis
(use-package paren
  :config
  (show-paren-mode 1))

;; Insert matching parenthesis
(use-package elec-pair
  :config
  (electric-pair-mode 1))

;; Indent automatically on RET
(use-package electric
  :config
  (electric-indent-mode 1))

;; Dired
(use-package dired
  :config
  (setq dired-listing-switches "-alhv --group-directories-first"
        dired-auto-revert-buffer t))

;; Dired-X
(use-package dired-x
  :bind ("C-x C-d" . dired-jump))

;; IDO
(use-package ido
  :config
  (ido-mode 1)
  (setq ido-everywhere t
	ido-enable-flex-matching t
	ido-default-buffer-method 'selected-window
	ido-separator "\n"
	ido-ignore-buffers
	'("^ "
	  "*Completions*"
	  "*Shell Command Output*"
	  "*Flymake log*"
	  "*Compile-Log*"
	  "magit-process*"
	  "magit-revision*"
	  "magit-reflog*")))

;; Uniquify buffer names
(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward))

;; Apropos
(use-package apropos
  :bind ("C-h a" . 'apropos)
  :config
  ;; More extensive apropos searches
  (setq apropos-do-all t))

;; Whitespace
(use-package whitespace
  :config
  (setq-default whitespace-style '(face tabs lines-tail trailing)
		whitespace-line-column 88))

;; Python
(use-package python
  :mode ("\\.py\\'" . python-mode)
  :bind (:map python-mode-map
	 ("M-[" . python-indent-shift-left)
	 ("M-]" . python-indent-shift-right)
	 ("C-c C-c" . nil))
  :config
  (add-hook 'python-mode-hook 'whitespace-mode)
  ;; Set fill-column for Python
  (add-hook 'python-mode-hook (lambda () (set-fill-column 79))))

;; Elpy
(use-package elpy
  :after python
  :bind (:map elpy-mode-map
	 ("C-c C-c" . nil)
	 ("<C-return>" . nil))
  :config
  (elpy-enable))

;; Markdown
(use-package markdown-mode
  :mode "\\.md\\'"
  :config
  (add-hook 'markdown-mode-hook 'visual-line-mode))

;; Auto revert files
(use-package autorevert
  :config
  (global-auto-revert-mode))

;; TRAMP
(use-package tramp
  :commands (find-file ido-find-file)
  :config
  ;; Use C-x C-f /ssh:etc...
  (setq tramp-default-method "ssh")
  (tramp-set-completion-function "ssh" '((tramp-parse-sconfig "~/.ssh/config"))))

;; Shell
(use-package shell
  :demand
  :bind (:map shell-mode-map
	 ("C-r" . comint-history-isearch-backward-regexp)
	 ("C-l" . goto-end-clear-screen))
  :config
  ;; In shell mode, don't jump to position after output
  (add-hook 'shell-mode-hook
	    (lambda ()
	      (remove-hook 'comint-output-filter-functions
			   'comint-postoutput-scroll-to-bottom)))

  ;; Ignore duplicate commands in shell mode
  (setq comint-input-ignoredups t)

  (defun goto-end-clear-screen ()
    "Go to the end of the buffer and then move current buffer line to
window line 0."
    (interactive)
    (end-of-buffer '(4))
    (recenter-top-bottom 0)))

;; Input keys for different languages
(use-package iso-transl
  :bind-keymap ("M-'" . iso-transl-ctl-x-8-map))

;; Tempo templates
(use-package tempo
  :after python
  :config
  (tempo-define-template "python-pdb"
			 '("import pdb; pdb.set_trace()")
			 "pdb")

  (tempo-define-template "python-code-interact"
			 '("import code; code.interact(local=locals())")
			 "interact")

  (tempo-define-template "python-property"
			 '("@property" n>
			   "def " (P "Property: " prop) "(self):" n>
			   "return self._" (s prop))
			 "property")

  (tempo-define-template "python-traceback"
			 '("import traceback; traceback.print_stack()")
			 "traceback")

  ;; Allow hippie-expand to complete tempo tags
  (defun try-tempo-complete-tag (old)
    (unless old
      (tempo-complete-tag)))

  (add-to-list 'hippie-expand-try-functions-list 'try-tempo-complete-tag))

;; JavaScript / JSON
(use-package js
  :mode (("\\.js\\'" . js-mode)
	 ("\\.json\\'" . js-mode))
  :bind ("C-c j" . json-pretty-print-dwim)
  :config
  (setq js-indent-level 4)

  (defun json-pretty-print-dwim ()
    "Prettify JSON in region if it is active, otherwise on whole buffer."
    (interactive)
    (let ((json-encoding-default-indentation (make-string js-indent-level ? )))
      (if (use-region-p)
	  (json-pretty-print (region-beginning) (region-end))
	(json-pretty-print-buffer)))))

;; GNUS
(use-package gnus
  :commands gnus
  :config
  (setq gnus-thread-sort-functions
	'(gnus-thread-sort-by-number
	  gnus-thread-sort-by-most-recent-date))

  (setq gnus-subthread-sort-functions
	'(gnus-thread-sort-by-number
	  (not gnus-thread-sort-by-most-recent-date)))

  (use-package nnir))

;; Spell-check messages
(use-package flyspell
  :hook (message-mode . flyspell-mode))

;; Configure ls-lisp (macOS only)
(when-system darwin
  (use-package ls-lisp
    :config
    (setq ls-lisp-dirs-first t
	  ls-lisp-use-insert-directory-program nil)
    (setq dired-listing-switches "-alhv")))

;; Org Mode
(use-package org
  :mode "\\.org\\'"
  :bind (("C-c o a" . org-agenda)
	 ("C-c o d" . dired-org-agenda)
	 :map org-mode-map
	 ("M-n" . outline-next-visible-heading)
	 ("M-p" . outline-previous-visible-heading)
	 ("C-j" . avy-goto-char-timer)
	 ("C-c o r" . org-archive-to-archive-sibling)
	 ("C-c o t" . org-force-cycle-archived)
	 ("C-c [" . nil)
	 ("C-'" . nil))
  :config
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

  ;; Record time and not when TODOs are completed
  (setq org-log-done 'note)

  ;; Don't repeat date when note is added ("NOTE CLOSED %t")
  (setf (cdr (assq 'done org-log-note-headings)) "NOTE:")

  (defun dired-org-agenda ()
    "Open org-directory with dired."
    (interactive)
    (dired org-directory "-l")
    (dired-hide-details-mode)))

;; Projectile
(use-package projectile
  :bind-keymap ("C-c p" . projectile-command-map)
  :bind (:map projectile-mode-map
	 ("C-c c" . projectile-find-file))
  :config
  (projectile-mode 1)
  (setq projectile-mode-line-function
	(lambda ()
	  (format " P[%s]" (projectile-project-name))))

  (setq projectile-use-git-grep t))

;; Magit
(use-package magit
  :bind (("C-x g" . magit-status)
	 ("C-c M-g" . magit-file-dispatch))
  :config
  ;; Register all directories in Workspace
  (add-to-list 'magit-repository-directories '("~/Workspace/" . 2))

  ;; Always confirm with yes/no when discarding changes
  (setq magit-slow-confirm t)

  ;; Remove ":" from magit buffer names to search them more easily
  (setq magit-buffer-name-format
	(replace-regexp-in-string ":" "" magit-buffer-name-format)))

;; Company completion
(use-package company
  :config
  (global-company-mode 1))

;; Flymake-Shellcheck
(use-package flymake-shellcheck
  :commands flymake-shellcheck-load
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load))

;; YAML
(use-package yaml-mode
  :mode (("\\.yml\\'" . yaml-mode)
	 ("\\.yaml\\'" . yaml-mode)))

;; Restclient mode
(use-package restclient
  :mode ("\\.http\\'" . restclient-mode)
  :bind (:map restclient-mode-map
	 ("C-c C-c" . close-response-and-request))
  :config
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
      (restclient-http-send-current-stay-in-window))))

;; Avy
(use-package avy
  :bind (("C-j" . avy-goto-char-timer))
  :config
  (setq avy-all-windows nil
        avy-background t
	avy-style 'words))

;; Imenu
(use-package imenu
  :bind ("M-i" . imenu))

;; Debbugs
(use-package debbugs
  :bind ("C-c e d" . debbugs-gnu))

;; Spotify controls
(use-package spotify
  :bind (("C-c s SPC" . spotify-playpause)
	 ("C-c s s" . spotify-next)
	 ("C-c s p" . spotify-previous))
  :config
  (when-system gnu/linux
    (global-set-key (kbd "C-c s c") 'spotify-current)))

;; Toggle Flymake
(use-package flymake
  :bind (("C-c f" . flymake-mode)
	 ("C-o" . flymake-goto-next-error)))

;; Calculator
(use-package calc
  :bind ("C-c q" . quick-calc))

;;----------------------------------------------------------------------------
;; Global Functions
;;----------------------------------------------------------------------------

(defun move-line-up ()
  "Move current line up."
  (interactive)
  (when (> (line-number-at-pos) 1)
    (let ((col (current-column)))
      (transpose-lines 1)
      (previous-line)
      (previous-line)
      (move-to-column col))))

(defun move-line-down ()
  "Move current line down."
  (interactive)
  (when (< (line-number-at-pos) (count-lines (point-min) (point-max)))
    (let ((col (current-column)))
      (next-line)
      (transpose-lines 1)
      (previous-line)
      (move-to-column col))))

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

(defun print-buffer-file-name (&optional arg)
  "Print the current buffer's file path.
If ARG is non-nil, make the file path the latest kill in the kill
ring."
  (interactive "P")
  (let ((name (buffer-file-name)))
    (unless name
      (user-error "Buffer is not visiting any file"))
    (message name)
    (when arg
      (kill-new name))))

(defun rename-file-buffer ()
  "Rename the current buffer's file, and the buffer itself to match
the new file name."
  (interactive)
  (let ((current-file-name (buffer-file-name)))
    (unless current-file-name
      (user-error "Current buffer is not visiting any file"))
    (when (buffer-modified-p)
      (user-error "Current buffer has unsaved changes"))
    (let ((new-file-name (read-file-name "New file name:" nil current-file-name 'confirm)))
      (when (or (file-exists-p new-file-name)
		(get-file-buffer new-file-name))
	(user-error "File already exists!"))
      (rename-file current-file-name new-file-name)
      (set-visited-file-name new-file-name)
      (let ((inhibit-message t))
	(save-buffer)))))

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

(define-derived-mode long-lines-mode fundamental-mode "Long-Lines"
  "Simple mode to allow editing files with very long lines."
  (setq bidi-display-reordering nil)
  (buffer-disable-undo))

(defun open-file-external (filename)
  "Open file or directory FILENAME using the user's preferred
application."
  (interactive "G")
  (let ((executable "xdg-open"))
    (when-system darwin (setq executable "open"))
    (unless (executable-find executable)
      (user-error (format "Could not find the %s executable" executable)))
    (unless (file-exists-p filename)
      (user-error "Invalid file path"))
    (call-process executable nil nil nil (file-truename filename))))

;;----------------------------------------------------------------------------
;; Global keybindings
;;----------------------------------------------------------------------------

(global-set-key (kbd "C-;") 'comment-line)
(global-set-key (kbd "C-<") 'scroll-right)
(global-set-key (kbd "C->") 'scroll-left)
(global-set-key (kbd "C-<backspace>") 'backward-delete-word)
(global-set-key (kbd "C-S-<backspace>") 'delete-whole-line)
(global-set-key (kbd "C-M-#") 'wrap-region)
(global-set-key (kbd "C-M-_") 'negative-argument)

(global-set-key (kbd "M-_") 'negative-argument)
(global-set-key (kbd "M-l") 'switch-to-buffer)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-j") 'mode-line-other-buffer)
(global-set-key (kbd "M-<backspace>") 'goto-last-edit)
(when-system darwin
  (global-set-key (kbd "M-`") 'other-frame))

(global-set-key (kbd "C-c d") 'duplicate-line)
(global-set-key (kbd "C-c k") 'kill-current-buffer)
(global-set-key (kbd "C-c i") 'indent-region)
(global-set-key (kbd "C-c e e") 'eval-buffer)
(global-set-key (kbd "C-c e i") 'edit-init)
(global-set-key (kbd "C-c e r") 'rename-file-buffer)
(global-set-key (kbd "C-c e p") 'print-buffer-file-name)
(global-set-key (kbd "C-c e o") 'open-file-external)
(global-set-key (kbd "C-c b") 'create-scratch-buffer)
(global-set-key (kbd "C-c m") 'kill-ring-save-whole-buffer)

(global-set-key (kbd "ESC ESC ESC") 'keyboard-quit)

(global-set-key [remap dabbrev-expand] 'hippie-expand)

;;----------------------------------------------------------------------------
;; Remove Global Keybindings
;;----------------------------------------------------------------------------

;; Disable some default keys that get hit by accident
(global-unset-key (kbd "C-x f"))
(global-unset-key (kbd "C-x s"))
(global-unset-key (kbd "C-x C-n"))
(global-unset-key (kbd "M-;"))
(global-unset-key (kbd "M-k"))
(global-unset-key (kbd "M-t"))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-t"))

;;----------------------------------------------------------------------------
;; Cleanup
;;----------------------------------------------------------------------------

;; Restore GC threshold
(setq gc-cons-threshold gc-default-threshold)
