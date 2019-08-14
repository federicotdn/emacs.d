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

;;----------------------------------------------------------------------------
;; Base Initialization
;;----------------------------------------------------------------------------

;; Initialize package management and custom variables
(setq custom-file "~/.emacs.d/init-package.el")
(load custom-file)

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
	"magit-process*"))

;; Activate side scroll
(put 'scroll-left 'disabled nil)
(put 'scroll-right 'disabled nil)
(set-default 'truncate-lines t)

;; Maximize at start
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Move backup and autosave to /tmp
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Show matching parenthesis
(show-paren-mode 1)

;; Insert matching parenthesis
(electric-pair-mode 1)

;; Indent automatically on RET
(electric-indent-mode 1)

;; Save position in buffer
(save-place-mode 1)

;; Disable truncate-lines when editing Markdown files
(add-hook 'markdown-mode-hook 'visual-line-mode)

;; Dired
(setq dired-listing-switches "-alhv --group-directories-first")
(setq dired-auto-revert-buffer t)

;; Set up uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Highlight long lines in python-mode
(require 'whitespace)
(setq-default whitespace-style '(face tabs lines-tail trailing)
	      whitespace-line-column 88)

(add-hook 'python-mode-hook 'whitespace-mode)

;; Set fill-column for Python
(add-hook 'python-mode-hook (lambda () (set-fill-column 79)))

;; Make frame title nicer
(setq frame-title-format (format "%%b - GNU Emacs %s" emacs-version))

;; Enable auto revert
(global-auto-revert-mode)

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

;; In shell mode, don't jump to position after output
(add-hook 'shell-mode-hook
	  (lambda ()
	    (remove-hook 'comint-output-filter-functions
			 'comint-postoutput-scroll-to-bottom)))

;; Ignore duplicate commands in shell mode
(setq comint-input-ignoredups t)

;; Load iso-transl in order to change the C-x 8 prefix later
(require 'iso-transl)

;; Load python-mode in order to change keymap later
(require 'python)

;; Create templates using tempo.el
(require 'tempo)

;; Tempo templates for Python

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

(add-to-list 'hippie-expand-try-functions-list 'try-tempo-complete-tag)

;; JS indent level
(setq js-indent-level 4)

;; Configure Gnus
(setq gnus-thread-sort-functions
      '(gnus-thread-sort-by-number
        gnus-thread-sort-by-most-recent-date))

(setq gnus-subthread-sort-functions
      '(gnus-thread-sort-by-number
        (not gnus-thread-sort-by-most-recent-date)))

;; Enable mails search (from https://www.emacswiki.org/emacs/GnusGmail#toc22)
(with-eval-after-load 'gnus
  (require 'nnir))

;; Always confirm quit
(setq confirm-kill-emacs 'yes-or-no-p)

;; Spell-check messages
(add-hook 'message-mode-hook 'flyspell-mode)

;; Always save bookmarks
(setq bookmark-save-flag 1)

;; C/C++ indent level
(setq-default c-basic-offset 4)

;; Ignore case in autocomplete
(setq completion-ignore-case t)

;; Enable narrow to region
(put 'narrow-to-region 'disabled nil)

;; Disable VC mode
(setq vc-handled-backends nil)

;; Setup stuff on macOS
(when-system darwin
  ;; Change behavior of left command key
  (setq mac-command-modifier 'meta)

  ;; Fix dired not working
  (require 'ls-lisp)
  (setq ls-lisp-dirs-first t
	ls-lisp-use-insert-directory-program nil)
  (setq dired-listing-switches "-alhv")

  ;; Add brew binaries to PATH
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
  (add-to-list 'exec-path "/usr/local/bin")

  ;; Disable bell
  (setq ring-bell-function 'ignore)

  ;; Setup ispell
  (setq ispell-program-name "/usr/local/bin/aspell"))

;;----------------------------------------------------------------------------
;; Org Mode
;;----------------------------------------------------------------------------

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

;;----------------------------------------------------------------------------
;; Package Initialization
;;----------------------------------------------------------------------------

;; Set theme, but make comments a bit brighter (original value: #75715E)
(setq monokai-comments "#908E80")
(load-theme 'monokai t)

;; Projectile
(projectile-mode +1)
(setq projectile-mode-line-function
      (lambda ()
	(format " P[%s]" (projectile-project-name))))

(setq projectile-use-git-grep t)

;; Magit
(with-eval-after-load 'magit
  ;; Register all directories in Workspace
  (add-to-list 'magit-repository-directories '("~/Workspace/" . 2))

  ;; Always confirm with yes/no when discarding changes
  (setq magit-slow-confirm t)

  ;; Remove ":" from magit buffer names to search them more easily
  (setq magit-buffer-name-format
	(replace-regexp-in-string ":" "" magit-buffer-name-format)))

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
(setq avy-style 'words)

;; Elpy
(elpy-enable)

;; Golang
(require 'go-mode)

;; Add go binaries
(add-to-list 'exec-path "~/Workspace/go/bin")

;; Setup GOPATH
(setenv "GOPATH" "~/Workspace/go")

;; Setup company-go
(add-hook 'go-mode-hook (lambda ()
			  (require 'company-go)
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)))

;; Fix https://github.com/dominikh/go-mode.el/issues/286
(advice-add 'godef--call :around
	    (lambda (fn &rest args)
	      (let ((default-directory "~"))
		(apply fn args))))

;;----------------------------------------------------------------------------
;; Custom Functions
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
;; Keybindings
;;----------------------------------------------------------------------------

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x C-d") 'dired-jump)

(global-set-key (kbd "C-h a") 'apropos)
(global-set-key (kbd "C-o") 'flymake-goto-next-error)
(global-set-key (kbd "C-j") 'avy-goto-char-timer)
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
(global-set-key (kbd "M-i") 'imenu)
(global-set-key (kbd "M-j") 'mode-line-other-buffer)
(global-set-key (kbd "M-<backspace>") 'goto-last-edit)
(when-system darwin
  (global-set-key (kbd "M-`") 'other-frame))

(global-set-key (kbd "C-c w") 'swap-window-pair-buffers)
(global-set-key (kbd "C-c d") 'duplicate-line)
(global-set-key (kbd "C-c f") 'flymake-mode)
(global-set-key (kbd "C-c c") 'projectile-find-file)
(global-set-key (kbd "C-c k") 'kill-current-buffer)
(global-set-key (kbd "C-c j") 'json-pretty-print-dwim)
(global-set-key (kbd "C-c i") 'indent-region)
(global-set-key (kbd "C-c e e") 'eval-buffer)
(global-set-key (kbd "C-c e i") 'edit-init)
(global-set-key (kbd "C-c e r") 'rename-file-buffer)
(global-set-key (kbd "C-c e d") 'debbugs-gnu)
(global-set-key (kbd "C-c e p") 'print-buffer-file-name)
(global-set-key (kbd "C-c e o") 'open-file-external)
(global-set-key (kbd "C-c q") 'quick-calc)
(global-set-key (kbd "C-c b") 'create-scratch-buffer)
(global-set-key (kbd "C-c m") 'kill-ring-save-whole-buffer)
(global-set-key (kbd "C-c z") 'apropos)
(global-set-key (kbd "C-c s SPC") 'spotify-playpause)
(global-set-key (kbd "C-c s s") 'spotify-next)
(global-set-key (kbd "C-c s p") 'spotify-previous)
(when-system gnu/linux
  (global-set-key (kbd "C-c s c") 'spotify-current))

(global-set-key (kbd "C-c o a") 'org-agenda)
(global-set-key (kbd "C-c o d") 'dired-org-agenda)
(global-set-key (kbd "C-c o r") 'org-archive-to-archive-sibling)
(global-set-key (kbd "C-c o t") 'org-force-cycle-archived)

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
(define-key go-mode-map (kbd "M-.") 'godef-jump)
(define-key global-map (kbd "M-'") iso-transl-ctl-x-8-map)

;;----------------------------------------------------------------------------
;; Remove default keybindings
;;----------------------------------------------------------------------------

;; Disable some default keys that get hit by accident

(global-unset-key (kbd "C-x f"))
(global-unset-key (kbd "M-;"))
(global-unset-key (kbd "M-k"))
(global-unset-key (kbd "C-z"))

(define-key org-mode-map (kbd "C-c [") nil)
(define-key org-mode-map (kbd "C-'") nil)
(define-key shell-mode-map (kbd "C-c C-l") nil)
(define-key elpy-mode-map (kbd "C-c C-c") nil)
(define-key elpy-mode-map (kbd "<C-return>") nil)
(define-key python-mode-map (kbd "C-c C-c") nil)

;;----------------------------------------------------------------------------
;; Cleanup
;;----------------------------------------------------------------------------

;; Restore GC threshold
(setq gc-cons-threshold gc-default-threshold)
