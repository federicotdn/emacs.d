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

;; General Emacs config is in init-base.el
(load "~/.emacs.d/init-base.el")

;;----------------------------------------------------------------------------
;; Org Mode
;;----------------------------------------------------------------------------

;; Disable truncate-lines when editing Org files
(add-hook 'org-mode-hook 'visual-line-mode)

;; Configure directories
(setq org-directory "~/Dropbox/org/")
(setq org-agenda-files (list org-directory))

;; Quick capture file
(setq org-default-notes-file (concat org-directory "/notes.org"))

;; TODO lists states, last state used as 'done'
(setq org-todo-keywords '((sequence "TODO" "CURRENT" "DONE")))

;; important tag
(setq org-tag-faces '(("imp" . (:foreground "red" :weight bold))))

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

;;----------------------------------------------------------------------------
;; Package Initialization
;;----------------------------------------------------------------------------

;; Set theme
(load-theme 'monokai t)

;; Projectile
(projectile-mode +1)
(setq projectile-mode-line-prefix " P")

;; Magit
(with-eval-after-load 'magit
  (add-to-list 'magit-repository-directories '("~/Workspace/" . 2)))

;; Elpy
(elpy-enable)

;; Company
(add-hook 'after-init-hook 'global-company-mode)

;; flymake-shellcheck
(require 'flymake-shellcheck)
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
(setq avy-keys '(?a ?s ?d ?f ?j ?k ?l ?\;))

;; Enable undo for EIN (Jupyter Notebooks)
(setq ein:worksheet-enable-undo t)

;;----------------------------------------------------------------------------
;; Custom Functions
;;----------------------------------------------------------------------------

(defun find-file-general (&optional arg)
  "If in a Projectile project, call projectile-find-file. Otherwise,
call ido-find-file."
  (interactive)
  (if (projectile-project-p)
      (projectile-find-file)
    (ido-find-file)))

(defun comment-really-dwim ()
  "Toggle comment on line (or region if active)."
  (interactive)
  (if (use-region-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(defun move-line-up ()
  "Move current line up."
  (interactive)
  (when (> (line-number-at-pos) 1)
    (transpose-lines 1)
    (previous-line)
    (previous-line)))

(defun move-line-down ()
  "Move current line down."
  (interactive)
  (when (< (line-number-at-pos) (count-lines (point-min) (point-max)))
    (next-line)
    (transpose-lines 1)
    (previous-line)))

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
    (error "This function only works with exactly two windows")))

(defun close-response-and-request ()
  "Close last HTTP response buffer and send a new request."
  (interactive)
  (while (get-buffer "*HTTP Response*")
    (kill-buffer "*HTTP Response*"))
  (when (= (count-windows) 1)
    (split-window-right))
  (restclient-http-send-current-stay-in-window))

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
    (if (= timestamp 0)
	(error "Selected value is not an integer value")
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
  "Print the current buffer's file path."
  (interactive)
  (let ((name (buffer-file-name)))
    (if name
	(message name)
      (error "Buffer is not visiting any file"))))

(defun thing-to-register-dwim (reg)
  "If called with negative prefix argument, prompt for register and
clear its contents. If called with prefix argument (4), prompt for a
register and save window configuration into it. If called with prefix
argument (16), prompt for a register and save frameset configuration
into it. If last executed action was defining a macro, prompt for a
register and save it there. Otherwise, if region is active, copy it to
a register. Otherwise save point position and current buffer to a
register."
  (interactive (list (register-read-with-preview
		      (if (equal current-prefix-arg '-)
			  "Delete register: "
			"Register: "))))
  (cond ((null current-prefix-arg)
	 (cond ((eq last-command 'kmacro-end-or-call-macro)
		(kmacro-to-register reg))
	       ((use-region-p)
		(copy-to-register reg (region-beginning) (region-end)))
	       (t
		(point-to-register reg))))
	((equal current-prefix-arg '-)
	 (setq register-alist (assq-delete-all reg register-alist)))
	((equal current-prefix-arg '(4))
	 (window-configuration-to-register reg))
	((equal current-prefix-arg '(16))
	 (frameset-to-register reg))))

(defun use-register-dwim (reg)
  "Prompt for a register name if called interactively, otherwise use
REG. If the selected register contains text, insert its contents into
the current buffer. If the register contains a point position (or file
query), jump to it. If the register contains a keyboard macro, execute
it. If the register contains a window or frameset configuration, apply
it."
  (interactive (list (register-read-with-preview "Register: ")))
  (let ((contents (get-register reg)))
    (if (stringp contents)
	(insert-register reg)
      (progn
	(when (markerp contents)
	  (let ((w (get-buffer-window (marker-buffer contents) t)))
	    (when w
	      (progn
		(select-frame-set-input-focus (window-frame w))
		(select-window w)))))
	(jump-to-register reg)))))

(defun rename-file-buffer ()
  "Rename the current buffer's file, and the buffer itself to match
the new file name."
  (interactive)
  (let ((current-file-name (buffer-file-name)))
    (if (and current-file-name (not (buffer-modified-p)))
	(let ((new-file-name (read-file-name "New file name:" nil current-file-name 'confirm)))
	  (if (and (not (file-exists-p new-file-name))
		   (not (get-file-buffer new-file-name)))
	      (progn
		(rename-file current-file-name new-file-name)
		(set-visited-file-name new-file-name)
		(set-buffer-modified-p nil))
	    (error "File already exists!")))
      (error "Current buffer is not visiting any file or has unsaved changes"))))

(defun import-icalendar-url (url dest)
  "Download an iCalendar file from URL (asynchronously) and convert it
to a Org mode file, using ical2orgpy. The created file will be placed
in file DEST, inside the current org-directory."
  (interactive "sEnter URL: \nsEnter filename: ")
  (unless (executable-find "ical2orgpy")
    (error "Could not find ical2orgpy executable"))
  (let ((ical-file (make-temp-file "emacs-ical"))
	(org-file (expand-file-name (concat org-directory dest))))
    (with-temp-file ical-file
      (url-insert-file-contents url))
    (if (= 0 (call-process "ical2orgpy" nil nil nil ical-file org-file))
	(message "iCal exported to: %s" org-file)
      (error "ical2orgpy process error"))
    (delete-file ical-file)))

(defun import-google-calendar ()
  "Import calendar from Google Calendar."
  (interactive)
  (import-icalendar-url gcal-url "gcal.org"))

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
  (if (use-region-p)
      (json-pretty-print (region-beginning) (region-end))
    (json-pretty-print-buffer)))

(defun goto-last-edit ()
  "Go to the last edit made in the current buffer."
  (interactive)
  (unless (or (consp buffer-undo-list)
	      (not buffer-undo-list))
    (error "Can't go to last edit: invalid undo list"))
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

(defun dired-default-directory ()
  "Open dired on the directory contained in `default-directory'."
  (interactive)
  (dired default-directory))

(define-derived-mode long-lines-mode fundamental-mode "Long-Lines"
  "Simple mode to allow editing files with very long lines."
  (setq bidi-display-reordering nil)
  (buffer-disable-undo))

;;----------------------------------------------------------------------------
;; Keybindings
;;----------------------------------------------------------------------------

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x C-d") 'dired-default-directory)

(global-set-key (kbd "C-o") 'flymake-goto-next-error)
(global-set-key (kbd "C-j") 'avy-goto-char-timer)
(global-set-key (kbd "C-;") 'comment-really-dwim)
(global-set-key (kbd "C-<") 'scroll-right)
(global-set-key (kbd "C->") 'scroll-left)
(global-set-key (kbd "C-,") 'query-replace-regexp)
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "C-M-=") 'wrap-region)
(global-set-key (kbd "C-<backspace>") 'backward-delete-word)

(global-set-key (kbd "M-l") 'ido-switch-buffer)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-i") 'imenu)
(global-set-key (kbd "M-s h c") 'clear-all-highlights)
(global-set-key (kbd "M-<backspace>") 'goto-last-edit)

(global-set-key (kbd "C-c w s") 'swap-window-pair-buffers)
(global-set-key (kbd "C-c w f") 'fit-window-to-buffer)
(global-set-key (kbd "C-c w w") 'balance-windows)
(global-set-key (kbd "C-c d") 'duplicate-line)
(global-set-key (kbd "C-c n") 'display-line-numbers-mode)
(global-set-key (kbd "C-c f") 'flymake-mode)
(global-set-key (kbd "C-c s SPC") 'spotify-playpause)
(global-set-key (kbd "C-c s s") 'spotify-next)
(global-set-key (kbd "C-c s p") 'spotify-previous)
(global-set-key (kbd "C-c s c") 'spotify-current)
(global-set-key (kbd "C-c c") 'find-file-general)
(global-set-key (kbd "C-c k") 'kill-current-buffer)
(global-set-key (kbd "C-c j") 'json-pretty-print-dwim)
(global-set-key (kbd "C-c i") 'indent-region)
(global-set-key (kbd "C-c h") 'shell-with-name)
(global-set-key (kbd "C-c e e") 'eval-buffer)
(global-set-key (kbd "C-c e i") 'edit-init)
(global-set-key (kbd "C-c e r") 'rename-file-buffer)
(global-set-key (kbd "C-c e d") 'debbugs-gnu)
(global-set-key (kbd "C-c e p") 'print-buffer-file-name)
(global-set-key (kbd "C-c t") 'parse-timestamp)
(global-set-key (kbd "C-c q") 'quick-calc)
(global-set-key (kbd "C-c b") 'create-scratch-buffer)
(global-set-key (kbd "C-c <tab>") 'ibuffer)
(global-set-key (kbd "C-c m") 'kill-ring-save-whole-buffer)
(global-set-key (kbd "C-c r j") 'use-register-dwim)
(global-set-key (kbd "C-c r r") 'thing-to-register-dwim)
(global-set-key (kbd "C-c z") 'apropos)

(global-set-key (kbd "C-c o c") 'org-capture)
(global-set-key (kbd "C-c o a") 'org-agenda)
(global-set-key (kbd "C-c o d") 'dired-org-agenda)
(global-set-key (kbd "C-c o s") 'org-sort)
(global-set-key (kbd "C-c o r") 'org-archive-to-archive-sibling)
(global-set-key (kbd "C-c o t") 'org-force-cycle-archived)
(global-set-key (kbd "C-c o g") 'import-google-calendar)

(global-set-key (kbd "ESC ESC ESC") 'keyboard-quit)

(global-set-key [remap dabbrev-expand] 'hippie-expand)

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(define-key restclient-mode-map (kbd "C-c C-v") 'close-response-and-request)
(define-key shell-mode-map (kbd "C-r") 'comint-history-isearch-backward-regexp)
(define-key shell-mode-map (kbd "C-l") 'goto-end-clear-screen)
(define-key shell-mode-map (kbd "C-M-l") 'comint-clear-buffer)

(define-key org-mode-map (kbd "M-n") 'outline-next-visible-heading)
(define-key org-mode-map (kbd "M-p") 'outline-previous-visible-heading)
(define-key global-map (kbd "M-'") iso-transl-ctl-x-8-map)

;; Free keys:
;; C-c SPC
;; C-.
;; M-j
;; M-[
;; M-]

;;----------------------------------------------------------------------------
;; Remove default keybindings
;;----------------------------------------------------------------------------

;; Disable some default keys that get hit by accident

(global-unset-key (kbd "C-x f"))
(global-unset-key (kbd "M-;"))

(define-key elpy-mode-map (kbd "<C-return>") nil)
(define-key elpy-mode-map (kbd "C-c C-c") nil)
(define-key org-mode-map (kbd "C-c [") nil)
(define-key org-mode-map (kbd "C-'") nil)
(define-key shell-mode-map (kbd "C-c C-l") nil)
(define-key python-mode-map (kbd "C-c C-c") nil)

;;----------------------------------------------------------------------------
;; Cleanup
;;----------------------------------------------------------------------------

;; Restore GC threshold
(setq gc-cons-threshold gc-default-threshold)
