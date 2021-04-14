;; init.el  -*- lexical-binding: t; -*-
;; Requires: Emacs 26+

;;----------------------------------------------------------------------------
;; GC Config
;;----------------------------------------------------------------------------

;; Set GC threshold to a large value during init
(defconst gc-default-threshold gc-cons-threshold)
(setq gc-cons-threshold (* gc-default-threshold 100))

;;----------------------------------------------------------------------------
;; Base Initialization
;;----------------------------------------------------------------------------

;; Initialize package management and custom variables
(setq custom-file "~/.emacs.d/init-package.el")
(load custom-file)

;; Enable delete selection mode
(delete-selection-mode)

;; Disable tool bar, scroll bar and menu bar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(scroll-bar-mode -1)
(menu-bar-mode -1)

;; More extensive apropos searches
(setq apropos-do-all t)

;; Show column number
(column-number-mode)

;; Customize scratch buffer
(setq initial-scratch-message nil)
(setq initial-major-mode 'fundamental-mode)

;; IDO
(ido-mode)
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
        "magit-process*"
        "magit-revision*"
        "magit-reflog*"))

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
(show-paren-mode)

;; Insert matching parenthesis
(electric-pair-mode)

;; Indent automatically on RET
(electric-indent-mode)

;; Save position in buffer
(save-place-mode)

;; Visual line mode when editing Markdown files
(add-hook 'markdown-mode-hook 'visual-line-mode)

;; Use markdown-mode for MDX
(add-to-list 'auto-mode-alist '("\\.mdx\\'" . markdown-mode))

;; Dired
(setq dired-listing-switches "-alhv --group-directories-first")
(setq dired-auto-revert-buffer t)

;; Dired-x
(require 'dired-x)

;; Set up uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Highlight long lines in python-mode
(require 'whitespace)
(setq-default whitespace-style '(face tabs lines-tail trailing)
              whitespace-line-column 88)

(add-hook 'python-mode-hook 'whitespace-mode)
(add-hook 'python-mode-hook 'highlight-indentation-mode)

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

;; Make scrolling quicker
(setq auto-window-vscroll nil)

;; Dont jump when scrolling by line
(setq scroll-conservatively 10)

;; Indent with spaces
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; secret values
(load "~/Dropbox/emacs/secrets.el" t t)

;; Print yank pointer index after yank-pop
(advice-add 'yank-pop :after
            (lambda (&rest r)
              (unless (window-minibuffer-p)
                (let* ((ring-len (length kill-ring))
                       (pos (- ring-len (length kill-ring-yank-pointer) -1)))
                  (message "Yanked element %d of %d" pos ring-len)))))

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

(tempo-define-template "python-pprint"
                       '("from pprint import pprint" n>
                         "pprint(" (P "Expression: ") ")")
                       "pprint")

(tempo-define-template "python-traceback"
                       '("import traceback; traceback.print_stack()")
                       "traceback")

;; Allow hippie-expand to complete tempo tags
(defun try-tempo-complete-tag (old)
  (unless old
    (tempo-complete-tag)))

(add-to-list 'hippie-expand-try-functions-list 'try-tempo-complete-tag)

;; JS indent level
(setq js-indent-level tab-width)

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

;; Ignore case in autocomplete
(setq completion-ignore-case t)

;; Hide VC mode line information
(setq-default mode-line-format (remove '(vc-mode vc-mode) mode-line-format))

;; Add a newline at the end of file on save
(setq require-final-newline t)

;; Disable bell
(setq ring-bell-function 'ignore)

;; Setup stuff on macOS
(when (eq system-type 'darwin)
  ;; Change behavior of left command key
  (setq mac-command-modifier 'meta)

  ;; Add brew binaries to PATH
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
  (add-to-list 'exec-path "/usr/local/bin")

  ;; Setup ispell
  (setq ispell-program-name "/usr/local/bin/aspell")

  ;; Fix GPG problem
  (setq epa-pinentry-mode 'loopback))

;; Setup stuff on macOS and Windows
(when (member system-type '(darwin windows-nt))
  ;; Better dired
  (require 'ls-lisp)
  (setq ls-lisp-dirs-first t
        ls-lisp-use-insert-directory-program nil)
  (setq dired-listing-switches "-alhv"))

;; Isearch show match count (Emacs 27+)
(setq isearch-lazy-count t)

;; Enable So Long mode (Emacs 27+)
(global-so-long-mode 1)

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
(setq org-tag-faces '(("imp" . (:foreground "red" :weight bold))))

;; Configure Babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (emacs-lisp . t)
   (shell . t)
   (verb . t)))

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

;; Align tags further right
(setq org-tags-column 85)

;;----------------------------------------------------------------------------
;; Package Initialization
;;----------------------------------------------------------------------------

;; Set theme, but make comments a bit brighter (original value: #75715E)
(setq monokai-comments "#908E80")
(load-theme 'monokai t)

;; Projectile
(projectile-mode)

(setq projectile-mode-line-function
      (lambda ()
        (format " P[%s]" (projectile-project-name))))

(setq projectile-use-git-grep t)

;; Magit
(with-eval-after-load 'magit
  ;; Always confirm with yes/no when discarding changes
  (setq magit-slow-confirm t)

  ;; Remove ":" from magit buffer names to search them more easily
  (setq magit-buffer-name-format
        (replace-regexp-in-string ":" "" magit-buffer-name-format)))

;; Company
(add-hook 'after-init-hook 'global-company-mode)

;; flymake-shellcheck
(add-hook 'sh-mode-hook 'flymake-shellcheck-load)

;; Eglot
(setq pyls-binary "~/Applications/bin/pyls")

(defun python-contact-venv (interactive?)
  "Custom Eglot LSP server contact function for Python + virtual environments."
  (unless interactive?
    (user-error "Contact function can only be used interactively"))
  (if (bound-and-true-p pyvenv-virtual-env)
      (unless (yes-or-no-p (format "Current virtual environment is: '%s', continue? "
                                   pyvenv-virtual-env))
        (user-error "Operation cancelled by user"))
    (call-interactively #'pyvenv-activate))
  (list pyls-binary))

(require 'eglot)
(add-to-list 'eglot-server-programs '(python-mode . python-contact-venv))

;; Set default env name for pyvenv
(setq pyvenv-default-virtual-env-name "env")

;; Configure Verb package
(setq verb-auto-kill-response-buffers t)

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(add-to-list 'verb-content-type-handlers '(".+/yaml" yaml-mode))

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

(defun backward-delete-word ()
  "Delete (at most) a word backwards, avoid changing the current line.
If the current line is empty, call `backward-delete-char'."
  (interactive)
  (if (zerop (current-column))
      (call-interactively #'backward-delete-char)
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
  "Edit the user Emacs initialization file."
  (interactive)
  (find-file (concat user-emacs-directory "init.el")))

(defun duplicate-line ()
  "Duplicate a line, and move point to it (maintain current column)."
  (interactive)
  (let ((val (buffer-substring (line-beginning-position) (line-end-position))))
    (save-excursion
      (move-end-of-line 1)
      (newline)
      (insert val)))
  (next-line))

(defun dired-org-agenda ()
  "Open `org-directory' with `dired'."
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
  (message "Buffer copied to kill ring"))

(defun unpropertize-buffer ()
  "Remove all text properties from current buffer."
  (interactive)
  (set-text-properties (point-min) (point-max) nil))

(defun kill-active-region ()
  "Invoke `kill-region' only if region is active."
  (interactive)
  (when (use-region-p)
    (call-interactively #'kill-region)))

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

(defun lock-screen ()
  "Lock the OS screen."
  (interactive)
  (if (eq system-type 'darwin)
      (call-process "pmset" nil nil nil "displaysleepnow")
    (call-process "gnome-screensaver-command" nil nil nil "--lock")))

(defun clean-mark-ring ()
  "Remove all markers with position 1 from `mark-ring'."
  (interactive)
  (setq mark-ring
        (seq-filter (lambda (m)
                      (not (= (marker-position m) 1)))
                    mark-ring)))

(defun pytest-run-test ()
  "Run a Python test using pytest and `compilation-mode'."
  (interactive)
  (let* ((buffer-name (format "*pytest %s*"
                              (projectile-project-name)))
         (function-name (which-function))
         (buf (compile (format "pytest -s -vvv %s%s"
                               (buffer-file-name)
                               (if function-name
                                   (format "::%s" function-name)
                                 "")))))
    (ignore-errors (kill-buffer buffer-name))
    (with-current-buffer buf
      (rename-buffer buffer-name))))

(defun black-format-file ()
  "Format a Python file using black."
  (interactive)
  (when (buffer-modified-p)
    (user-error "Buffer contains unsaved changes"))
  (call-process "black" nil nil nil (buffer-file-name))
  (revert-buffer t t))

;;----------------------------------------------------------------------------
;; Keybindings
;;----------------------------------------------------------------------------

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x s") 'save-buffer)
(global-set-key (kbd "C-x C-d") 'dired-jump)

(global-set-key (kbd "C-h a") 'apropos)
(global-set-key (kbd "C-o") 'flymake-goto-next-error)
(global-set-key (kbd "C-w") 'kill-active-region)
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
(global-set-key (kbd "M-'") iso-transl-ctl-x-8-map)
(when (eq system-type 'darwin)
  (global-set-key (kbd "M-`") 'other-frame))

(global-set-key (kbd "C-c d") 'duplicate-line)
(global-set-key (kbd "C-c f") 'flymake-mode)
(global-set-key (kbd "C-c c") 'projectile-find-file)
(global-set-key (kbd "C-c k") 'kill-current-buffer)
(global-set-key (kbd "C-c j") 'json-pretty-print-dwim)
(global-set-key (kbd "C-c i") 'indent-region)
(global-set-key (kbd "C-c e i") 'edit-init)
(global-set-key (kbd "C-c e r") 'rename-file-buffer)
(global-set-key (kbd "C-c e d") 'debbugs-gnu)
(global-set-key (kbd "C-c e p") 'print-buffer-file-name)
(global-set-key (kbd "C-c e l") 'lock-screen)
(global-set-key (kbd "C-c y r") 'eglot-rename)
(global-set-key (kbd "C-c y t") 'pytest-run-test)
(global-set-key (kbd "C-c y b") 'black-format-file)
(global-set-key (kbd "C-c q") 'quick-calc)
(global-set-key (kbd "C-c m") 'kill-ring-save-whole-buffer)
(global-set-key (kbd "C-c u") 'unpropertize-buffer)
(global-set-key (kbd "C-c o a") 'org-agenda)
(global-set-key (kbd "C-c o d") 'dired-org-agenda)
(global-set-key (kbd "C-c s SPC") 'spotify-playpause)
(global-set-key (kbd "C-c s s") 'spotify-next)
(global-set-key (kbd "C-c s p") 'spotify-previous)
(global-set-key (kbd "C-c s c") 'spotify-current)

(global-set-key (kbd "ESC ESC ESC") 'keyboard-quit)
(global-set-key [remap dabbrev-expand] 'hippie-expand)

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(define-key shell-mode-map (kbd "C-r") 'comint-history-isearch-backward-regexp)
(define-key shell-mode-map (kbd "C-l") 'goto-end-clear-screen)
(define-key python-mode-map (kbd "M-[") 'python-indent-shift-left)
(define-key python-mode-map (kbd "M-]") 'python-indent-shift-right)

(define-key org-mode-map (kbd "M-n") 'outline-next-visible-heading)
(define-key org-mode-map (kbd "M-p") 'outline-previous-visible-heading)
(define-key org-mode-map (kbd "C-c o r") 'org-archive-to-archive-sibling)
(define-key org-mode-map (kbd "C-c o t") 'org-force-cycle-archived)

;;----------------------------------------------------------------------------
;; Remove default keybindings
;;----------------------------------------------------------------------------

;; Disable some default keys that get hit by accident
(global-unset-key (kbd "C-x f"))
(global-unset-key (kbd "C-x C-n"))
(global-unset-key (kbd "M-;"))
(global-unset-key (kbd "M-k"))
(global-unset-key (kbd "M-t"))
(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-t"))

(define-key org-mode-map (kbd "C-c [") nil)
(define-key org-mode-map (kbd "C-'") nil)
(define-key python-mode-map (kbd "C-c C-c") nil)
(define-key c-mode-map (kbd "M-j") nil)

;;----------------------------------------------------------------------------
;; Per-PC configuration file
;;----------------------------------------------------------------------------

;; local.el is gitignore'd
(load "~/.emacs.d/local.el" t t)

;;----------------------------------------------------------------------------
;; Cleanup
;;----------------------------------------------------------------------------

;; Restore GC threshold
(setq gc-cons-threshold gc-default-threshold)
