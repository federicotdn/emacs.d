;;; rel.el --- Cycle through recurringly edited lines -*- lexical-binding: t; -*-

(setq rel--rel-table -1)

(defvar rel--rel-table-global)

(defun rel-cycle-lines ()
  (interactive)
  (message "%s" rel--rel-table))

(defun rel--after-change (beg end length)
  (add-to-list rel--rel-table (count-lines 1 end) t))

;;;###autoload
(define-minor-mode rel-mode
  "Cycle through recurringly edited lines."
  :ligher nil
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-.") 'rel-cycle-lines)
	    map)
  (cond
   (rel-mode (rel--mode-on))
   (t (rel--mode-off))))

(defun rel--mode-on ()
  (setq-local rel--rel-table nil)
  (add-hook 'after-change-functions #'rel--after-change nil t)
  (message "rel mode on"))

(defun rel--mode-off ()
  (kill-local-variable 'rel--rel-table)
  (remove-hook 'after-change-functions #'rel--after-change t)
  (message "rel mode off"))

;;; rel.el ends here
