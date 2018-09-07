;;; rel.el --- Cycle through recurringly edited lines -*- lexical-binding: t; -*-

(defvar rel--rel-table)

(defvar rel--rel-table-global)

(defun rel-cycle-lines ()
  (interactive)
  (message "%s" rel--rel-table))

(defun rel--after-change (beg end length)
  )

;;;###autoload
(define-minor-mode rel-mode
  "Cycle through recurringly edited lines."
  :ligher nil
  :keymap (let ((map (make-sparse-keymap)))
	    (define-key map (kbd "C-.") 'rel-cycle-lines)
	    map)
  (make-local-variable 'rel--rel-table)
  (make-local-variable 'after-change-functions)
  (add-hook 'after-change-functions #'rel--after-change nil t))

;;; rel.el ends here
