(defun visualize-undo ()
  (interactive)
  (let
      ((buffer (generate-new-buffer "visual-undo"))
       (target-undo-list buffer-undo-list))
    (with-current-buffer buffer
      (visual-undo-mode)
      (setq-local undo-list target-undo-list)
      (visual-mode-init))
    (pop-to-buffer buffer)))

(define-derived-mode visual-undo-mode special-mode "Visual-Undo"
  "Visual undo mode."
  (make-local-variable 'undo-list))

(defun visual-mode-init ()
  (edit-buffer
   (dolist (item undo-list)
     (when (listp item)
       (let
	   ((first (car item)))
	 (cond ((integerp first)
		(insert (format "del %s\n" first)))
	       ((stringp first)
		(insert (format "ins %s\n" first)))))))))

(defmacro edit-buffer (&rest body)
  `(let ((inhibit-read-only t))
     ,@body))

(defun visual-undo--message-undo-list ()
  "Show the undo list."
  (interactive)
  (message "%s" undo-list))
