(defun visualize-undo ()
  (interactive)
  (let
      ((buffer (generate-new-buffer "visual-undo"))
       (target-undo-list buffer-undo-list))
    (with-current-buffer buffer
      (progn
	(visual-undo-mode)
	(setq-local undo-list target-undo-list)))
    (pop-to-buffer buffer)))

(define-derived-mode visual-undo-mode special-mode "Visual-Undo"
  "Visual undo mode."
  (make-local-variable 'undo-list))

(defun visual-undo--message-undo-list ()
  "Show the undo list."
  (interactive)
  (message "%s" undo-list))
