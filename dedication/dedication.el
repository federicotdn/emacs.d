(defun toggle-window-dedicated ()
  "Toggles the selected window's dedicated flag."
  (interactive)
  (set-window-dedicated-p (get-buffer-window) (not (window-dedicated-p)))
  )


(defun bypass-window-dedicated-wrapper (fn &rest arguments)
  "Temporarily disable the selected window's dedicated flag and call function."
  (let
      (
       (previous-flag (window-dedicated-p))
       (window (get-buffer-window))
       )
    (set-window-dedicated-p window nil)
    (apply fn arguments)
    (set-window-dedicated-p window previous-flag)
    )
  )

(defun bypass-window-dedicated (fn)
  "Enable a function to bypass the selected window's dedicated flag."
  (advice-add fn :around #'bypass-window-dedicated-wrapper)
  )

(defun enable-dedication-mode-line ()
  "Modify the mode line to show an exclamation mark on dedicated windows."
  (setq mode-line-front-space
	(cons
	 '(:eval (if (window-dedicated-p) " !" nil))
	 mode-line-front-space)
	)
  )

(provide 'dedication)
