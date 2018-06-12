(defun dedication-toggle-window-dedicated ()
  "Toggles the selected window's dedicated flag."
  (interactive)
  (set-window-dedicated-p (get-buffer-window) (not (window-dedicated-p)))
  )


(defun dedication-bypass-window-dedicated-wrapper (fn &rest arguments)
  "Temporarily disable the selected window's dedicated flag and call function."
  (let
      (
       (previous-flag (window-dedicated-p))
       (window (get-buffer-window))
       )
    (set-window-dedicated-p window nil)
    (unwind-protect (apply fn arguments) (set-window-dedicated-p window previous-flag))
    )
  )

(defun dedication-bypass-window-dedicated (fn)
  "Enable a function to bypass the selected window's dedicated flag."
  (advice-add fn :around #'dedication-bypass-window-dedicated-wrapper)
  )

(setq dedication-mode-line-indicator " !")

(defun dedication-enable-mode-line-indicator ()
  "Modify the mode line to show an exclamation mark on dedicated windows."
  (setq mode-line-front-space
	(cons
	 '(:eval (if (window-dedicated-p) dedication-mode-line-indicator nil))
	 mode-line-front-space)
	)
  )

(provide 'dedication)
