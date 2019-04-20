;; utils.el --- misc. utility functions -*- lexical-binding: t; -*-

(defun url-encode-dwim (start end)
  "Parse a URL contained in buffer substring START - END and url-quote
its querystring component, if it has one. The results are inserted
back on the current buffer."
  (interactive "r")
  (unless (use-region-p)
    (user-error "Region is not active"))
  (let* ((text (buffer-substring start end))
	 (url-data (url-generic-parse-url text))
	 (path (car (url-path-and-query url-data)))
	 (querystring (cdr (url-path-and-query url-data)))
	 (new-querystring ""))
    (when (> (length querystring) 0)
      ;; `url-parse-query-string' returns list of conses, one per
      ;; parameter, in reverse order
      (dolist (item (reverse (url-parse-query-string querystring)))
	(when (> (length new-querystring) 0)
	  (setq new-querystring (concat new-querystring "&")))
	(setq new-querystring (concat new-querystring
				      (car item) "=" (url-hexify-string (cadr item)))))
      (setf (url-filename url-data) (concat path "?" new-querystring))
      (save-excursion
	(delete-region start end)
	(insert (url-recreate-url url-data))))))

(defun parse-timestamp ()
  "Read date and time from UNIX timestamp in region."
  (interactive)
  (let* ((selection (buffer-substring-no-properties (mark) (point)))
	 (timestamp (string-to-number selection)))
    (if (= timestamp 0)
	(user-error "Selected value is not an integer value")
      (message (format-time-string "%B %e, %Y - %T (UTC)" timestamp t)))))
