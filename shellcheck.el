;;; shellcheck.el --- A bash/sh Flymake backend  -*- lexical-binding: t; -*-
(defvar-local shellcheck--flymake-proc nil)

(defun shellcheck-flymake (report-fn &rest _args)
  (unless (executable-find
           "shellcheck") (error "Could not find shellcheck executable"))

  (when (process-live-p shellcheck--flymake-proc)
    (kill-process shellcheck--flymake-proc))

  (let* ((source (current-buffer))
	 (filename (buffer-file-name source)))
    (save-restriction
      (widen)
      (setq
       shellcheck--flymake-proc
       (make-process
        :name "shellcheck-flymake" :noquery t :connection-type 'pipe
        :buffer (generate-new-buffer " *shellcheck-flymake*")
        :command (list "shellcheck" "-f" "gcc" filename)
        :sentinel
        (lambda (proc _event)
          (when (eq 'exit (process-status proc))
            (unwind-protect
                (if (with-current-buffer source (eq proc shellcheck--flymake-proc))
                    (with-current-buffer (process-buffer proc)
                      (goto-char (point-min))
                      (cl-loop
                       while (search-forward-regexp
                              "^.+?:\\([0-9]+\\):\\([0-9]+\\): \\(.*\\): \\(.*\\)$"
                              nil t)
		       for severity = (match-string 3)
                       for msg = (match-string 4)
                       for (beg . end) = (flymake-diag-region
                                          source
                                          (string-to-number (match-string 1))
					  (string-to-number (match-string 2)))
                       for type = (cond ((string= severity "note") :note)
					((string= severity "warning") :warning)
					(t :error))
                       collect (flymake-make-diagnostic source
                                                        beg
                                                        end
                                                        type
                                                        msg)
                       into diags
                       finally (funcall report-fn diags)))
                  (flymake-log :warning "Canceling obsolete check %s"
                               proc))
              (kill-buffer (process-buffer proc))))))))))

(defun shellcheck-setup-flymake-backend ()
  (add-hook 'flymake-diagnostic-functions 'shellcheck-flymake nil t))

(add-hook 'sh-mode-hook 'shellcheck-setup-flymake-backend)
