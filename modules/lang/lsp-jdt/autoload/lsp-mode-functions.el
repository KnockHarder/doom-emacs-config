;;; lang/lsp-jdt/autoload/lsp-mode-functions.el

;;;###autoload
(defun lsp-copy-hover-value-at-point ()
  "Copy hover value at point to kill ring."
  (interactive)
  (if-let* ((hover-value (lsp-hover-value-at-point)))
      (progn (kill-new hover-value)
             (message "Copied hover value: %s" hover-value))
    (message "Cannot find any hover value at point")))

;;;###autoload
(defun lsp-copy-java-method-reference-without-params ()
  (interactive)
  (if-let* ((hover-value (lsp-hover-value-at-point))
            (matches (string-match "\\([A-Z][.a-zA-Z0-9]*\\)\\.\\([a-z][a-zA-Z0-9]*\\)(" hover-value))
            (class (match-string 1 hover-value))
            (method (match-string 2 hover-value))
            (reference (concat class "." method)))
      (progn (kill-new reference)
             (message "Copied method reference: %s" reference))
    (message "Cannot find any method at point")))

;;;###autoload
(defun lsp-show-java-symbol-definition-location ()
  "Get defin Maven coordinate from input-string."
  (interactive)
  (if-let* ((response (car (lsp-request "textDocument/definition" (lsp--text-document-position-params))))
            (uri (gethash "uri" response)))
      (let ((location (cond
                       ((string-prefix-p "jdt://" uri)
                        (if-let* ((mvn-group-id (and (string-match "=\\/maven.groupId=\\(/[^=]+\\)=" uri)
                                                     (match-string 1 uri)))
                                  (mvn-artifact-id (and (string-match "=\\/maven.artifactId=\\(/[^=]+\\)=" uri)
                                                        (match-string 1 uri)))
                                  (mvn-version (and (string-match "=\\/maven.version=\\(/[^=]+\\)=" uri)
                                                    (match-string 1 uri))))
                            (concat (substring mvn-group-id 1) ":"
                                    (substring mvn-artifact-id 1) ":"
                                    (substring mvn-version 1))
                          "Internal Library"))
                       ((string-prefix-p "file://" uri)
                        (string-remove-prefix "file://" uri))
                       (t (format "Unknown URI: %s" uri)))))
        (message "Definition location: %s" location))))

;;;###autoload
(defun lsp-format-changed-lines (&optional buffer)
  "Lsp-format changed lines in the buffer."
  (interactive)
  (if (not buffer)
      (setq buffer (current-buffer)))
  (when (y-or-n-p "Format the changed regions with lsp?")
    (let ((old-file (buffer-file-name buffer))
          (new-file (diff-file-local-copy buffer))
          (diff-line-regions (list)))
      (unless old-file
        (error "Buffer is not visiting a file"))
      (with-temp-buffer
        (shell-command (format "diff %s %s" old-file new-file) (current-buffer))
        (goto-char (point-min))
        (while (re-search-forward "^[0-9,]+[ac]\\([0-9,]+\\)$" nil t)
          (push (match-string 1) diff-line-regions)))
      (with-current-buffer buffer
        (let ((current-point (point))
              (current-line (progn (goto-char (point-min))
                                   1)))
          (dolist (line-region diff-line-regions)
            (let* ((lines (split-string line-region ","))
                   (start-line (string-to-number (car lines)))
                   (end-line (if (> (length lines) 1)
                                 (string-to-number (cadr lines))
                               start-line))
                   (start (progn
                            (forward-line (- start-line current-line))
                            (beginning-of-line)
                            (point)))
                   (end (progn
                          (forward-line (- end-line start-line))
                          (end-of-line)
                          (point))))
              (lsp-format-region start end)
              (setq current-line end-line)))
          (goto-char current-point))))))
