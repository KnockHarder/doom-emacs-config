;;; lang/lsp-jdt/lsp-session-functions.el

;;;###autoload
(defun revert-lsp-session-buffer ()
  (interactive)
  (let ((inhibit-read-only t)
        (session (lsp-session)))
    (erase-buffer)
    (--each (lsp-session-folders session)
      (widget-create
       `(tree-widget
         :tag ,(propertize it 'face 'font-lock-keyword-face)
         :open t
         ,@(->> session
                (lsp-session-folder->servers)
                (gethash it)
                (-map 'lsp--render-workspace))))))
  (goto-char (point-min)))
(bind-key "g" #'revert-lsp-session-buffer lsp-browser-mode-map)
