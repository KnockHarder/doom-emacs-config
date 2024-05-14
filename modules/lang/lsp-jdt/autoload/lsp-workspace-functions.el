;;; lang/lsp-jdt/autoload/lsp-workspace.el

;;;###autoload
(defun lsp-shutdown-workspace-and-clean-up ()
  "Shutdown language server. Then kill buffers watched and remove workspace folders for lsp."
  (interactive)
  (--when-let (pcase (lsp-workspaces)
                (`nil (user-error "There are no active servers in the current buffer"))
                (`(,workspace) (when (y-or-n-p (format "Are you sure you want to stop the server %s?"
                                                       (lsp--workspace-print workspace)))
                                 workspace))
                (workspaces (lsp--completing-read "Select server: "
                                                  workspaces
                                                  'lsp--workspace-print nil t)))
    (let ((buffers (lsp--workspace-buffers it))
          (workspace-folders (lsp--workspace-workspace-folders it))
          (activate-folders (hash-table-keys (lsp-session-folder->servers (lsp-session)))))
      (dolist (b buffers)
        (with-current-buffer b
          (when-let ((brach (magit-get-current-branch)))
            (puthash brach activate-folders lsp-vc-branch-worksapce-roots-map))))
      (lsp-vc-branch-worksapce-roots-file-save)
      (lsp-workspace-shutdown it)
      (while (lsp-workspaces)
        (sit-for 0.2))
      (dolist (folder activate-folders)
        (when (not (member folder (hash-table-keys (lsp-session-folder->servers (lsp-session)))))
          (lsp-workspace-folders-remove folder)
          ))
      (dolist (b buffers)
        (when (buffer-modified-p)
          (switch-to-buffer b))
        (kill-buffer b)))))

;;;###autoload
(defun lsp-kill-workspace-buffers ()
  "Kill all buffers watched by lsp."
  (interactive)
  (--when-let (pcase (lsp-workspaces)
                (`nil (user-error "There are no active servers in the current buffer"))
                (`(,workspace) workspace)
                (workspaces (lsp--completing-read "Select server: "
                                                  workspaces
                                                  'lsp--workspace-print nil t)))
    (dolist (b (lsp--workspace-buffers it))
      (when (buffer-modified-p)
        (switch-to-buffer b))
      (kill-buffer b))))
