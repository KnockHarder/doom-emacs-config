;;; lang/lsp-jdt/config.el

(add-to-list 'auto-mode-alist '("\\.java\\'" . java-ts-mode))

(defcustom lsp-vc-branch-worksapce-roots-file nil
  "File to save the workspace roots for project branches."
  :type 'string)
(defvar lsp-vc-branch-worksapce-roots-map (make-hash-table :test 'equal))

(defun lsp-vc-branch-worksapce-roots-file-save ()
  "Save workspace directory paths to file for current branch."
  (if (and lsp-vc-branch-worksapce-roots-file
           (file-writable-p lsp-vc-branch-worksapce-roots-file))
      (with-temp-file lsp-vc-branch-worksapce-roots-file
        (insert (prin1-to-string lsp-vc-branch-worksapce-roots-map)))
    (message "Cannot save workspace roots to file: %s" lsp-vc-branch-worksapce-roots-file)))

(defun lsp-vc-branch-worksapce-roots-file-load ()
  "Load workspace directory paths from file for all branches."
  (when (and (not (null lsp-vc-branch-worksapce-roots-file))
             (file-readable-p lsp-vc-branch-worksapce-roots-file))
    (with-temp-buffer
      (insert-file-contents lsp-vc-branch-worksapce-roots-file)
      (setq lsp-vc-branch-worksapce-roots-map (read (current-buffer))))))

(defun set-up-java-lsp()
  (when (or (null lsp-vc-branch-worksapce-roots-file)
            (not (file-readable-p lsp-vc-branch-worksapce-roots-file)))
    (setq lsp-vc-branch-worksapce-roots-file
          (expand-file-name "branch-workspace-roots.el" (file-name-parent-directory lsp-java-workspace-dir)))
    (lsp-vc-branch-worksapce-roots-file-load))
  (if (string-suffix-p ".cache/" (file-name-parent-directory (buffer-file-name)))
      (let* ((cache-dir (file-name-parent-directory (buffer-file-name)))
             (workspace-dir (file-name-parent-directory cache-dir)))
        (setq-local lsp-java-workspace-dir workspace-dir
                    lsp-java-workspace-cache-dir cache-dir))
    (when-let* ((branch (magit-get-current-branch))
                (base-dir (file-name-parent-directory lsp-java-workspace-dir))
                (workspace-dir (expand-file-name (concat branch "/") base-dir)))
      (setq-local lsp-java-workspace-dir workspace-dir)
      (setq-local lsp-java-workspace-cache-dir
                  (expand-file-name ".cache/" workspace-dir))
      (unless (->> (lsp-session-server-id->folders (lsp-session)) (gethash 'jdtls))
        (when-let ((roots (gethash branch lsp-vc-branch-worksapce-roots-map)))
          (dolist (it roots) (lsp-workspace-folders-add it))
          (puthash 'jdtls roots (lsp-session-server-id->folders (lsp-session)))))))
  (setq-local er/try-expand-list '(er/mark-treesit-parent-node))
  (add-hook 'find-file-hook #'lsp 0 t)
  (add-hook 'before-save-hook (lambda ()
                                (untabify (point-min) (point-max))) nil t)
  (add-hook 'before-save-hook #'lsp-format-changed-lines 0 t))

(use-package! lsp-java
  :commands lsp
  :custom
  (lsp-java-server-install-dir
   (expand-file-name "language-server/java/jdtls/" user-emacs-directory))
  (lsp-java-save-actions-organize-imports t)
  (lsp-java-cleanup-actions-on-save t)
  (lsp-disabled-clients '(semgrep-ls))
  (lsp-java-type-hierarchy-lazy-load t)
  (lsp-java-code-generation-use-blocks t)
  (lsp-java-compile-null-analysis-mode "automatic")
  (read-process-output-max (* 1024 (* 1024 3)))
  :bind
  (:map lsp-mode-map
        ("C-c c f" . #'lsp-format-region)
        ("C-c l d" . #'lsp-ui-doc-show)
        ("C-c l i" . #'lsp-ui-peek-find-implementation)
        ("C-c l I" . #'lsp-find-implementation)
        ("C-c l r" . #'lsp-ui-peek-find-references)
        ("C-c l R" . #'lsp-find-references)
        ("C-c l s" . #'consult-lsp-file-symbols)
        ("C-c l S" . #'lsp-ui-find-workspace-symbol)
        ("C-c l p" . #'consult-flycheck)
        ("C-c l P" . #'consult-lsp-diagnostics)
        ("C-c l c" . #'lsp-treemacs-call-hierarchy)
        ("M-RET" . #'lsp-execute-code-action))
  :config
  (define-key general-override-mode-map (kbd "C-c c f") nil)
  :hook
  (java-ts-mode . set-up-java-lsp))

(defun lsp-hover-value-at-point ()
  (when-let* ((params (lsp--text-document-position-params))
              (response (lsp-request "textDocument/hover" params))
              (contents (gethash "contents" response)))
    (when (vectorp contents)
      (setq contents (aref contents 0)))
    (gethash "value" contents)))
