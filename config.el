;;; init.el -- init

;; key bindings and system settings
(setq doom-theme 'doom-dracula
      doom-font (font-spec :size 14)
      doom-variable-pitch-font (font-spec :size 16)
      bookmark-save-flag 1)
(setq! doom-modeline-buffer-file-name-style 'truncate-with-project)
(setq gcmh-high-cons-threshold (* 1024 (* 1024 16)))
(global-subword-mode 1)
(add-hook 'doom-init-ui-hook #'toggle-frame-maximized)
(global-set-key (kbd "s-k") 'kill-current-buffer)

;; window and frame
(defun my/open-buffer-new-maximum-frame (&optional position)
  (interactive)
  (let ((consult--buffer-display (lambda (buffer &optional norecord)
                                   (with-selected-frame (make-frame)
                                     (switch-to-buffer buffer norecord)
                                     (set-frame-parameter (selected-frame) 'fullscreen 'maximized)))))
    (consult-buffer)))
(bind-key "C-x 5 b" #'my/open-buffer-new-maximum-frame)


;; region
(use-package! expand-region
  :bind
  ("C-=" . er/expand-region))

;; version control
(use-package! magit
  :commands magit--handle-bookmark magit-get-current-branch
  :bind
  (:map magit-mode-map
        ("C-c o l" . #'browse-url))
  :hook
  (magit-status-mode . magit-save-repository-buffers)
  (magit-diff-mode . magit-save-repository-buffers)
  :config
  (setq-default magit-diff-refine-hunk 'all))

;; rime
(use-package! rime
  :custom
  (rime-show-candidate 'posframe)
  (rime-librime-root (expand-file-name "librime/" user-emacs-directory))
  (rime-emacs-module-header-root (expand-file-name "../../../include"
                                                   invocation-directory))
  (rime-user-data-dir "~/Library/Rime")
  (rime-disable-predicates '(rime-predicate-space-after-cc-p
                             rime-predicate-after-alphabet-char-p
                             rime-predicate-current-uppercase-letter-p
                             rime-predicate-prog-in-code-p
                             rime-predicate-punctuation-after-space-cc-p
                             rime-predicate-punctuation-line-begin-p))
  :config
  (add-to-list 'safe-local-variable-values
               '(eval progn (activate-rime)))
  (defun activate-rime()
    (activate-input-method default-input-method))
  (add-hook 'markdown-mode-hook 'activate-rime)
  (add-hook 'plantuml-mode-hook 'activate-rime)
  (add-hook 'git-commit-mode-hook 'activate-rime))

;; gpt config
(let* ((local-server "127.0.0.1:8080")
       (url (format "http://%s/v1/models" local-server))
       (local-key (getenv "LOCAL_GPT_KEY")))
  (request url
    :parser 'json-read
    :sync t
    :timeout 1
    :success
    (cl-function
     (lambda (&key data &allow-other-keys)
       (when-let* ((models (cdr (assoc 'data data)))
                   (ids (mapcar (lambda (m)
                                  (cdr (assoc 'id m))) models))
                   (id-not-found t)
                   (id (cl-find-if (lambda (x)
                                     (member x ids)) '("gpt-4" "gpt-3.5-turbo"))))
         (setq my/gptel-model id my/gptel-key local-key my/gptel-host local-server)
         (use-package! gptel
           :commands gptel
           :custom
           (gptel-model my/gptel-model)
           :config
           (setq-default gptel-backend (gptel-make-openai "ChatGPT"
                                         :key my/gptel-key
                                         :host my/gptel-host
                                         :stream t
                                         :models '("gpt-3.5-turbo" "gpt-4")
                                         :protocol "http"))))))
    :error
    (cl-function
     (lambda (&rest args &key data error-thrown &allow-other-keys)
       (message "My GPT: No avliable local GPT service. error: %S" error-thrown)))))

(use-package! copilot
  :commands copilot--on-doc-focus
  :hook
  (java-ts-mode . copilot-mode)
  (emacs-lisp-mode . copilot-mode)
  (protobuf-mode . copilot-mode)
  :config
  (defun copilot-complete-dwim ()
    (interactive)
    (if (eq (overlay-buffer copilot--overlay) (current-buffer))
        (copilot-accept-completion)
      (copilot-complete)))
  :bind
  (:map copilot-mode-map
        ("C-c TAB TAB" . #'copilot-complete-dwim)
        ("C-c TAB a" . #'copilot-accept-completion)
        ("C-c TAB w" . #'copilot-accept-completion-by-word)
        ("C-c TAB l" . #'copilot-accept-completion-by-line)
        ("C-c TAB h" . #'copilot-accept-completion-by-paragraph)
        ("C-c TAB n" . #'copilot-next-completion)
        ("C-c TAB p" . #'copilot-previous-completion)))

;; code
(bind-key "C-:" #'company-other-backend)
(setq project-find-functions '(project-try-vc project-projectile))
(after! vc-git
  (add-to-list 'projectile-project-root-functions #'vc-git-root))
(defcustom lsp-vc-branch-worksapce-roots-file nil
  "File to save the workspace roots for project branches."
  :type 'string)
(defvar lsp-vc-branch-worksapce-roots-map (make-hash-table :test 'equal))
(use-package! lsp
  :custom
  (lsp-modeline-code-actions-enable nil)
  :config
  (defun lsp-vc-branch-worksapce-roots-file-save ()
    (if (and lsp-vc-branch-worksapce-roots-file
             (file-writable-p lsp-vc-branch-worksapce-roots-file))
        (with-temp-file lsp-vc-branch-worksapce-roots-file
          (insert (prin1-to-string lsp-vc-branch-worksapce-roots-map)))
      (message "Cannot save workspace roots to file: %s" lsp-vc-branch-worksapce-roots-file)))
  (defun lsp-vc-branch-worksapce-roots-file-load ()
    (when (and (not (null lsp-vc-branch-worksapce-roots-file))
               (file-readable-p lsp-vc-branch-worksapce-roots-file))
      (with-temp-buffer
        (insert-file-contents lsp-vc-branch-worksapce-roots-file)
        (setq lsp-vc-branch-worksapce-roots-map (read (current-buffer))))))
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
        (kill-buffer b)))))
(use-package! lsp-ui
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-include-signature t)
  :config
  (setq lsp-ui-doc-position 'bottom
        lsp-ui-doc-max-width 100))
(use-package! python
  :commands python python-mode
  :bind
  (:map python-mode-map
        ("C-c M-}" . #'python-nav-forward-defun)
        ("C-c M-{" . #'python-nav-backward-defun)
        ))
(use-package! pipenv
  :commands pipenv-mode
  :init
  (defun my/setup-pipenv ()
    "set python paths for pipenv."
    (let ((python-path (string-trim (shell-command-to-string "pipenv run which python"))))
      (setq-local python-interpreter python-path)
      (setq-local pythonic-interpreter python-path)
      (setq-local lsp-pyright-python-executable-cmd python-path)
      (shell-command "pipenv run pip install isort pyflakes flake8")))
  :hook
  (pipenv-mode . my/setup-pipenv))

(add-to-list 'auto-mode-alist '("\\.java\\'" . java-ts-mode))
(setq-hook! 'java-ts-mode read-process-output-max (* 1024 (* 1024 3)))
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
  (add-hook 'find-file-hook #'lsp 0 t))
(add-hook! java-ts-mode #'set-up-java-lsp)
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
  (defun my/lsp-java-hover-value ()
    (when-let* ((params (lsp--text-document-position-params))
                (response (lsp-request "textDocument/hover" params))
                (contents (gethash "contents" response)))
      (when (vectorp contents)
        (setq contents (aref contents 0)))
      (gethash "value" contents)))
  (defun my/lsp-java-copy-hover-value ()
    (interactive)
    (if-let* ((hover-value (my/lsp-java-hover-value)))
        (progn (kill-new hover-value)
               (message "Copied hover value: %s" hover-value))
      (message "Cannot find any hover value at point")))
  (defun my/lsp-java-copy-method-reference-without-params ()
    (interactive)
    (if-let* ((hover-value (my/lsp-java-hover-value))
              (matches (string-match "\\([A-Z][.a-zA-Z0-9]*\\)\\.\\([a-z][a-zA-Z0-9]*\\)(" hover-value))
              (class (match-string 1 hover-value))
              (method (match-string 2 hover-value))
              (reference (concat class "." method)))
        (progn (kill-new reference)
               (message "Copied method reference: %s" reference))
      (message "Cannot find any method at point")))
  (defun my/lsp-java-show-definition-location ()
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
  (defun my/lsp-java-format-changed-lines (&optional buffer)
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
  (defun set-up-lsp-java-before-save-hooks ()
    (add-hook 'before-save-hook (lambda ()
                                  (untabify (point-min) (point-max))) nil t)
    (add-hook 'before-save-hook #'my/lsp-java-format-changed-lines 0 t))
  (add-hook 'java-ts-mode-hook #'set-up-lsp-java-before-save-hooks))
;; spell and translate
(after! spell-fu
  (when-let ((dict (spell-fu-get-ispell-dictionary "english")))
    (setq-default spell-fu-dictionaries `(,dict))))
(after! ispell
  (setq-default ispell-dictionary "english"))

(use-package! go-translate
  :commands gts-do-translate
  :custom
  (gts-translate-list '(("en" "zh")))
  :config
  (setq-default gts-default-translator
                (gts-translator
                 :picker (gts-noprompt-picker)
                 :engines (list (gts-stardict-engine)
                                (gts-google-engine)
                                (gts-youdao-dict-engine))
                 :render (gts-buffer-render))))

;; doc
(use-package! org
  :commands org-mode
  :custom
  (org-image-actual-width nil)
  :bind
  (:map org-mode-map
        ("C-c l TAB" . #'org-fold-show-subtree)
        ("C-c l <backtab>" . #'org-fold-hide-subtree))
  :config
  (defun unset-org-last-indirect-buffer (&rest REST)
    (setq org-last-indirect-buffer nil))
  (advice-add #'org-tree-to-indirect-buffer :before #'unset-org-last-indirect-buffer))
(use-package! org-tree-slide
  :commands org-tree-slide-mode
  :init
  (defvar-local org-tree-slide-pre-line-number-mode nil)
  (defun org-tree-slide-frobide-line-number ()
    (if org-tree-slide-mode
        (progn
          (setq-local org-tree-slide-pre-line-number-mode display-line-numbers-mode)
          (display-line-numbers-mode 0))
      (display-line-numbers-mode org-tree-slide-pre-line-number-mode))
    )
  :hook
  (org-tree-slide-mode . org-tree-slide-frobide-line-number))

;; browser
(setq! browse-url-browser-function #'xwidget-webkit-browse-url)

;; shell
(use-package shell
  :commands shell-mode
  :init
  (defun my/setup-shell-mode ()
    (when (and (string-match-p "\\*.*Shell Command.*\\*" (buffer-name))
               truncate-lines)
      (toggle-truncate-lines 0)))
  :hook
  (shell-mode . my/setup-shell-mode))

;; local config files
(defcustom local-config-files (list) "Local config files."
  :type '(repeat string))
(add-hook 'doom-first-buffer-hook (lambda ()
                                    (dolist (file local-config-files)
                                      (load file))))

;; buffer and file
(defun delete-file-and-kill-buffer ()
  "Kill the current buffer and delete the file it is visiting."
  (interactive)
  (if-let ((filename (buffer-file-name))
           (buffer (current-buffer)))
      (when (y-or-n-p (format
                       "Are you sure to delete current file? (y/n) : %s "
                       filename))
        (delete-file filename)
        (kill-current-buffer))
    (kill-current-buffer)))
