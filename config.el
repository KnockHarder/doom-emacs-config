;;; init.el -- init

;; key bindings and system settings
(setq doom-theme 'doom-dracula
      doom-font (font-spec :size 14)
      doom-variable-pitch-font (font-spec :size 16))
(setq! doom-modeline-buffer-file-name-style 'truncate-with-project)
(setq gcmh-high-cons-threshold (* 1024 (* 1024 16)))
(global-subword-mode 1)
(add-to-list 'default-frame-alist '(width . 120))
(add-to-list 'default-frame-alist '(height . 40))
(add-hook 'doom-init-ui-hook #'toggle-frame-maximized)
(global-set-key (kbd "s-k") 'kill-current-buffer)

;; window and frame
(use-package! ace-window
  :commands ace-window
  :custom
  (aw-scope 'frame)
  :bind
  ("C-x 4 o" . ace-window))
(defun my/open-side-window (buf &optional position)
  (interactive "bSelect Buffer:")
  (unless position
    (let* ((pos-str (completing-read "Choose a position: "
                                     '(left top right bottom)))
           )
      (setq position (cond
                      ((string-equal pos-str "left") 'left)
                      ((string-equal pos-str "top") 'top)
                      ((string-equal pos-str "right") 'right)
                      ((string-equal pos-str "bottom") 'bottom)))
      ))
  (let* ((widown-width (if (memq position  (list 'left 'right)) 35 nil))
         (window-height (if (memq position (list 'top 'bottom)) 10 nil))
         (win (display-buffer buf `(display-buffer-in-side-window
                                    .
                                    (
                                     (side . ,position)
                                     (window-width . ,widown-width)
                                     (window-height . ,window-height)
                                     (dedicated . t)
                                     (no-other-window . t))))))
    (set-window-parameter win 'no-other-window t)
    win))
(defun my/delete-other-not-side-windows ()
  (interactive)
  (let* ((main-window (window-main-window))
         (main-window-buffer (window-buffer main-window))
         (window (selected-window))
         (buffer (current-buffer)))
    (when (and main-window-buffer
               (not (eq main-window window)))
      (set-window-buffer main-window buffer)
      (select-window (window-main-window))))
  (mapc #'delete-window
        (delq (selected-window)
              (seq-filter (lambda (window)
                            (not (window-parameter window 'window-side)))
                          (window-list)))))
(global-set-key (kbd "C-x 1") #'my/delete-other-not-side-windows)
(defun my/open-buffer-new-maximum-frame (buffer &optional position)
  (interactive "bSelect Buffer:")
  (with-selected-frame (make-frame)
    (switch-to-buffer buffer)
    (set-frame-parameter (selected-frame) 'fullscreen 'maximized)))
(bind-key "C-x 5 b" #'my/open-buffer-new-maximum-frame)
(defun my/open-side-info-windows ()
  "Open info buffers at sides."
  (interactive)
  (when-let ((buffer (messages-buffer))
             (window (my/open-side-window buffer 'left)))
    (with-current-buffer buffer
      (toggle-truncate-lines 0)
      (display-line-numbers-mode 1)
      (set-window-point window (point-max))))
  (when-let ((name (bound-and-true-p flycheck-error-list-buffer))
             (buffer (get-buffer name)))
    (my/open-side-window buffer 'bottom))
  (when-let* ((buffer (get-buffer "*lsp-log*"))
              (window (my/open-side-window buffer 'top)))
    (with-current-buffer buffer
      (set-window-point window (point-max)))))
(add-hook! 'doom-first-buffer-hook #'my/open-side-info-windows)

;; region
(use-package! expand-region
  :bind
  ("C-=" . er/expand-region))

;; version control
(use-package! magit
  :commands magit--handle-bookmark
  :config
  (setq-default magit-diff-refine-hunk 'all)
  :bind
  (:map magit-mode-map
        ("C-c o l" . #'browse-url)))

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
(after! projectile
  (defun file-like-symbol-at-point ()
    (when (eq major-mode 'java-ts-mode)
      (when-let* ((node (treesit-node-at (point)))
                  (node-name (or (treesit-node-field-name node) ""))
                  (parent-type (treesit-node-type (treesit-node-parent node))))
        (cond
         ((member parent-type '("generic_type"
                                "type_arguments"
                                "method_reference"
                                "scoped_identifier"))
          (treesit-node-text node t))
         ((string= "type" node-name)
          (treesit-node-text node t))
         ((and (string-equal parent-type "method_invocation")
               (string-equal node-name "object"))
          (downcase (treesit-node-text node t)))
         (t
          nil)))))
  (defun projectile-find-file-with-symboal-at-point (&optional invalidate-cache)
    (interactive "P")
    (projectile-maybe-invalidate-cache invalidate-cache)
    (let* ((project-root (projectile-acquire-root))
           (symbol (thing-at-point 'symbol))
           (default (file-like-symbol-at-point)))
      (when-let* ((prompt (if default
                              (format "Find file (default %s): " default)
                            "Find file: "))
                  (file (projectile-completing-read prompt
                                                    (projectile-project-files project-root)
                                                    :initial-input default)))
        (find-file  (expand-file-name file project-root))
        (run-hooks 'projectile-find-file-hook))))
  (define-key projectile-mode-map (kbd "C-c p f") 'projectile-find-file-with-symboal-at-point))

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
(add-hook! java-ts-mode #'lsp)
(setq! lsp-modeline-code-actions-enable nil)
(setq-hook! 'java-ts-mode read-process-output-max (* 1024 (* 1024 3)))
(use-package! lsp-java
  :commands lsp
  :custom
  (lsp-java-server-install-dir
   (expand-file-name "language-server/java/jdtls/" user-emacs-directory))
  (lsp-java-save-actions-organize-imports t)
  (lsp-java-cleanup-actions-on-save t)
  (lsp-disabled-clients '(semgrep-ls))
  :bind
  (:map lsp-mode-map
        ("C-c c f" . #'lsp-format-region)
        ("C-c l d" . #'lsp-ui-doc-show)
        ("C-c l i" . #'lsp-ui-peek-find-implementation)
        ("C-c l I" . #'lsp-find-implementation)
        ("C-c l r" . #'lsp-ui-peek-find-references)
        ("C-c l R" . #'lsp-find-references)
        ("C-c l s" . #'consult-lsp-file-symbols)
        ("C-c l S" . #'consult-lsp-symbols)
        ("C-c l p" . #'consult-flycheck)
        ("C-c l P" . #'consult-lsp-diagnostics)
        ("M-RET" . #'lsp-execute-code-action)
        ("C-c C-n" . #'treesit-end-of-defun)
        ("C-c C-p" . #'treesit-beginning-of-defun))
  :config
  (setq! lsp-ui-doc-enable nil)
  (define-key general-override-mode-map (kbd "C-c c f") nil)
  (defun my/lsp-java-hover-value ()
    (when-let* ((params (lsp--text-document-position-params))
                (response (lsp-request "textDocument/hover" params))
                (contents (gethash "contents" response)))
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
  (defun my/lsp-java-show-definition-maven-coorinate ()
    "Get defin Maven coordinate from input-string."
    (interactive)
    (if-let* ((response (car (lsp-request "textDocument/definition" (lsp--text-document-position-params))))
              (uri (gethash "uri" response))
              (mvn-group-id (and (string-match "=\\/maven.groupId=\\(/[^=]+\\)=" uri)
                                 (match-string 1 uri)))
              (mvn-artifact-id (and (string-match "=\\/maven.artifactId=\\(/[^=]+\\)=" uri)
                                    (match-string 1 uri)))
              (mvn-version (and (string-match "=\\/maven.version=\\(/[^=]+\\)=" uri)
                                (match-string 1 uri))))
        (message (concat (substring mvn-group-id 1) ":"
                         (substring mvn-artifact-id 1) ":"
                         (substring mvn-version 1)))
      (message "Error: Cannot parse Maven Coordinate")))
  (defun my/treesit-parent-util-type (NODE TYPE)
    (treesit-parent-until NODE (lambda (node)
                                 (string-equal (treesit-node-type node) TYPE))))
  (defun my/java-copy-outter-class-name-at-point ()
    (interactive)
    (if-let* ((node (treesit-node-at (point)))
              (node (my/treesit-parent-util-type node "class_declaration"))
              (class-name-node (treesit-node-child-by-field-name node "name"))
              (content (treesit-node-text class-name-node t)))
        (progn (kill-new content)
               (message "Copied class name: %s" content))
      (message "Cannot find and class name from point" content)))
  (defun my/lsp-java-format-changed-lines (&optional buffer)
    "Lsp-format changed lines in the buffer."
    (interactive)
    (if (not buffer)
        (setq buffer (current-buffer)))
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
          (goto-char current-point)))))
  (defun my/java-move-method (NUM)
    (when-let* ((current-point (point))
                (point-node (treesit-node-at current-point))
                (node (my/treesit-parent-util-type point-node "method_declaration"))
                (start1 (treesit-node-start node))
                (end1 (treesit-node-end node))
                (comment-types '("line_comment" "block_comment"))
                (done (gensym "done")))
      (let ((temp-node node))
        (while-let ((prev-node (treesit-node-prev-sibling temp-node))
                    (prev-type (treesit-node-type prev-node)))
          (if (not (member prev-type comment-types))
              (setq temp-node nil)
            (setq start1 (treesit-node-start prev-node))
            (setq temp-node prev-node))))
      (let ((i 0)
            (start2)
            (end2))
        (cond
         ((< NUM 0) (while-let ((_ (<= i (- NUM)))
                                (prev-node (treesit-node-prev-sibling node))
                                (prev-type (treesit-node-type prev-node)))
                      (message "Deal %d" i)
                      (unless end2
                        (setq end2 (treesit-node-end prev-node)))
                      (if (member prev-type comment-types)
                          (setq start2 (treesit-node-start prev-node))
                        (setq i (1+ i))
                        (when (<= i (- NUM))
                          (setq start2 (treesit-node-start prev-node))))
                      (setq node prev-node)))
         ((> NUM 0) (while-let ((_ (< i NUM))
                                (next-node (treesit-node-next-sibling node))
                                (next-type (treesit-node-type next-node)))
                      (unless start2
                        (setq start2 (treesit-node-start next-node)))
                      (unless (member next-type comment-types)
                        (setq end2 (treesit-node-end next-node))
                        (setq i (1+ i)))
                      (setq node next-node))))
        (when (and start2 end2)
          (transpose-regions start1 end1 start2 end2)))))
  (defun my/java-move-method-up (ARG)
    (interactive "P")
    (my/java-move-method (- (prefix-numeric-value ARG))))
  (defun my/java-move-method-down (ARG)
    (interactive "P")
    (my/java-move-method (prefix-numeric-value ARG)))
  (defun my/set-up-java-ts-mode ()
    (add-hook 'before-save-hook (lambda ()
                                  (untabify (point-min) (point-max))) nil t)
    (add-hook 'before-save-hook #'my/lsp-java-format-changed-lines 0 t))
  (add-hook 'java-ts-mode-hook #'my/set-up-java-ts-mode))

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
        ("C-c l <backtab>" . #'org-fold-hide-subtree)))
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
