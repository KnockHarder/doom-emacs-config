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
(use-package! lsp
  :custom
  (lsp-modeline-code-actions-enable nil))
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
