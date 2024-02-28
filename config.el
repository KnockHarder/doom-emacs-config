;;; init.el -- init

;; key bindings and theme
(global-set-key (kbd "s-k") 'kill-current-buffer)
(setq doom-theme 'doom-dracula
      doom-font (font-spec :size 14)
      doom-variable-pitch-font (font-spec :size 16))

;; maximum frame after UI initialized
(add-to-list 'default-frame-alist '(width . 120))
(add-to-list 'default-frame-alist '(height . 40))
(add-hook 'doom-init-ui-hook #'toggle-frame-maximized)
(use-package! ace-window
  :commands ace-window
  :custom
  (aw-scope 'frame)
  )

;; my window functions
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
    (set-window-parameter win 'no-other-window t)))
(defun my/delete-other-on-side-windows ()
  (interactive)
  (mapc #'delete-window
        (delq (selected-window)
              (seq-filter (lambda (window)
                            (not (window-parameter window 'window-side)))
                          (window-list))))
  )
(global-set-key (kbd "C-x 1") #'my/delete-other-on-side-windows)
(defun my/read-window ()
  "Select not side window in current frame."
  (let* ((window-alist (mapcar (lambda (w)
                                 (cons (buffer-name (window-buffer w)) w))
                               (window-list)))
         (selected (assoc (completing-read "Select window: " (mapcar 'car window-alist)
                                           nil t nil nil
                                           (buffer-name (window-buffer (next-window))))
                          window-alist)))
    (cdr selected)))
(defun my/scroll-window-up (window &optional ARG)
  "Select not side window in current frame and scroll down.
 Like scroll-up-command."
  (interactive (list (my/read-window) current-prefix-arg))
  (with-selected-window window
    (scroll-up ARG)))
(defun my/scroll-window-down (window &optional ARG)
  "Select not side window in current frame and scroll down.
 Like scroll-up-command."
  (interactive (list (my/read-window) current-prefix-arg))
  (with-selected-window window
    (scroll-up (if ARG
                   (- ARG)
                 '-))))
(global-set-key (kbd "C-M-v") #'my/scroll-window-up)
(global-set-key (kbd "C-M-S-v") #'my/scroll-window-down)

;; region
(use-package! expand-region
  :bind
  ("C-=" . er/expand-region))

(setq-hook! magit-status-mode magit-diff-refine-hunk 'all)

;; rime
(use-package! rime
  :custom
  (default-input-method "rime")
  (rime-show-candidate 'posframe)
  (rime-librime-root (expand-file-name "librime/dist" user-emacs-directory))
  (rime-emacs-module-header-root (expand-file-name "librime/manual-headers"
                                                   user-emacs-directory))
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
  (add-hook 'git-commit-mode-hook 'activate-rime)
  )

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
       (let* ((models (cdr (assoc 'data data)))
              (ids (mapcar (lambda (m)
                             (cdr (assoc 'id m))) models))
              (id-not-found t)
              )
         (dolist (id '("gpt-4" "gpt-3.5-turbo"))
           (when (and id-not-found (member id ids))
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
                                             :protocol "http"
                                             )
                             )
               )
             (setq id-not-found nil)
             )
           )
         )
       )
     )
    :error
    (cl-function
     (lambda (&rest args &key data error-thrown &allow-other-keys)
       (message "My GPT: No avliable local GPT service. error: %S" error-thrown)
       )
     )
    )
  )

;; code
(setq project-find-functions '(project-try-vc project-projectile))
(after! vc-git
  (add-to-list 'projectile-project-root-functions #'vc-git-root))
(after! projectile
  (defun my/projectile-find-file (&optional invalidate-cache)
    (interactive "P")
    (projectile-maybe-invalidate-cache invalidate-cache)
    (let* ((project-root (projectile-acquire-root))
           (symbol (thing-at-point 'symbol))
           (default-value (if symbol
                              (downcase symbol)
                            nil))
           (prompt (if default-value
                       (format "Find file (default %s): " default-value)
                     "Find file: "))
           (file (projectile-completing-read prompt
                                             (projectile-project-files project-root)
                                             :initial-input default-value))
           )
      (when file
        (find-file  (expand-file-name file project-root))
        (run-hooks 'projectile-find-file-hook))))
  (define-key projectile-mode-map (kbd "C-c p f") 'my/projectile-find-file)
  )

(setq gcmh-high-cons-threshold (* 1024 (* 1024 16)))
(use-package! python
  :commands python python-mode
  :bind
  (:map python-mode-map
        ("C-c M-}" . #'python-nav-forward-defun)
        ("C-c M-{" . #'python-nav-backward-defun)
        ))
(defun my/setup-pipenv ()
  "set python paths for pipenv."
  (let ((python-path (string-trim (shell-command-to-string "pipenv run which python")))
        )
    (setq-local python-interpreter python-path)
    (setq-local pythonic-interpreter python-path)
    (setq-local lsp-pyright-python-executable-cmd python-path)
    (shell-command "pipenv run pip install isort pyflakes flake8")
    )
  )
(add-hook! pipenv-mode #'my/setup-pipenv)

(add-to-list 'auto-mode-alist '("\\.java\\'" . java-ts-mode))
(add-hook! java-ts-mode #'lsp)
(require 'lsp-java)
(use-package! lsp-java
  :custom
  (lsp-java-server-install-dir
   (expand-file-name "language-server/java/jdtls/" user-emacs-directory))
  (lsp-response-timeout 3)
  :hook
  (java-mode . subword-mode)
  (java-mode . (lambda ()
                 (setq read-process-output-max (* 1024 (* 1024 3)))))
  :bind
  (:map lsp-mode-map
        ("C-c c p r" . #'lsp-ui-peek-find-references)
        ("C-c c p d" . #'lsp-ui-doc-glance)
        ("C-c c p i" . #'lsp-ui-doc-show))
  :config
  (defun my/lsp-java-copy-hover-value ()
    (interactive)
    (let* ((params (lsp--text-document-position-params))
           (response (lsp-request "textDocument/hover" params))
           (contents (gethash "contents" response)))
      (if contents
          (kill-new (gethash "value" contents)))))
  (defun my/lsp-java-show-definition-maven-coorinate ()
    "Get defin Maven coordinate from input-string."
    (interactive)
    (let* ((response (car (lsp-request "textDocument/definition" (lsp--text-document-position-params))))
           (uri (gethash "uri" response))
           (mvn-group-id (and (string-match "=\\/maven.groupId=\\(/[^=]+\\)=" uri)
                              (match-string 1 uri)))
           (mvn-artifact-id (and (string-match "=\\/maven.artifactId=\\(/[^=]+\\)=" uri)
                                 (match-string 1 uri)))
           (mvn-version (and (string-match "=\\/maven.version=\\(/[^=]+\\)=" uri)
                             (match-string 1 uri))))
      (message (if (and mvn-group-id mvn-artifact-id mvn-version)
                   (concat (substring mvn-group-id 1) ":"
                           (substring mvn-artifact-id 1) ":"
                           (substring mvn-version 1))
                 "Error: Cannot parse Maven Coordinate")))))
(setq! lsp-ui-doc-show-with-cursor t
       lsp-ui-doc-position 'top)

(after! spell-fu
  (let ((dict (spell-fu-get-ispell-dictionary "english"))
        )
    (when dict
      (setq-default spell-fu-dictionaries `(,dict)))
    )
  )

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

(setq! org-image-actual-width nil)

;; browser
(setq! browse-url-browser-function #'xwidget-webkit-browse-url)
