(global-set-key (kbd "s-k") 'kill-current-buffer)
(setq doom-theme 'doom-dracula
      doom-font (font-spec :size 14)
      doom-variable-pitch-font (font-spec :size 16))
;; session
(use-package! savehist
  :init
  (savehist-mode))

(if (not (daemonp))
    (desktop-save-mode 1)
  (defun restore-desktop (frame)
    (with-selected-frame frame
      (desktop-save-mode 1)
      (desktop-read)
      (remove-hook 'after-make-frame-functions 'restore-desktop)))
  (add-hook 'after-make-frame-functions 'restore-desktop))

;; region
(use-package! expand-region
  :bind
  ("C-=" . er/expand-region))

(after! magit
  (setq magit-diff-refine-hunk 'all))

;; rime
(use-package! rime
  :custom
  (default-input-method "rime")
  (rime-show-candidate 'posframe)
  (rime-librime-root (expand-file-name "librime/dist" user-emacs-directory))
  (rime-emacs-module-header-root (expand-file-name "librime/manual-headers" user-emacs-directory))
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
             (use-package! gptel
               :custom
               (gptel-model id)
               :config
               (setq-default gptel-backend (gptel-make-openai "ChatGPT"
                                             :key local-key
                                             :host local-server
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
