(global-set-key (kbd "s-k") 'kill-current-buffer)
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

;; theme
(load-theme 'nord t)

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
  :custom-face
  (rime-default-face ((t (:background "gray100" :foreground "#333333"))))
  )
