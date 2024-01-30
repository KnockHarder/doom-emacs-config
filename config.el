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

;; consult
(use-package! consult
  :bind (("C-x c s" . consult-line)
         ("C-x c r" . consult-recent-file)
         ("C-x c g" . consult-git-grep)
         ("C-x b" . consult-buffer)
         ))

;; vertico
(use-package! vertico
  :init
  (vertico-mode))
;; orderless
(use-package! orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  )

;; region
(use-package! expand-region
  :bind
  ("C-=" . er/expand-region))

;; magit
(use-package! magit
  :custom
  (magit-diff-refine-hunk 'all)
  )
