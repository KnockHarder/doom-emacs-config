(doom! :checkers
       (syntax +childframe)
       (spell +everywhere)
       :completion
       (company +childframe)
       (vertico +icons +childframe)
       :config
       (default +bindings +smartparens)
       :editor
       (format +onsave)
       snippets
       :emacs
       (ibuffer +icons)
       (dired)
       :lang
       emacs-lisp
       (json +lsp)
       markdown
       (python +lsp +pyenv)
       (java +lsp)
       kotlin
       (org +present)
       (treesit-f)
       (lsp-jdt)
       :tools
       (magit)
       (lsp)
       (editorconfig)
       :ui
       modeline
       doom
       doom-dashboard
       workspaces
       modeline
       side-window
       )
