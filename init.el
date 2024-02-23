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
       :lang
       emacs-lisp
       (json +lsp)
       markdown
       (python +lsp +pyenv)
       (java +lsp)
       :tools
       (magit)
       (lsp)
       (editorconfig)
       :ui
       doom
       doom-dashboard
       workspaces
       modeline
       (treemacs +lsp)
       )
