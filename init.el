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
       :emacs
       (ibuffer +icons)
       :lang
       emacs-lisp
       markdown
       (python +lsp +pyright +pyenv +tree-sitter)
       (java +lsp)
       :tools
       (magit)
       (lsp +eglot)
       (tree-sitter)
       :ui
       doom
       doom-dashboard
       workspaces
       modeline
       (treemacs +lsp)
       )
