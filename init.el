(doom! :completion
       (company +childframe)
       (vertico +icons +childframe)
       :ui
       doom
       doom-dashboard
       workspaces
       modeline
       (treemacs +lsp)
       :emacs
       (ibuffer +icons)
       :tools
       (magit)
       (lsp  +eglot)
       (tree-sitter)
       :editor
       (format +onsave)
       :lang
       markdown
       (python +lsp +pyright +pyenv +tree-sitter)
       emacs-lisp
       :config
       (default +bindings +smartparens)
       )
