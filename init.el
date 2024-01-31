(doom! :completion
       (company +childframe)
       (vertico +icons +childframe)
       :ui
       doom
       doom-dashboard
       :emacs
       (ibuffer +icons)
       :tools
       (magit)
       (lsp  +eglot)
       (tree-sitter)
       :lang
       markdown
       (python +lsp +pyright +pyenv +tree-sitter)
       emacs-lisp
       :config
       (default +bindings +smartparens)
       )
