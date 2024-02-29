(package! consult)
(package! expand-region)
(package! rime)
(package! gptel)
(package! request)
(package! rg)
(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el" "dist")))
(package! go-translate
  :recipe (:host github :repo "lorniu/go-translate" :files("*.el")))
(package! protobuf
  :recipe (:host github :repo "protocolbuffers/protobuf" :files("editors/*.el")))
(when (package! lsp-bridge
        :recipe (:host github
                 :repo "manateelazycat/lsp-bridge"
                 :branch "master"
                 :files ("*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
                 ;; do not perform byte compilation or native compilation for lsp-bridge
                 :build (:not compile)))
  (package! markdown-mode)
  (package! yasnippet))
