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
