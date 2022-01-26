;; -*- no-byte-compile: t; -*-
;;; completion/corfu/packages.el

(package! corfu)
(package! cape)

(when (featurep! +lsp)
  (package! kind-icon)
  (package! corfu-doc
    :recipe (:host github :repo "galeo/corfu-doc")))
