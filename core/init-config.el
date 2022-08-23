(use-package protobuf-mode
  :pin melpa-stable
  :ensure t
  :mode "\\.proto\\'"
  :init
  (defconst my-protobuf-style
    '((c-basic-offset . 2)
      (indent-tabs-mode . nil)))
  (add-hook 'protobuf-mode-hook
			(lambda () (c-add-style "my-style" my-protobuf-style t)))
  )

(use-package yaml-mode
  :pin melpa-stable
  :ensure t
  :mode "\\.yml'"
  :init
  (add-hook 'yaml-mode-hook
			'(lambda ()
               (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
  )

(use-package lua-mode
  :pin melpa-stable
  :ensure t
  :mode "\\.lua\\'"
  )

(use-package dockerfile-mode
  :pin melpa-stable
  :ensure t
  :mode "Dockerfile\\'"
  )

(use-package nginx-mode
  :pin melpa-stable
  :ensure t
  :defer t
  )

(provide 'init-config)
