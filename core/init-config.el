(use-package protobuf-mode
  :ensure t
  :mode "\\.proto\\'"
  :init
  (defconst my-protobuf-style
    '((c-basic-offset . 2)
      (indent-tabs-mode . nil)))
  (add-hook 'protobuf-mode-hook
			(lambda () (c-add-style "my-style" my-protobuf-style t))))

(use-package thrift
  :ensure t
  :config
  (setq thrift-indent-level 4))

(use-package yaml-mode
  :ensure t
  :mode "\\.yml'"
  :init
  (add-hook 'yaml-mode-hook
			(lambda ()
              (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(use-package lua-mode
  :ensure t
  :mode "\\.lua\\'")

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile\\'")

(use-package nginx-mode
  :ensure t
  :defer t)

(use-package sql-indent
  :ensure t
  :hook (sql-mode . sqlind-minor-mode))

(provide 'init-config)
