(use-package python
  :pin melpa-stable
  :ensure t
  :mode "\\.py'"
  :init
  (setq python-shell-interpreter "python3")
  (set-variable 'py-indent-offset 4)
  (set-variable 'python-indent-guess-indent-offset nil)
  :config
  (defun lsp-py-install-save-hooks ()
	(add-hook 'before-save-hook #'flycheck-buffer)
	(add-hook 'before-save-hook #'lsp-organize-imports t t)
	(add-hook 'before-save-hook #'lsp-format-buffer t t)
	)
  (add-hook 'python-mode-hook #'lsp-py-install-save-hooks)
  )

(provide 'init-ide-python)
