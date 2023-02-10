(use-package python
  :ensure t
  :mode "\\.py'"
  :hook ((python-mode-hook . flycheck-mode)
		 (python-mode-hook . company-mode)
		 )
  :init
  (setq python-shell-interpreter "python3")
  (set-variable 'py-indent-offset 4)
  (set-variable 'python-indent-guess-indent-offset nil)
  )

(provide 'init-ide-python)
