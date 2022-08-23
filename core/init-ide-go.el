(use-package go-mode
  :pin melpa-stable
  :ensure t
  :mode "\\.go\\'"
  :interpreter "go"
  :config
  (use-package go-tag
	:pin melpa-stable
	:ensure t
	:init
	(setq go-tag-args (list "-transform" "snakecase"))
	(with-eval-after-load 'go-mode
	  (define-key go-mode-map (kbd "C-c t") #'go-tag-add)
	  (define-key go-mode-map (kbd "C-c T") #'go-tag-remove))
	)
  (use-package gotest
	:pin melpa-stable
	:ensure t
	:init
	(setq go-test-verbose t)
	:config
	(define-key go-mode-map (kbd "<f9> t f") 'go-test-current-file)
	(define-key go-mode-map (kbd "<f9> t t") 'go-test-current-test)
	(define-key go-mode-map (kbd "<f9> t p") 'go-test-current-project)
	)
  )

(use-package flycheck-golangci-lint
  :pin melpa
  :ensure t
  :hook (go-mode . flycheck-golangci-lint-setup)
  :init
  (defvar-local flycheck-local-checkers nil)
  (defun +flycheck-checker-get(fn checker property)
    (or (alist-get property (alist-get checker flycheck-local-checkers))
        (funcall fn checker property)))
  (advice-add 'flycheck-checker-get :around '+flycheck-checker-get)
  (add-hook 'go-mode-hook (lambda()
                            (flycheck-golangci-lint-setup)
                            (setq flycheck-local-checkers '((lsp . ((next-checkers . (golangci-lint))))))))
  )

(provide 'init-ide-go)
