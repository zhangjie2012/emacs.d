(use-package flycheck-golangci-lint
  :pin melpa
  :ensure t
  :defer t
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

(use-package go-mode
  :pin melpa
  :ensure t
  :hook ((go-mode-hook . lsp-deferred)
		 (go-mode-hook . company-mode)
		 (go-mode-hook . flycheck-mode)
		 (go-mode-hook . flycheck-golangci-lint-setup)
		 )
  :config
  (require 'lsp-go)
  ;; https://github.com/golang/tools/blob/master/gopls/doc/analyzers.md
  (setq lsp-go-analyses
		'((fieldalignment . t)
		  (nilness . t)
		  (unusedwrite . t)
		  (unusedparams . t)
		  ))
  ;; $GOPATH/bin
  ;; (add-to-list 'exec-path "~/go/bin")
  ;; (setq gofmt-command "goimports")
  (defun my-go-mode-hook()
	(add-hook 'before-save-hook 'gofmt-before-save)
	(add-hook 'before-save-hook 'flycheck-buffer)
	)
  (add-hook 'go-mode-hook 'my-go-mode-hook)

  (use-package go-tag
	:pin melpa
	:ensure t
	:init
	(setq go-tag-args (list "-transform" "snakecase"))
	(with-eval-after-load 'go-mode
	  (define-key go-mode-map (kbd "C-c t") #'go-tag-add)
	  (define-key go-mode-map (kbd "C-c T") #'go-tag-remove))
	)

  (use-package gotest
	:pin melpa
	:ensure t
	:init
	(setq go-test-verbose t)
	:config
	(define-key go-mode-map (kbd "<f9> t f") 'go-test-current-file)
	(define-key go-mode-map (kbd "<f9> t t") 'go-test-current-test)
	(define-key go-mode-map (kbd "<f9> t p") 'go-test-current-project)
	)
  )

(provide 'init-ide-go)
