(use-package flycheck-golangci-lint
  :ensure t
  :defer t
  :init
  (defvar-local flycheck-local-checkers nil)
  (defun +flycheck-checker-get(fn checker property)
    (or (alist-get property (alist-get checker flycheck-local-checkers))
        (funcall fn checker property)))
  (advice-add 'flycheck-checker-get :around '+flycheck-checker-get)
  )

(use-package go-mode
  :ensure t
  :hook ((go-mode-hook . company-mode)
         (go-mode-hook . flycheck-mode)
         (go-mode-hook . flycheck-golangci-lint-setup)
         (go-mode-hook . eglot-ensure)
         )
  :config
  (defun eglot-organize-imports ()
    "Offer to execute the source.organizeImports code action."
    (interactive)
    (unless (eglot--server-capable :codeActionProvider)
      (eglot--error "Server can't execute code actions!"))
    (let* ((server (eglot--current-server-or-lose))
           (actions (jsonrpc-request
                     server
                     :textDocument/codeAction
                     (list :textDocument (eglot--TextDocumentIdentifier))))
           (action (cl-find-if
                    (jsonrpc-lambda (&key kind &allow-other-keys)
                      (string-equal kind "source.organizeImports" ))
                    actions)))
      (when action
        (eglot--dcase action
          (((Command) command arguments)
           (eglot-execute-command server (intern command) arguments))
          (((CodeAction) edit command)
           (when edit (eglot--apply-workspace-edit edit))
           (when command
             (eglot--dbind ((Command) command arguments) command
               (eglot-execute-command server (intern command) arguments))))))))
  (defun eglot-format-buffer-on-save ()
    (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
    (add-hook 'before-save-hook #'eglot-organize-imports)
    (add-hook 'before-save-hook #'flycheck-buffer)
    )
  (add-hook 'go-mode-hook #'eglot-format-buffer-on-save)

  (use-package go-tag
    :ensure t
    :init
    (setq go-tag-args (list "-transform" "snakecase"))
    (with-eval-after-load 'go-mode
      (define-key go-mode-map (kbd "C-c t") #'go-tag-add)
      (define-key go-mode-map (kbd "C-c T") #'go-tag-remove))
    )
  (use-package gotest
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
