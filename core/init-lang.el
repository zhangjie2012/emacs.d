(use-package flycheck
  :ensure t
  :hook ((prog-mode-hook . flycheck-mode))
  :bind (("<f9> <f9>" . flycheck-buffer)
		 ("<f9> l" . flycheck-list-errors))
  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc javascript-jshint python-pylint))

  ;; set flycheck tool
  (cond
   ((string-equal system-type "gnu/linux")
    (setq flycheck-javascript-eslint-executable "/usr/bin/eslint")
    )
   ((string-equal system-type "darwin")
    (setq flycheck-javascript-eslint-executable "eslint")
    ))

  ;; set config file
  (setq flycheck-eslint-args '("-c" "/home/zhangjie/.eslintrc.json"))

  ;; Python
  (setq flycheck-python-flake8-executable "flake8")
  (setq flycheck-flake8rc "~/.flake8")
  (setq flycheck-indication-mode nil)

  :config
  ;; just mode enable check
  (setq flycheck-check-syntax-automatically '())
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  )

(use-package flycheck-golangci-lint
  :ensure t
  :hook ((go-mode-hook . flycheck-golangci-lint-setup))
  :defer t
  :init
  (defvar-local flycheck-local-checkers nil)
  (defun +flycheck-checker-get(fn checker property)
    (or (alist-get property (alist-get checker flycheck-local-checkers))
        (funcall fn checker property)))
  (advice-add 'flycheck-checker-get :around '+flycheck-checker-get)
  )

(use-package eglot
  :ensure t
  :hook ((go-mode-hook . eglot-ensure)
         (python-mode-hook . eglot-ensure))
  :bind (:map eglot-mode-map
              ("<f9> s s" . eglot-reconnect)
              ("<f9> s d" . eldoc))
  :config
  (setq eldoc-echo-area-use-multiline-p nil
        eglot-ignored-server-capabilities '(:documentHighlightProvider))

  (add-to-list 'eglot-stay-out-of 'flymake) ;; disable flymake
  )

(use-package go-mode
  :ensure t
  :mode "\\.go'"
  :config
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
  )

(use-package python
  :ensure t
  :mode "\\.py'"
  :init
  (setq python-shell-interpreter "python3")
  (set-variable 'py-indent-offset 4)
  (set-variable 'python-indent-guess-indent-offset nil)
  :config
  (defun eglot-format-buffer-on-save ()
    (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
    (add-hook 'before-save-hook #'flycheck-buffer)
    )
  (add-hook 'python-mode-hook #'eglot-format-buffer-on-save)
  )

(use-package web-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.wxml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.xml?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . web-mode))
  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (setq-default indent-tabs-mode nil)
    (setq web-mode-markup-indent-offset 2)  ; HTML
    (setq web-mode-css-indent-offset 2)  ; CSS
    (setq web-mode-code-indent-offset 2) ; script/code
    (setq web-mode-script-padding 1)     ; html 内嵌 script 开头缩进
    )
  (add-hook 'web-mode-hook 'my-web-mode-hook)
  :config
  (setq web-mode-enable-current-element-highlight t)
  ;; (setq web-mode-content-types-alist
  ;;       '(("jsx" . "\\.js[x]?\\'")))
  )

;; for LESS
(use-package css-mode
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.less\\'" . css-mode))
  (add-to-list 'auto-mode-alist '("\\.wxss\\'" . css-mode))
  :config
  (setq css-indent-offset 2)
  )

(use-package rjsx-mode
  :ensure t
  :hook ((rjsx-mode-hook . company-mode))
  :defer t
  :init
  (add-to-list 'auto-mode-alist '(".*\\.js\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '(".*\\.jsx\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '(".*\\.ts\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '(".*\\.tsx\\'" . rjsx-mode))
  (add-hook 'rjsx-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil) ;;Use space instead of tab
              (setq js-indent-level 2) ;;space width is 2 (default is 4)
              (setq js2-strict-missing-semi-warning nil))) ;;disable the semicolon warning
  (with-eval-after-load 'rjsx-mode
    ;; (define-key rjsx-mode-map "<" nil)
    ;; (define-key rjsx-mode-map (kbd "C-d") nil)
    ;; (define-key rjsx-mode-map ">" nil)
    (define-key rjsx-mode-map (kbd "M-.") nil)
    )
  )

(provide 'init-lang)
