(use-package flycheck
  :ensure t
  :hook ((prog-mode . flycheck-mode))
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

  ;; Python
  (setq flycheck-python-flake8-executable "flake8")
  (setq flycheck-flake8rc "~/.flake8")
  (setq flycheck-indication-mode nil)

  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)

  ;; 避免卡顿，设定语法检测的时机，延迟 1s
  ;; 1. 停止修改后
  ;; 2. 切换 buffer 后
  (setq flycheck-idle-change-delay 1
        flycheck-idle-buffer-switch-delay 1)
  (setq flycheck-check-syntax-automatically '(idle-change idle-buffer-switch))
  )

;; https://github.com/weijiangan/flycheck-golangci-lint
(use-package flycheck-golangci-lint
  :ensure t
  :hook ((go-mode . flycheck-golangci-lint-setup))
  :init
  (defvar-local flycheck-local-checkers nil)
  (defun +flycheck-checker-get(fn checker property)
    (or (alist-get property (alist-get checker flycheck-local-checkers))
        (funcall fn checker property)))
  (advice-add 'flycheck-checker-get :around '+flycheck-checker-get)
  :config
  (setq flycheck-golangci-lint-config "~/.golangci.yaml")
  )

(use-package eglot
  :ensure t
  :hook ((go-mode . eglot-ensure)
         (python-mode . eglot-ensure))
  :bind (:map eglot-mode-map
              ("<f9> s s" . eglot-reconnect)
              ("<f9> s d" . eldoc)
              ("<f9> s i" . eglot-find-implementation)
              )
  :config
  (setq eldoc-echo-area-use-multiline-p nil
        eglot-ignored-server-capabilities '(:documentHighlightProvider))
  (add-to-list 'eglot-stay-out-of 'flymake) ;; disable flymake
  )

(use-package go-mode
  :ensure t
  :after eglot
  :mode "\\.go'"
  :config
  (defun my-eglot-organize-imports () (interactive)
         (eglot-code-actions nil nil "source.organizeImports" t))
  (defun eglot-buffer-on-save ()
    (add-hook 'before-save-hook #'my-eglot-organize-imports nil t)
    (add-hook 'before-save-hook #'eglot-format-buffer -10 t)
    )
  (add-hook 'go-mode-hook #'eglot-buffer-on-save)
  )

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
