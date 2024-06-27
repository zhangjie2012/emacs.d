(use-package flycheck
  :ensure t
  :hook ((go-mode . flycheck-mode)
         (emacs-lisp-mode . flycheck-mode)
		 (rjsx-mode . flycheck-mode)
		 (python-mode . flycheck-mode))
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
  (setq flycheck-check-syntax-automatically '(idle-change idle-buffer-switch)))

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
  (setq flycheck-golangci-lint-config "~/.golangci.yaml"))

(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-global-modes '(not org-mode markdown-mode eshell-mode thrift-mode)
		company-format-margin-function nil
		company-tooltip-maximum-width 72
		company-tooltip-minimum-width 40
		company-show-quick-access nil
		company-tooltip-margin 1
		company-tooltip-limit 8))

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((go-mode . lsp-deferred)
		 (elisp-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  ;; TODO ignore file watchers https://emacs-lsp.github.io/lsp-mode/page/file-watchers/
  (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols)
  (setq lsp-idle-delay 0.500
		lsp-log-io nil
		lsp-headerline-breadcrumb-enable nil
		lsp-enable-symbol-highlighting nil))

(use-package lsp-ui
  :commands lsp-ui-mode
  :init
  :ensure t
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (setq lsp-ui-sideline-show-diagnostics nil
		lsp-ui-sideline-show-hover nil
		lsp-ui-doc-enable nil))

(use-package go-mode
  :ensure t
  :config
  (defun lsp-go-install-save-hooks ()
	(add-hook 'before-save-hook #'lsp-format-buffer t t)
	(add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))

(use-package go-tag
  :ensure t
  :init
  (setq go-tag-args (list "-transform" "snakecase"))
  (with-eval-after-load 'go-mode
    (define-key go-mode-map (kbd "C-c t") #'go-tag-add)
    (define-key go-mode-map (kbd "C-c T") #'go-tag-remove)))

(use-package python
  :ensure t
  :mode "\\.py'"
  :init
  (setq python-shell-interpreter "python3")
  (set-variable 'py-indent-offset 4)
  (set-variable 'python-indent-guess-indent-offset nil))

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
  (setq css-indent-offset 2))

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
    (define-key rjsx-mode-map (kbd "M-.") nil)))

(use-package format-all
  :ensure t
  :bind ("<f8> q" . format-all-region-or-buffer))

(provide 'init-lang)
