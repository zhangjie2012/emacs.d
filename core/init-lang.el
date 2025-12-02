(use-package flycheck
  :ensure t
  :hook ((go-mode . flycheck-mode)
         (emacs-lisp-mode . flycheck-mode)
		 (rjsx-mode . flycheck-mode))
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
  (setq flycheck-indication-mode 'left-fringe)
  ;; Python
  ;; lsp 集成了 flake8, 因此 flycheck python-mode disable
  :config
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  ;; 避免卡顿，设定语法检测的时机，延迟 1s
  ;; 1. 停止修改后
  ;; 2. 切换 buffer 后
  (setq flycheck-idle-change-delay 1
        flycheck-idle-buffer-switch-delay 1)
  (setq flycheck-check-syntax-automatically '(idle-change idle-buffer-switch)))

(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :init
  ;; 提升性能：禁用自动排序（由 capf/LSP 决定）
  (setq company-transformers nil)
  (setq company-idle-delay 0.15
        company-minimum-prefix-length 1
        company-echo-delay 0
        company-show-quick-access nil)
  :config
  (setq company-backends
        '((company-capf
           :with company-dabbrev-code
           company-files)))
  (setq company-dabbrev-code-everywhere nil
        company-dabbrev-code-other-buffers nil
        company-dabbrev-other-buffers nil
        company-dabbrev-ignore-case t
        company-dabbrev-downcase nil)
  (setq company-tooltip-limit 10
        company-tooltip-maximum-width 60
        company-tooltip-minimum-width 20
        company-tooltip-margin 1
        company-tooltip-align-annotations t
        company-format-margin-function
        'company-detect-icons-margin)
  (setq company-require-match nil
        company-auto-commit nil)
  (setq company-global-modes
        '(not org-mode markdown-mode eshell-mode shell-mode thrift-mode)))

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-use-plists t
        lsp-idle-delay 0.3
        lsp-log-io nil
        lsp-enable-symbol-highlighting nil
        lsp-enable-file-watchers nil
        lsp-enable-folding nil
        lsp-enable-snippet nil
        lsp-lens-enable nil
        lsp-modeline-code-actions-enable nil
        lsp-headerline-breadcrumb-enable nil
        lsp-semantic-tokens-enable nil
        lsp-completion-no-cache t)
  :hook ((go-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (lisp-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))

  :bind (("<f8> s" . lsp-restart-workspace))
  :config
  (setq lsp-file-watch-ignored-directories
        '("[/\\\\]\\.git\\'"
          "[/\\\\]node_modules\\'"
          "[/\\\\]\\.hg\\'"
          "[/\\\\]\\.idea\\'"
          "[/\\\\]\\.vscode\\'"
          "[/\\\\]target\\'"
          "[/\\\\]build\\'"))
  ;; Python LSP (pylsp)
  (setq lsp-pylsp-plugins-flake8-enabled t
        lsp-pylsp-plugins-flake8-config "~/.flake8"
        lsp-pylsp-plugins-mccabe-enabled nil
        lsp-pylsp-plugins-pydocstyle-enabled nil))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config

  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references]  #'lsp-ui-peek-find-references)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-webkit nil
        lsp-ui-doc-position 'at-point
        lsp-ui-doc-show-with-mouse t
        lsp-ui-doc-show-with-cursor nil
        lsp-ui-doc-border (face-foreground 'font-lock-comment-face)
        lsp-ui-peek-fontify 'on-demand))

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
              (setq indent-tabs-mode nil)
              (setq js-indent-level 2)
              (setq js2-strict-missing-semi-warning nil)))
  (with-eval-after-load 'rjsx-mode
    ;; (define-key rjsx-mode-map "<" nil)
    ;; (define-key rjsx-mode-map (kbd "C-d") nil)
    ;; (define-key rjsx-mode-map ">" nil)
    (define-key rjsx-mode-map (kbd "M-.") nil)))

(use-package format-all
  :ensure t
  :bind ("<f8> q" . format-all-region-or-buffer)
  :config
  (setq-default format-all-formatters
                '(("JavaScript"  (prettierd))
                  ("JSON"        (prettier))
                  ("JSX"         (prettierd))
                  ("TypeScript"  (prettier))
                  ("TSX"         (prettier))
				  ("YAML"        (prettier))
				  ("Markdown"    (prettier))
                  ("Go"          (goimports)))))

(provide 'init-lang)
