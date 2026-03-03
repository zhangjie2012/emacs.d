(use-package treesit
  :ensure nil
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode)
         ("\\.js\\'"  . js-ts-mode)
         ("\\.jsx\\'" . js-ts-mode)
         ("\\.json\\'" . json-ts-mode)
         ("\\.css\\'"  . css-ts-mode)
         ("\\.go\\'"   . go-ts-mode))
  :config
  (setq treesit-language-source-alist
        '((go "https://github.com/tree-sitter/tree-sitter-go")
          (gomod "https://github.com/camdencheek/tree-sitter-go-mod")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript")
          (python "https://github.com/tree-sitter/tree-sitter-python")))
  ;; 如果没有安装，可以使用 M-x treesit-install-language-grammar 安装
  (setq treesit-font-lock-level 3))

(use-package flycheck
  :ensure t
  :hook ((go-ts-mode . flycheck-mode)
         (emacs-lisp-mode . flycheck-mode)
         (js-ts-mode . flycheck-mode)
         (tsx-ts-mode . flycheck-mode)
         (typescript-ts-mode . flycheck-mode)
         (python-ts-mode . flycheck-mode))
  :init
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc javascript-jshint python-pylint))
  ;; set flycheck tool
  (cond
   ((string-equal system-type "gnu/linux")
    (setq flycheck-javascript-eslint-executable "/usr/bin/eslint"))
   ((string-equal system-type "darwin")
    (setq flycheck-javascript-eslint-executable "eslint")))
  (setq flycheck-indication-mode 'left-fringe)
  ;; Python: lsp 集成了 flake8, 因此 flycheck python-mode disable
  :config
  ;; Enable eslint for TS/JS modes
  (dolist (mode '(js-ts-mode tsx-ts-mode typescript-ts-mode))
    (flycheck-add-mode 'javascript-eslint mode))

  ;; 避免卡顿，设定语法检测的时机，延迟 1s
  (setq flycheck-idle-change-delay 2
        flycheck-idle-buffer-switch-delay 2)
  (setq flycheck-check-syntax-automatically '(idle-change idle-buffer-switch)))

(use-package go-ts-mode
  :ensure nil
  :config
  (setq go-ts-mode-indent-offset 4))

(use-package company
  :ensure t
  :hook ((prog-mode . company-mode)
         (text-mode . (lambda () (company-mode -1)))
         (thrift-mode . (lambda () (company-mode -1))))
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :config
  (setq company-idle-delay 0.2
        company-minimum-prefix-length 1
        company-tooltip-align-annotations t
        company-selection-wrap-around t
        company-transformers '(company-sort-by-backend-importance)
        company-dabbrev-other-buffers nil
        company-in-string-or-comment nil))

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-use-plists t
        lsp-idle-delay 0.5
        lsp-log-io nil
        lsp-enable-symbol-highlighting nil
        lsp-enable-file-watchers nil
        lsp-enable-folding nil
        lsp-enable-snippet nil
        lsp-lens-enable nil
        lsp-modeline-code-actions-enable nil
        lsp-headerline-breadcrumb-enable nil
        lsp-semantic-tokens-enable nil
        lsp-completion-no-cache t
        )
  :hook ((go-ts-mode . lsp-deferred)
         (python-ts-mode . lsp-deferred)
         (js-ts-mode . lsp-deferred)
         (tsx-ts-mode . lsp-deferred)
         (typescript-ts-mode . lsp-deferred)
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
          "[/\\\\]build\\'"
          "[/\\\\]dist\\'"))

  ;; ---------------------------------------------------------
  ;; JavaScript / TypeScript Performance & Formatting
  ;; ---------------------------------------------------------
  ;; 关闭 JS/TS 特有的 Inlay Hints 以提升性能
  (setq lsp-javascript-display-return-type-hints nil
        lsp-javascript-display-variable-type-hints nil
        lsp-javascript-display-enum-member-value-hints nil)

  ;; 强制 JS/TS 缩进为 2 空格 (覆盖 Server 默认行为)
  (setq lsp-javascript-format-insert-space-after-opening-and-before-closing-nonempty-braces nil
        lsp-typescript-format-insert-space-after-opening-and-before-closing-nonempty-braces nil
        lsp-javascript-format-indent-size 2
        lsp-typescript-format-indent-size 2
        lsp-javascript-format-tab-size 2
        lsp-typescript-format-tab-size 2)

  ;; ---------------------------------------------------------
  ;; Python LSP Configuration
  ;; ---------------------------------------------------------
  (setq lsp-pylsp-plugins-flake8-enabled t
        lsp-pylsp-plugins-flake8-config "~/.flake8"
        lsp-pylsp-plugins-mccabe-enabled nil
        lsp-pylsp-plugins-pydocstyle-enabled nil))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
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

(use-package go-tag
  :ensure t
  :init
  (setq go-tag-args (list "-transform" "snakecase")))

(use-package python-ts-mode
  :ensure nil
  :mode "\\.py\\'"
  :config
  (setq python-shell-interpreter "python3")
  (setq python-indent-offset 4)
  (setq python-indent-guess-indent-offset nil))

(use-package html-ts-mode
  :ensure nil
  :mode "\\.html?\\'")

(use-package css-ts-mode
  :ensure nil
  :mode "\\.css\\'"
  :config
  (setq css-indent-offset 2))

(use-package json-ts-mode
  :ensure nil
  :mode "\\.json\\'"
  :config
  (setq json-ts-mode-indent-offset 2))

(use-package typescript-ts-mode
  :ensure nil
  :mode (("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :hook (typescript-ts-base-mode . (lambda ()
                                     (setq-local indent-tabs-mode nil)
                                     (setq-local typescript-ts-mode-indent-offset 2)))
  :config
  (setq typescript-ts-mode-indent-offset 2))

(use-package js-ts-mode
  :ensure nil
  :mode (("\\.js\\'" . js-ts-mode)
         ("\\.jsx\\'" . js-ts-mode))
  :hook (js-ts-mode . (lambda ()
                        (setq-local indent-tabs-mode nil)
                        (setq-local js-indent-level 2)))
  :config
  (setq js-indent-level 2))

(use-package apheleia
  :ensure t
  :hook (after-init . apheleia-global-mode)
  :bind ("<f8> q" . apheleia-format-buffer)
  :config
  ;; 强制 Prettier 使用 2 空格缩进
  (setf (alist-get 'prettier apheleia-formatters)
        '("apheleia-npx" "prettier" "--stdin-filepath" filepath "--tab-width" "2"))

  (setq apheleia-mode-alist
        (append '((go-ts-mode . goimports)
                  (js-ts-mode . prettier)
                  (tsx-ts-mode . prettier)
                  (typescript-ts-mode . prettier)
                  (json-ts-mode . prettier)
                  (css-ts-mode . prettier)
                  (python-ts-mode . black))
                apheleia-mode-alist)))

(provide 'init-lang)
