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
  (setq treesit-font-lock-level 4))

(use-package treesit-auto
  :ensure t
  :config
  (global-treesit-auto-mode))

(use-package flycheck
  :ensure t
  :hook ((go-ts-mode . flycheck-mode)
         (emacs-lisp-mode . flycheck-mode)
         (js-ts-mode . flycheck-mode)
         (tsx-ts-mode . flycheck-mode)
         (typescript-ts-mode . flycheck-mode))
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
  (flycheck-add-mode 'javascript-eslint 'js-ts-mode)
  (flycheck-add-mode 'javascript-eslint 'tsx-ts-mode)
  (flycheck-add-mode 'javascript-eslint 'typescript-ts-mode)
  ;; 避免卡顿，设定语法检测的时机，延迟 1s
  ;; 1. 停止修改后
  ;; 2. 切换 buffer 后
  (setq flycheck-idle-change-delay 1
        flycheck-idle-buffer-switch-delay 1)
  (setq flycheck-check-syntax-automatically '(idle-change idle-buffer-switch)))

(use-package treesit-fold
  :ensure t
  :config
  (setq treesit-fold-range-alist
        (append treesit-fold-range-alist
                `((go-ts-mode . ,(treesit-fold-parsers-go)))))
  (global-treesit-fold-mode 1)
  (setq treesit-fold-summary-show t)
  (setq treesit-fold-line-count-show t)
  
  (with-eval-after-load 'go-ts-mode
    (bind-keys :map go-ts-mode-map
               ("C-'" . treesit-fold-toggle)
               ("C-:" . treesit-fold-open-all)
               ("C-;" . treesit-fold-close-all))))

(use-package go-ts-mode
  :ensure nil
  :config
  (setq go-ts-mode-indent-offset 4))

(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :bind (:map company-active-map
              ("C-n" . company-select-next)
              ("C-p" . company-select-previous))
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1
        company-tooltip-align-annotations t
        company-selection-wrap-around t
        company-transformers '(company-sort-by-occurrence)
        company-dabbrev-other-buffers nil))

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
        lsp-completion-no-cache t
        )
  :hook ((go-ts-mode . lsp-deferred)
         (python-mode . lsp-deferred)
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
  ;; 优化 JavaScript/TypeScript 的性能
  (setq lsp-javascript-display-return-type-hints nil)
  (setq lsp-javascript-display-variable-type-hints nil)
  (setq lsp-javascript-display-enum-member-value-hints nil)
  
  ;; 强制 JS/TS 缩进为 2 空格
  (setq lsp-javascript-format-insert-space-after-opening-and-before-closing-nonempty-braces nil)
  (setq lsp-typescript-format-insert-space-after-opening-and-before-closing-nonempty-braces nil)
  (setq lsp-javascript-format-indent-size 2)
  (setq lsp-typescript-format-indent-size 2)
  (setq lsp-javascript-format-tab-size 2)
  (setq lsp-typescript-format-tab-size 2)
  ;; Python LSP
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

(use-package python
  :ensure nil
  :mode "\\.py\\'"
  :init
  (setq python-shell-interpreter "python3")
  (set-variable 'py-indent-offset 4)
  (set-variable 'python-indent-guess-indent-offset nil))

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
                  (python-mode . black))
                apheleia-mode-alist)))

(provide 'init-lang)
