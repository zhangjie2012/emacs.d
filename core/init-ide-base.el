(use-package projectile
  :pin melpa-stable
  :ensure t
  :bind-keymap (("<f8>" . projectile-command-map))
  :bind (:map projectile-command-map
			  ("F" . projectile-find-file-other-window)
			  ("w" . projectile-find-file-in-known-projects)
			  ("D" . projectile-dired-other-window)
			  ("k" . projectile-kill-buffers)
			  ("v" . projectile-vc)
			  ("b" . projectile-switch-to-buffer)
			  )
  :config
  ;; 打开项目缓存, 否则大的项目每次构建会比较慢
  ;; 你可以通过下面两个名称来清除缓存
  ;; - projectile-purge-file-from-cache
  ;; - projectile-purge-dir-from-cache
  (setq projectile-enable-caching t)
  ;; projectile 有三种构建索引的方式: native, hybird, alien
  ;;   native 使用 Emacs lisp 实现, hybird/alien 使用外部命令类似 find, git 来实现
  ;;   alien 优化了 hybird 的性能: 它不会对外部命令返回的结果做任何处理和排序, 以获得最好的性能
  ;;   使用外部命令的话, 类似 .gitignore 会自动生效
  ;; 注意: alien 会忽略 .projectile 文件
  (setq projectile-indexing-method 'alien)
  ;; 在每个目录下都可用(即使没有项目文件)
  (setq projectile-require-project-root 'prompt)
  ;; 对结果进行排序(active buffer + recently opened)
  (setq projectile-sort-order 'recentf-active)
  (setq projectile-completion-system 'ivy)

  ;; fix windows system "projectile-find-file" throw
  ;; 'tr' is not recognized as an internal or external command ...
  ;; via: https://github.com/bbatsov/projectile/issues/1302
  (setq projectile-git-submodule-command nil)
  (defun project-find-go-module (dir)
	(when-let ((root (locate-dominating-file dir "go.mod")))
      (cons 'go-module root)))

  (cl-defmethod project-root ((project (head go-module)))
	(cdr project))

  :init
  (use-package counsel-projectile
    :pin melpa
    :ensure t
    :config
    (counsel-projectile-mode 1)
    )
  ;; (projectile-mode +1)
  )

(use-package company
  :pin melpa
  :ensure t
  :hook ((prog-mode-hook . company-mode)
		 (protobuf-mode-hook . company-mode))
  :bind (:map company-active-map
			  ("M-n" . nil)
			  ("M-p" . nil)
			  ("C-n" . company-select-next)
			  ("C-p" . company-select-previous)
			  )
  :init
  ;; markdown-mode, eshell-mode ignore complete
  (setq company-global-modes '(not markdown-mode gfm-mode eshell-mode))
  (setq company-transformers '(company-sort-by-occurrence))
  (setq company-echo-delay 0)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
  )

(use-package company-box
  :pin melpa
  :ensure t
  :hook (company-mode . company-box-mode)
  )

(use-package lsp-mode
  :pin melpa-stable
  :ensure t
  :hook
  ;; (emacs-lisp-mode-hook . lsp-deferred)
  (python-mode-hook . lsp-deferred)
  ;; (rjsx-mode . lsp) ; so slow
  (go-mode-hook . lsp-deferred)
  :commands lsp
  :bind (("<f9> s s" . lsp-workspace-restart)
		 ("<f9> s r" . lsp-find-references)
		 ("<f9> s d" . lsp-describe-thing-at-point)
		 ("<f9> s i" . lsp-find-implementation)
		 ("<f9> s h" . lsp-toggle-symbol-highlight)
		 :map lsp-signature-mode
		 ("<f9> s p" . lsp-signature-previous)
		 ("<f9> s n" . lsp-signature-next)
		 )
  :init
  ;; config from https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
  (setq
   lsp-enable-symbol-highlighting nil
   lsp-ui-doc-enable t
   lsp-ui-doc-show-with-cursor nil
   lsp-lens-enable nil
   lsp-headerline-breadcrumb-enable nil
   lsp-ui-sideline-enable nil
   lsp-modeline-code-actions-enable t
   lsp-signature-render-documentation nil
   lsp-completion-show-detail t
   lsp-completion-show-kind t
   )
  ;; other
  (setq
   lsp-completion-enable t
   lsp-log-io nil
   lsp-diagnostic-package :none
   lsp-eldoc-render-all nil
   lsp-enable-folding nil
   lsp-enable-links nil
   lsp-restart 'auto-restart
   ;; lsp-enable-snippet nil
   ;; python
   lsp-pyls-server-command 'pyls
   )
  :config
  (push "[/\\\\]googleapis$" lsp-file-watch-ignored)
  )

(use-package lsp-ui
  :pin melpa-stable
  :ensure t
  :commands lsp-ui-mode
  :hook (lsp-mode-hook . lsp-ui-mode)
  :bind (("<f9> s c" . lsp-ui-flycheck-list)
		 :map lsp-mode-map
		 ("M-." . lsp-ui-peek-find-definitions)
		 ("M-?" . lsp-ui-peek-find-references))
  :init
  (setq
   lsp-ui-doc-enable nil
   lsp-ui-doc-show-with-cursor nil
   lsp-ui-doc-show-with-mouse nil
   lsp-ui-sideline-enable nil
   )
  )

(use-package flycheck
  :pin melpa-stable
  :ensure t
  :hook (prog-mode-hook . flycheck-mode)
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

  ;; 指定配置文件
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

(use-package git-gutter+
  :pin melpa-stable
  :ensure t
  :bind (("<f9> g" . git-gutter+-mode))
  :config (progn
            (define-key git-gutter+-mode-map (kbd "C-x n") 'git-gutter+-next-hunk)
            (define-key git-gutter+-mode-map (kbd "C-x p") 'git-gutter+-previous-hunk)
            (define-key git-gutter+-mode-map (kbd "C-x r") 'git-gutter+-revert-hunks))
  :diminish (git-gutter+-mode . "gg"))

(provide 'init-ide-base)
