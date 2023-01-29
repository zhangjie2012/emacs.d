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
		 ("<f9> s m" . lsp-ui-imenu)
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
