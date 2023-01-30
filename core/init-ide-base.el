(use-package string-inflection
  :pin melpa-stable
  :ensure t
  :bind (:map prog-mode-map
              ("C-M-j" . my-string-inflection-cycle-auto))
  :init
  (defun my-string-inflection-cycle-auto ()
	"switching by major-mode"
	(interactive)
	(cond
	 ((eq major-mode 'emacs-lisp-mode)
      (string-inflection-all-cycle))
	 ((eq major-mode 'python-mode)
      (string-inflection-python-style-cycle))
	 ((eq major-mode 'go-mode) ;; golang use java style
      (string-inflection-java-style-cycle))
	 (t (string-inflection-all-cycle))))  ;; default
  )

;; https://github.com/purcell/exec-path-from-shell/issues/36
(use-package exec-path-from-shell
  :pin melpa
  :ensure t
  :defer 0.5
  :config
  (when (and window-system
             (memq window-system '(mac ns x)))
	(exec-path-from-shell-initialize)
	(exec-path-from-shell-copy-env "GOPATH")
	(progn (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO"))
			 (add-to-list 'exec-path-from-shell-variables var)))
	)
  )

(use-package lsp-mode
  :pin melpa
  :ensure t
  :hook ((lsp-mode-hook . lsp-enable-which-key-integration))
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
  (setq lsp-keymap-prefix "<f9> s")
  :config
  (setq lsp-enable-symbol-highlighting nil
		lsp-ui-doc-enable t
		lsp-headerline-breadcrumb-enable nil
		lsp-ui-sideline-enable nil
		lsp-signature-render-documentation nil
		)
  (setq lsp-diagnostic-package :none
		lsp-enable-folding nil
		lsp-enable-links nil
		lsp-restart 'auto-restart
		lsp-enable-snippet nil
		)
  ;; (push "[/\\\\]googleapis$" lsp-file-watch-ignored)
  )

(use-package lsp-ui
  :pin melpa
  :ensure t
  :commands lsp-ui-mode
  :hook (lsp-mode-hook . lsp-ui-mode)
  :bind (("<f9> s c" . lsp-ui-flycheck-list)
		 ("<f9> s m" . lsp-ui-imenu)
		 :map lsp-mode-map
		 ("M-." . lsp-ui-peek-find-definitions)
		 ("M-?" . lsp-ui-peek-find-references))
  :config
  (setq lsp-ui-doc-enable nil
		lsp-ui-doc-show-with-mouse nil
		lsp-ui-sideline-enable nil
		)
  )

(use-package company
  :pin melpa
  :ensure t
  :hook ((emacs-lisp-mode-hook . (lambda ()
								   (setq-local company-backends '(company-elisp))))
		 (emacs-lisp-mode-hook . company-mode))
  :bind (:map company-active-map
			  ("M-n" . nil)
			  ("M-p" . nil)
			  ("C-n" . company-select-next)
			  ("C-p" . company-select-previous)
			  )
  :config
  ;; markdown-mode, eshell-mode ignore complete
  (setq company-global-modes '(not markdown-mode gfm-mode eshell-mode)
		company-echo-delay 0
		company-idle-delay 0.1
		company-minimum-prefix-length 1)
  )

;; (use-package lsp-mode
;;   :pin melpa-stable
;;   :ensure t
;;   :hook
;;   ;; (emacs-lisp-mode-hook . lsp-deferred)
;;   (python-mode-hook . lsp-deferred)
;;   ;; (rjsx-mode . lsp) ; so slow
;;   (go-mode-hook . lsp-deferred)
;;   :commands lsp
;;   :bind (("<f9> s s" . lsp-workspace-restart)
;; 		 ("<f9> s r" . lsp-find-references)
;; 		 ("<f9> s d" . lsp-describe-thing-at-point)
;; 		 ("<f9> s i" . lsp-find-implementation)
;; 		 ("<f9> s h" . lsp-toggle-symbol-highlight)
;; 		 :map lsp-signature-mode
;; 		 ("<f9> s p" . lsp-signature-previous)
;; 		 ("<f9> s n" . lsp-signature-next)
;; 		 )
;;   :init
;;   ;; config from https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
;;   (setq lsp-enable-symbol-highlighting nil
;; 		lsp-ui-doc-enable t
;; 		lsp-ui-doc-show-with-cursor nil
;; 		lsp-lens-enable nil
;; 		lsp-headerline-breadcrumb-enable nil
;; 		lsp-ui-sideline-enable nil
;; 		lsp-modeline-code-actions-enable t
;; 		lsp-signature-render-documentation nil
;; 		lsp-completion-show-detail t
;; 		lsp-completion-show-kind t
;; 		)
;;   ;; other
;;   (setq lsp-diagnostic-package :none
;; 		lsp-enable-folding nil
;; 		lsp-enable-links nil
;; 		lsp-restart 'auto-restart
;; 		;; lsp-enable-snippet nil
;; 		;; python
;; 		lsp-pyls-server-command 'pyls
;; 		)
;;   :config
;;   (push "[/\\\\]googleapis$" lsp-file-watch-ignored)
;;   )

(use-package flycheck
  :pin melpa-stable
  :ensure t
  ;; :hook (prog-mode-hook . flycheck-mode)
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
