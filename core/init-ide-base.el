(use-package string-inflection
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

(use-package company
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

(use-package flycheck
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
  :ensure t
  :bind (("<f9> g" . git-gutter+-mode))
  :config (progn
            (define-key git-gutter+-mode-map (kbd "C-x n") 'git-gutter+-next-hunk)
            (define-key git-gutter+-mode-map (kbd "C-x p") 'git-gutter+-previous-hunk)
            (define-key git-gutter+-mode-map (kbd "C-x r") 'git-gutter+-revert-hunks))
  :diminish (git-gutter+-mode . "gg"))

(use-package eglot
  :ensure t
  :bind (:map eglot-mode-map
              ("<f9> s s" . eglot-reconnect)
              ("<f9> s d" . eldoc))
  :config
  (setq eldoc-echo-area-use-multiline-p nil
        eglot-ignored-server-capabilites '(:documentHighlightProvider))
  (add-to-list 'eglot-stay-out-of 'flymake) ;; disable flymake
)

(provide 'init-ide-base)
