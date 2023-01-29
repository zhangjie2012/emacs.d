(use-package projectile
  :pin melpa-stable
  :ensure t
  :bind (:map projectile-mode-map
              ("<f8>" . projectile-command-map)
              ("C-c p" . projectile-command-map)
			  :map projectile-command-map
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
  (projectile-mode +1)
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
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations nil)
  (setq company-begin-commands '(self-insert-command)) ; start autocompletion only after typing
)

(use-package avy
  :pin melpa-stable
  :ensure t
  :bind (("M-s i" . avy-goto-word-1)
         ("M-s j" . avy-goto-line)
         ("M-s k" . avy-copy-line))
  :config
  (setq avy-background t)
  )

(use-package vertico
  :pin gnu
  :ensure t
  :init
  (vertico-mode)
  )

(use-package orderless
  :pin gnu
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))
  )

(use-package marginalia
  :pin gnu
  :ensure t
  :init
  (add-hook 'marginalia-mode-hook #'all-the-icons-completion-marginalia-setup)
  (marginalia-mode)
  )

(use-package consult
  :pin melpa
  :ensure t
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :bind (
		 ("C-x b" . consult-buffer)
		 ("M-y" . consult-yank-pop)
		 ("<f9> m" . consult-imenu)
		 ("M-s [" . consult-ripgrep)
		 ("M-s ]" . consult-git-grep)
		 ("C-s" . consult-line)
		 )
  :init
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)
  (advice-add #'register-preview :override #'consult-register-window)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (setq consult-preview-key (kbd "M-."))
  :config
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  )

(use-package ace-window
  :pin melpa-stable
  :ensure t
  :bind (("M-o" . ace-window)
		 ("M-s t" . ace-swap-window))
  :config
  (setq aw-keys '(?1 ?2 ?3 ?4 ?7 ?8 ?9 ?0))
  (set-face-attribute
   'aw-mode-line-face nil
   :inherit 'mode-line-buffer-id
   :foreground "chartreuse")
  )

(use-package expand-region
  :pin melpa-stable
  :ensure t
  :bind (("M-m" . er/expand-region)
		 ("M-s s" . er/mark-symbol)
		 ("M-s p" . er/mark-outside-pairs)
		 ("M-s P" . er/mark-inside-pairs)
		 ("M-s q" . er/mark-outside-quotes)
		 ("M-s m" . er/mark-comment)
		 ("M-s Q" . er/mark-inside-quotes)
		 ("M-s f" . er/mark-defun))
  )

(use-package multiple-cursors
  :pin melpa-stable
  :ensure t
  :bind (("M-s ;" . mc/mark-all-symbols-like-this-in-defun))
  )

(use-package highlight-symbol
  :pin melpa-stable
  :ensure t
  :bind (("M--" . highlight-symbol-at-point)
		 ("M-n" . highlight-symbol-next)
		 ("M-p" . highlight-symbol-prev))
  )

(use-package which-key
  :pin melpa-stable
  :ensure t
  :hook (prog-mode-hook . which-key-mode)
  :init
  (which-key-setup-minibuffer)
  )

(use-package dired-subtree
  :pin melpa
  :ensure t
  :init
  (setq dired-subtree-use-backgrounds nil)
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             (";" . dired-subtree-remove))
  )

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
  :config
  (when (and window-system
             (memq window-system '(mac ns x)))
	(exec-path-from-shell-initialize)
	(exec-path-from-shell-copy-env "GOPATH")
	(progn (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO"))
			 (add-to-list 'exec-path-from-shell-variables var)))
	)
  )

(use-package youdao-dictionary
  :pin melpa
  :ensure t
  :bind (
		 ("<f9> f" . youdao-dictionary-search-at-point+)
		 ("<f9> F" . youdao-dictionary-search-from-input)
		 )
  :config
  (setq url-automatic-caching t)
  (setq youdao-dictionary-use-chinese-word-segmentation t)
  )

;; -----------------------------------------------------------------------------

(defvar current-date-time-format "%Y-%m-%d %H:%M:%S"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defun insert-current-date-time ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
  (interactive)
  (insert (format-time-string current-date-time-format (current-time)))
  )

(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(global-set-key (kbd "M-*") 'match-paren)
(global-set-key (kbd "<f9> 1") 'delete-other-windows)
(global-set-key (kbd "<f9> 2") 'split-window-below)
(global-set-key (kbd "<f9> 3") 'split-window-horizontally)
(global-set-key (kbd "<f9> c") 'eshell)
(global-set-key (kbd "<f9> i") 'insert-current-date-time)
(global-set-key (kbd "<f9> d") 'dired-jump)
(global-set-key (kbd "<f9> w") 'save-buffer)
(global-set-key (kbd "<f10>") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-2") 'set-mark-command) ;; actual is C-@

(provide 'init-modernization)
