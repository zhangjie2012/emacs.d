(use-package whitespace
  :pin manual
  :ensure t
  :demand
  :init
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  ;; :config
  ;; (defun no-trailing-whitespace ()
  ;;   (setq show-trailing-whitespace nil))
  ;; (add-hook 'minibuffer-setup-hook 'no-trailing-whitespace)
  ;; (add-hook 'calendar-setup-hook 'no-trailing-whitespace)
  ;; (set-face-attribute 'trailing-whitespace nil :background "indian red")
  ;; (setq-default show-trailing-whitespace t)
  )

(use-package avy
  :ensure t
  :bind (("M-s i" . avy-goto-word-1)
         ("M-s c" . avy-goto-char)
         ("M-s j" . avy-goto-line)
         ("M-s k" . avy-copy-line)
         )
  :config
  (setq avy-background t)
  )

(use-package ivy
  :ensure t
  :bind (("C-x b" . ivy-switch-buffer)
	     ("<f6>" . ivy-resume))
  :init
  (setq ivy-use-virtual-buffers nil)
  (setq ivy-count-format "(%d-%d) ")
  (setq enable-recursive-minibuffers t)
  :config
  (ivy-mode 1)
  )

(use-package ivy-rich
  :ensure t
  :config
  (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  )

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))

(use-package counsel
  :ensure t
  :bind (("M-s [" . counsel-rg)
	     ("M-s ]" . counsel-git-grep)
	     ("M-x" . counsel-M-x)
	     ("M-y" . counsel-yank-pop)
	     ("C-x C-f" . counsel-find-file)
         ("<f9> m" . counsel-semantic-or-imenu)
	     )
  :config
  (setq counsel-rg-base-command "rg -i --max-columns 240 --no-heading --with-filename --line-number %s")
  )

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
	     ("C-;" . embark-dwim))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  )

(use-package projectile
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
  (use-package counsel-projectile
    :ensure t
    :config
    (counsel-projectile-mode 1)
    )

  ;; (projectile-mode +1)
  )

(use-package ace-window
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
  :ensure t
  :bind (("M-s ;" . mc/mark-all-symbols-like-this-in-defun))
  )

(use-package highlight-symbol
  :ensure t
  :bind (("M--" . highlight-symbol-at-point)
	     ("M-n" . highlight-symbol-next)
	     ("M-p" . highlight-symbol-prev))
  )

(use-package which-key
  :ensure t
  :hook (prog-mode-hook . which-key-mode)
  :init
  (which-key-setup-minibuffer)
  )

(use-package dired-subtree
  :ensure t
  :init
  (setq dired-subtree-use-backgrounds nil)
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             (";" . dired-subtree-remove))
  )

(use-package youdao-dictionary
  :ensure t
  :bind (
	     ("<f9> f" . youdao-dictionary-search-at-point+)
	     ("<f9> F" . youdao-dictionary-search-from-input)
	     )
  :config
  (setq url-automatic-caching t)
  (setq youdao-dictionary-use-chinese-word-segmentation t)
  )

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode-hook . rainbow-delimiters-mode)
  )

;; line number
;; (set-face-foreground 'line-number "darkgrey")
(global-set-key (kbd "M-s l") 'display-line-numbers-mode)

(use-package linum-relative
  :ensure t
  :bind (("M-s r" . linum-relative-toggle))
  :config
  (setq linum-relative-backend 'display-line-numbers-mode)
  )

(use-package display-fill-column-indicator
  :pin manual
  :custom
  (display-fill-column-indicator-column 120)
  (display-fill-column-indicator-character ?\u2502)
  :config
  (global-set-key (kbd "M-s n") 'display-fill-column-indicator-mode)
  )

(use-package git-gutter+
  :ensure t
  :bind (("<f9> g" . git-gutter+-mode))
  :config (progn
            (define-key git-gutter+-mode-map (kbd "C-x n") 'git-gutter+-next-hunk)
            (define-key git-gutter+-mode-map (kbd "C-x p") 'git-gutter+-previous-hunk)
            (define-key git-gutter+-mode-map (kbd "C-x r") 'git-gutter+-revert-hunks))
  :diminish (git-gutter+-mode . "gg"))

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
         (prog-mode-hook . company-mode)
         )
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
(global-set-key (kbd "<f6>") 'show-file-name)
(global-set-key (kbd "<f10>") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-2") 'set-mark-command) ;; actual is C-@

(provide 'init-modernization)
