(use-package avy
  :pin melpa-stable
  :ensure t
  :bind (("M-s i" . avy-goto-word-1)
         ("M-s j" . avy-goto-line)
         ("M-s k" . avy-copy-line))
  :config
  (setq avy-background t)
  )

(use-package ivy
  :pin melpa-stable
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

(use-package swiper
  :pin melpa-stable
  :ensure t
  :bind (("C-s" . swiper)))

(use-package counsel
  :pin melpa-stable
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

(use-package ivy-rich
  :pin melpa
  :ensure t
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
  (setq ivy-rich-path-style 'abbrev)
  (ivy-rich-mode 1)
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
