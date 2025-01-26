(use-package whitespace
  :pin manual
  :ensure t
  :demand
  :init
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(use-package avy
  :ensure t
  :bind (("M-i" . avy-goto-word-1)
         ("M-j" . avy-goto-line)
         ("M-s i" . avy-goto-char)
         ("M-s k" . avy-copy-line))
  :config
  (setq avy-case-fold-search nil)
  (setq avy-keys (number-sequence ?a ?z))
  (setq avy-highlight-first t)
  (setq avy-background t))

(use-package project
  :ensure nil
  :bind (("<f8> f" . project-find-file)
         ("<f8> p" . project-switch-project)
         ("<f8> k" . project-kill-buffers)
         ("<f8> b" . project-switch-to-buffer)
		 ("<f8> c" . project-compile)))

(use-package expand-region
  :ensure t
  :bind (("M-m" . er/expand-region)
	     ("M-s s" . er/mark-symbol)
	     ("M-s p" . er/mark-outside-pairs)
	     ("M-s P" . er/mark-inside-pairs)
	     ("M-s q" . er/mark-outside-quotes)
	     ("M-s m" . er/mark-comment)
	     ("M-s Q" . er/mark-inside-quotes)
	     ("M-s f" . er/mark-defun)))

(use-package multiple-cursors
  :ensure t
  :bind (("M-s ;" . mc/mark-all-symbols-like-this-in-defun)
         ("C-M-n" . mc/mark-next-like-this)
         ("C-M-p" . mc/mark-previous-like-this)))

(use-package symbol-overlay
  :ensure t
  :bind (("M--" . symbol-overlay-put)
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev)))

(use-package which-key
  :ensure t
  :hook (prog-mode . which-key-mode)
  :config
  (which-key-setup-minibuffer))

(use-package display-fill-column-indicator
  :pin manual
  :custom
  (display-fill-column-indicator-column 120)
  (display-fill-column-indicator-character ?\u2502))

(use-package linum-relative
  :ensure t
  :config
  (setq linum-relative-backend 'display-line-numbers-mode))

;; (use-package git-gutter+
;;   :ensure t
;;   :bind ("<f8> g" . git-gutter+-mode))

;; https://github.com/purcell/exec-path-from-shell/issues/36
(use-package exec-path-from-shell
  :ensure t
  :when (eq system-type 'darwin)
  :hook (after-init . exec-path-from-shell-initialize))

(use-package magit
  :ensure t
  :bind (("M-s ," . magit-status))
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package fanyi
  :ensure t
  :bind (("<f8> y" . fanyi-dwim2))
  :config
  (set-fontset-font t 'emoji (font-spec :family "Apple Color Emoji") nil 'prepend)
  :custom
  (fanyi-providers '(;; 海词
                     fanyi-haici-provider
                     ;; 有道同义词词典
                     fanyi-youdao-thesaurus-provider
                     ;; longman
                     fanyi-longman-provider)))

(use-package keyfreq
  :ensure t
  :config
  (setq keyfreq-excluded-commands
        '(abort-minibuffers
          backward-char
          backward-delete-char-untabify
          backward-word
          delete-backward-char
          eval-last-sexp
          execute-extended-command
          forward-char
          forward-word
          left-char
          mouse-drag-region
          mouse-set-point
          move-beginning-of-line
          move-end-of-line
          mwheel-scroll
          newline
          next-line
          org-self-insert-command
          pixel-scroll-precision
          previous-line
          right-char
          save-buffer
          self-insert-command
          vertico-exit
          vertico-next
          vertico-previous
          org-delete-backward-char))
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))

(provide 'init-modern)
