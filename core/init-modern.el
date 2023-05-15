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
         ("M-s c" . avy-goto-char)
         ("M-s k" . avy-copy-line))
  :config
  (setq avy-case-fold-search nil)
  (setq avy-keys (number-sequence ?a ?z))
  (setq avy-highlight-first t)
  (setq avy-background t))

(use-package project
  :ensure nil
  :bind-keymap ("<f8>" . project-prefix-map))

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

(use-package highlight-symbol
  :ensure t
  :bind (("M--" . highlight-symbol-at-point)
	     ("M-n" . highlight-symbol-next)
	     ("M-p" . highlight-symbol-prev)))

(use-package which-key
  :ensure t
  :hook (prog-mode . which-key-mode)
  :init
  (which-key-setup-minibuffer))

(use-package youdao-dictionary
  :ensure t
  :bind (("<f9> f" . youdao-dictionary-search-at-point+)
	     ("<f9> F" . youdao-dictionary-search-from-input))
  :config
  (setq url-automatic-caching t)
  (setq youdao-dictionary-use-chinese-word-segmentation t))

;; line number
;; (set-face-foreground 'line-number "darkgrey")
(global-set-key (kbd "M-s l") 'display-line-numbers-mode)

(use-package linum-relative
  :ensure t
  :bind (("M-s r" . linum-relative-toggle))
  :config
  (setq linum-relative-backend 'display-line-numbers-mode))

(use-package display-fill-column-indicator
  :pin manual
  :custom
  (display-fill-column-indicator-column 120)
  (display-fill-column-indicator-character ?\u2502)
  :config
  (global-set-key (kbd "M-s n") 'display-fill-column-indicator-mode))

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
	 (t (string-inflection-all-cycle)))))

;; https://github.com/purcell/exec-path-from-shell/issues/36
(use-package exec-path-from-shell
  :ensure t
  :when (eq system-type 'darwin)
  :hook (after-init . exec-path-from-shell-initialize))

;; (use-package exec-path-from-shell
;;   :ensure t
;;   :defer 0.5
;;   :when (eq system-type 'darwin)
;;   :config
;;   (when (and window-system
;;              (memq window-system '(mac ns x)))
;; 	(exec-path-from-shell-initialize)
;; 	(exec-path-from-shell-copy-env "GOPATH")
;; 	(progn (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID" "GPG_AGENT_INFO"))
;; 			 (add-to-list 'exec-path-from-shell-variables var)))))

(use-package magit
  :ensure t
  :bind (("M-s ," . magit-status))
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))

(use-package emacs
  :ensure nil
  :init
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

  (defun toggle-frame-alpha ()
    (interactive)
    (let* ((pair (or (frame-parameter nil 'alpha) '(100 100)))
           (alpha (apply '+ pair)))
      (set-frame-parameter nil
                           'alpha
                           (if (or (null alpha) (eq alpha 200) (eq alpha 2.0))
                               '(80 60) '(100 100)))))

  (global-set-key (kbd "<f10>") 'toggle-frame-fullscreen)
  (global-set-key (kbd "<f6>") 'show-file-name)
  (global-set-key (kbd "<f9> 1") 'delete-other-windows)
  (global-set-key (kbd "<f9> 2") 'split-window-below)
  (global-set-key (kbd "<f9> 3") 'split-window-horizontally)
  (global-set-key (kbd "<f9> <SPC>") 'toggle-frame-alpha)
  (global-set-key (kbd "<f9> c") 'eshell)
  (global-set-key (kbd "<f9> d") 'dired-jump)
  (global-set-key (kbd "<f9> i") 'insert-current-date-time)
  (global-set-key (kbd "<f9> w") 'save-buffer)
  (global-set-key (kbd "C-2") 'set-mark-command) ;; actual is C-@
  (global-set-key (kbd "C-x k") 'kill-this-buffer) ;; kill-this-buffer replace kill-buffer
  (global-set-key (kbd "M-*") 'match-paren)
  (global-set-key (kbd "S-<backspace>") 'kill-whole-line))

(provide 'init-modern)
