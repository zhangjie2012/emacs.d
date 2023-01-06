(set-charset-priority 'unicode)
(setq locale-coding-system   'utf-8-unix)
(set-terminal-coding-system  'utf-8-unix)
(set-keyboard-coding-system  'utf-8-unix)
;; (set-selection-coding-system 'utf-8-unix)
(prefer-coding-system        'utf-8-unix)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; make unix lineendings default, 不管 Windows 还 Linux/Mac 文件格式统一为 LF
(setq default-buffer-file-coding-system 'utf-8-unix)

(setq system-time-locale "C")
;; (setq system-time-locale "zh_CN.UTF-8")

(setq confirm-kill-emacs  'y-or-n-p
      auto-save-default    nil
      mouse-yank-at-point  t
      make-backup-files    nil
      indent-tabs-mode     nil
      create-lockfiles     nil)

(add-hook 'text-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)
             (setq tab-width 4)))
(setq indent-line-function (quote insert-tab))

(setq-default tab-width 4)
(fset 'yes-or-no-p 'y-or-n-p)

(electric-pair-mode nil)
(electric-indent-mode 1)
;; (electric-quote-mode 1)

(save-place-mode t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq visible-bell nil)
(setq ring-bell-function 'ignore)

(global-auto-revert-mode t)

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-message t)
(setq indicate-empty-lines t)
(setq show-trailing-whitespace t)
(show-paren-mode t)
(tool-bar-mode -1)
(set-scroll-bar-mode nil)
(menu-bar-mode -1)
(column-number-mode t)
(blink-cursor-mode -1)
(setq visible-cursor nil)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq display-time-default-load-average nil
	  display-time-format "%H:%M")
(display-time-mode t)
(display-battery-mode 1)

(provide 'init-base)
