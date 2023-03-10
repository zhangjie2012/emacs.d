(setq read-process-output-max (* 1024 1024))  ;; 1mb
(setq gc-cons-threshold (* 1024 1024 100))  ;; 100mb

(set-charset-priority 'unicode)
(setq locale-coding-system   'utf-8-unix)
(set-terminal-coding-system  'utf-8-unix)
(set-keyboard-coding-system  'utf-8-unix)
;; (set-selection-coding-system 'utf-8-unix)
(prefer-coding-system        'utf-8-unix)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix)
      default-buffer-file-coding-system 'utf-8-unix ; Windows/ Linux/Mac all LF
      )
(setq system-time-locale "C")
;; (setq system-time-locale "zh_CN.UTF-8")

(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs  'y-or-n-p
      auto-save-default    nil
      ;; mouse-yank-at-point  t
      make-backup-files    nil
      create-lockfiles     nil)

(setq-default indent-tabs-mode nil)

(setq default-frame-alist
      '((menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (horizontal-scroll-bars)
        (vertical-scroll-bars)))

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq frame-resize-pixelwise t)
(setq window-resize-pixelwise nil)

(setq scroll-conservatively 101)
(setq mouse-wheel-follow-mouse 't
      mouse-wheel-progressive-speed nil
      ;; The most important setting of all! Make each scroll-event move 2 lines at
      ;; a time (instead of 5 at default). Simply hold down shift to move twice as
      ;; fast, or hold down control to move 3x as fast. Perfect for trackpads.
      mouse-wheel-scroll-amount '(1 ((shift) . 3) ((control) . 6)))

(add-hook 'text-mode-hook
          #'(lambda ()
              (setq indent-tabs-mode nil)
              (setq tab-width 4)))
(setq indent-line-function (quote insert-tab))

(save-place-mode t)
(setq-default tab-width 4)

(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

(setq use-dialog-box nil)

(column-number-mode 1)

(electric-pair-mode 1)
(electric-indent-mode 1)
;; (electric-quote-mode 1)

(setq inhibit-startup-echo-area-message t
      inhibit-startup-message t
      indicate-empty-lines t
      )
(blink-cursor-mode -1)
(setq visible-cursor nil)

(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

(setq confirm-kill-processes nil)

(setq display-time-default-load-average nil
      display-time-format "[%m-%d %H:%M]"
      )
(display-time-mode t)
;; (display-battery-mode 1)

(provide 'init-early)
