(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-kill-emacs  'y-or-n-p
      auto-save-default    nil
      ;; mouse-yank-at-point  t
      make-backup-files    nil
      create-lockfiles     nil)

(setq-default indent-tabs-mode nil)

(set-charset-priority 'unicode)
(setq locale-coding-system   'utf-8-unix)
(set-terminal-coding-system  'utf-8-unix)
(set-keyboard-coding-system  'utf-8-unix)
;; (set-selection-coding-system 'utf-8-unix)
(prefer-coding-system        'utf-8-unix)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix)
      default-buffer-file-coding-system 'utf-8-unix) ;; Windows/ Linux/Mac all LF

(setq system-time-locale "C")
;; (setq system-time-locale "zh_CN.UTF-8")


(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

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
      indicate-empty-lines t)

(setq use-short-answers t)

(blink-cursor-mode -1)
(setq visible-cursor nil)

(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t
      completion-ignore-case t)

(setq confirm-kill-processes nil)

(setq display-time-default-load-average nil
      display-time-format "[%m-%d %H:%M]")
;; (display-time-mode t)
;; (display-battery-mode 1)

;; SmoothScrolling https://www.emacswiki.org/emacs/SmoothScrolling
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)

(setq scroll-step 2
      scroll-margin 2
      hscroll-step 2
      hscroll-margin 2
      scroll-conservatively 101
      scroll-preserve-screen-position 'always)

(setq auto-window-vscroll nil)

(setq inhibit-compacting-font-caches t)

(setq load-prefer-newer t)

(provide 'init-base)
