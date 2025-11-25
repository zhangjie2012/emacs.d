(fset 'yes-or-no-p 'y-or-n-p)

(setq
 confirm-kill-emacs 'y-or-n-p
 auto-save-default nil
 make-backup-files nil
 create-lockfiles nil
 use-short-answers t
 use-dialog-box nil
 inhibit-startup-message t
 inhibit-startup-echo-area-message t
 indicate-empty-lines t
 confirm-kill-processes nil)

(set-charset-priority 'unicode)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8-unix)

(setq
 locale-coding-system 'utf-8-unix
 default-buffer-file-coding-system 'utf-8-unix
 default-process-coding-system '(utf-8-unix . utf-8-unix))

(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)

(setq system-time-locale "C")

(setq frame-title-format
      '((:eval (if buffer-file-name
                   (abbreviate-file-name buffer-file-name)
                 "%b"))))


(setq-default indent-tabs-mode nil
              tab-width 4)

(add-hook 'text-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil
                  tab-width 4)))

;; 现代 advice (替代 defadvice/ad-activate)
(defun my/align-regexp-no-tabs (orig-fun &rest args)
  (let ((indent-tabs-mode nil))
    (apply orig-fun args)))

(advice-add 'align-regexp :around #'my/align-regexp-no-tabs)

(save-place-mode 1)
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

(column-number-mode 1)

(electric-pair-mode 1)
(electric-indent-mode 1)

(blink-cursor-mode 1)
(setq-default cursor-type 'bar)
(setq visible-cursor nil)

(setq
 read-file-name-completion-ignore-case t
 read-buffer-completion-ignore-case t
 completion-ignore-case t)

(setq display-time-default-load-average nil
      display-time-format "%H:%M")
(display-time-mode 1)
(display-battery-mode 1)

(setq frame-resize-pixelwise t)
(pixel-scroll-precision-mode 1)

(setq
 mouse-wheel-scroll-amount '(1 ((shift) . 1))
 mouse-wheel-progressive-speed nil
 mouse-wheel-follow-mouse t
 scroll-step 1)

(setq inhibit-compacting-font-caches t)
(setq load-prefer-newer t)

(provide 'init-base)
