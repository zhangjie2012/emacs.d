;; encode

;; (add-hook 'text-mode-hook
;;           '(lambda ()
;;              (setq indent-tabs-mode nil)
;;              (setq tab-width 4)))
;; (setq indent-line-function (quote insert-tab))

;; (setq-default tab-width 4)
;; (fset 'yes-or-no-p 'y-or-n-p)


;; (save-place-mode t)
;; (add-hook 'before-save-hook 'delete-trailing-whitespace)

;; (setq visible-bell nil)
;; (setq ring-bell-function 'ignore)

;; (global-auto-revert-mode t)

;; (setq frame-title-format
;;       '((:eval (if (buffer-file-name)
;;                    (abbreviate-file-name (buffer-file-name))
;;                  "%b"))))


;; (show-paren-mode t)


(blink-cursor-mode -1)
(setq visible-cursor nil)
;; 启动时自动最大化窗口


(provide 'init-base)
