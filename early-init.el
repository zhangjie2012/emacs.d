(setq gc-cons-threshold (* 1024 1024 100) ;; 100m
      read-process-output-max (* 4 1024 1024)
      gc-cons-percentage 0.6)

(setq default-frame-alist
      '((menu-bar-lines . 0)
        (tool-bar-lines . 0)
        (horizontal-scroll-bars)
        (vertical-scroll-bars)))

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(undecorated . t))

(setq frame-inhibit-implied-resize t)

(setenv "LSP_USE_PLISTS" "true")

(provide 'early-init)
