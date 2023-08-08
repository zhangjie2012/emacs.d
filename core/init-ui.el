;; (use-package doom-themes
;;   :ensure t
;;   :init
;;   (defun toggle-theme ()
;;     (interactive)
;;     (cond ((eq (car custom-enabled-themes) 'doom-one)
;;            (mapc #'disable-theme custom-enabled-themes)
;;            (load-theme 'doom-snazzy t))
;;           ((eq (car custom-enabled-themes) 'doom-snazzy)
;;            (mapc #'disable-theme custom-enabled-themes)
;;            (load-theme 'doom-one t))))
;;   ;; day/night use diff theme: via https://github.com/jakebox/jake-emacs
;;   (let ((hour (string-to-number (substring (current-time-string) 11 13))))
;;     (if (or (> hour 17) (< hour 7))
;;         (load-theme 'doom-one t)
;;       (load-theme 'doom-snazzy t)))
;;   :config
;;   (setq doom-themes-enable-bold t
;;         doom-themes-enable-italic nil)
;;   (doom-themes-visual-bell-config)
;;   (doom-themes-org-config)
;;   (global-set-key (kbd "<f5>") 'toggle-theme))

(use-package doom-themes
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (load-theme 'doom-one-light t))

(use-package nerd-icons
  :ensure t
  :when (display-graphic-p)
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package nerd-icons-dired
  :ensure t
  :after nerd-icons
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  :config
  (setq doom-modeline-buffer-modification-icon nil
        doom-modeline-project-detection 'project
        doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-unicode-fallback t))

(use-package emacs
  :ensure nil
  :bind (("C--" . text-scale-decrease)
         ("C-=" . text-scale-increase)
         ("C-0" . text-scale-adjust))
  :config
  (set-face-attribute 'default nil :family "Sarasa Term SC Nerd" :height 180)
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.5))))
   '(org-level-2 ((t (:inherit outline-1 :height 1.35))))
   '(org-level-3 ((t (:inherit outline-1 :height 1.25))))
   '(org-level-4 ((t (:inherit outline-1 :height 1.0))))))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package pulsar
  :ensure t
  :bind ("M-h" . pulsar-pulse-line-blue))

(global-hl-line-mode +1)

(use-package dashboard
  :ensure t
  :after (nerd-icons)
  :init
  (setq dashboard-display-icons-p t)
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  :config
  (setq dashboard-projects-backend 'project-el
        dashboard-items '((projects . 8)
                          (recents . 8)
                          (agenda . 8))
        dashboard-banner-logo-title "为天地立心, 为生民立命; 为往圣继绝学, 为万世开太平"
        dashboard-footer-messages '("https://github.com/zhangjie2012/emacs.d")
        dashboard-startup-banner (concat user-emacs-directory "logos/cacodemon.svg")
        dashboard-image-banner-max-height 160
        dashboard-set-navigator t
        dashboard-set-footer nil
        dashboard-show-shortcuts nil)
  (setq dashboard-agenda-tags-format 'ignore
        dashboard-agenda-sort-strategy '(priority-down)
        dashboard-week-agenda t)
  (setq dashboard-navigator-buttons
        `((;; homepage
           (,(nerd-icons-octicon "nf-oct-home" :height 1.0 :v-adjust 0.0)
            "Homepage"
            "Go to homepage"
            (lambda (&rest _) (browse-url "https://www.zhangjiee.com/")))
           ;; Github
           (,(nerd-icons-octicon "nf-oct-mark_github" :height 1.0 :v-adjust 0.0)
            "Github"
            "Go to github"
            (lambda (&rest _) (browse-url "https://github.com/zhangjie2012")))
           )))
  (dashboard-setup-startup-hook))

(provide 'init-ui)
