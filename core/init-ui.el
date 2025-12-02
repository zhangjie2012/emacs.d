(use-package doom-themes
  :ensure t
  :init
  (defun toggle-theme ()
    (interactive)
    (cond ((eq (car custom-enabled-themes) 'doom-one-light)
           (mapc #'disable-theme custom-enabled-themes)
           (load-theme 'doom-one t))
          ((eq (car custom-enabled-themes) 'doom-one)
           (mapc #'disable-theme custom-enabled-themes)
           (load-theme 'doom-one-light t))))
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  (load-theme 'doom-one-light t)
  (global-set-key (kbd "<f12>") 'toggle-theme))

(use-package nerd-icons
  :ensure t
  :when (display-graphic-p)
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono"))

(use-package nerd-icons-dired
  :ensure t
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  (setq doom-modeline-height 25)
  :config
  (setq doom-modeline-buffer-modification-icon nil)
  (setq doom-modeline-project-detection 'auto)
  (setq doom-modeline-buffer-file-name-style 'relative-to-project)
  (setq doom-modeline-unicode-fallback t)
  (setq doom-modeline-enable-word-count nil
        doom-modeline-vcs-max-length 20
        doom-modeline-buffer-encoding nil))

(use-package emacs
  :ensure nil
  :bind (("C--" . text-scale-decrease)
         ("C-=" . text-scale-increase)
         ("C-0" . text-scale-adjust))
  :config
  (set-face-attribute 'default nil :family "Maple Mono NF CN" :height 160)
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.5))))
   '(org-level-2 ((t (:inherit outline-1 :height 1.2))))
   '(org-level-3 ((t (:inherit outline-1 :height 1.0))))
   '(org-level-4 ((t (:inherit outline-1 :height 1.0))))))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(global-hl-line-mode +1)

(use-package dashboard
  :ensure t
  :after nerd-icons
  :init
  (setq dashboard-display-icons-p t
        dashboard-icon-type 'nerd-icons
        dashboard-set-heading-icons t
        dashboard-set-file-icons t)
  (setq dashboard-startupify-list-delay 0.2)
  (setq dashboard-week-agenda t
        dashboard-agenda-tags-format 'ignore
        dashboard-agenda-sort-strategy '(priority-down))
  :config
  ;; project.el 的检索缓存，加速项目列表
  (setq project--list-hidden-projects t)
  (setq dashboard--banner-cache t)
  (setq dashboard-items-default-length 8)
  (setq dashboard-projects-backend 'project-el
        dashboard-items '((projects . 8)
                          (recents . 8))
        dashboard-banner-logo-title "不二"
        dashboard-footer-messages '("https://github.com/zhangjie2012/emacs.d")
        dashboard-startup-banner (concat user-emacs-directory "logos/cacodemon.svg")
        dashboard-image-banner-max-height 160
        dashboard-set-navigator t
        dashboard-set-footer nil
        dashboard-show-shortcuts nil)
  (setq dashboard-navigator-buttons
        `(((,(nerd-icons-octicon "nf-oct-home" :height 1.0 :v-adjust 0.0)
            "Homepage"
            "Go to homepage"
            (lambda (&rest _)
              (browse-url "https://www.zhangjiee.com/")))
           (,(nerd-icons-octicon "nf-oct-mark_github" :height 1.0 :v-adjust 0.0)
            "Github"
            "Go to github"
            (lambda (&rest _)
              (browse-url "https://github.com/zhangjie2012"))))))
  (add-hook 'emacs-startup-hook #'dashboard-refresh-buffer)
  (dashboard-setup-startup-hook))

(use-package ansi-color
  :ensure t
  :hook (compilation-filter . ansi-color-compilation-filter))

(provide 'init-ui)
