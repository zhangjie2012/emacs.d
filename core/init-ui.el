(use-package all-the-icons
  :pin melpa-stable
  :ensure t
  :if (display-graphic-p)
  :init
  (setq inhibit-compacting-font-caches t)
  )

(use-package all-the-icons-ivy-rich
  :pin melpa-stable
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1)
  :config
  (setq all-the-icons-ivy-rich-icon t)
  (setq all-the-icons-ivy-rich-color-icon t)
  (setq all-the-icons-ivy-rich-icon-size 1.0)
  (setq all-the-icons-ivy-rich-project t)
  (setq inhibit-compacting-font-caches t)
  )

(use-package all-the-icons-dired
  :pin melpa
  :ensure
  :init (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  )

(use-package doom-themes
  :pin melpa
  :ensure t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic nil)
  (load-theme 'doom-one-light t)
  (doom-themes-org-config)
  )

(use-package doom-modeline
  :pin melpa-stable
  :ensure t
  :config
  ;; (set-face-foreground 'doom-modeline-buffer-modified "orangered")
  ;; (set-face-foreground 'doom-modeline-buffer-major-mode "systemBrownColor")
  ;; (set-face-foreground 'doom-modeline-buffer-minor-mode "systemBrownColor")
  ;; (set-face-foreground 'doom-modeline-project-dir "systemBrownColor")
  ;; (set-face-foreground 'doom-modeline-project-root-dir "systemBrownColor")
  ;; (set-face-foreground 'doom-modeline-project-parent-dir "systemBrownColor")
  ;; (set-face-foreground 'doom-modeline-project-dir "white")
  ;; (set-face-foreground 'doom-modeline-buffer-file "systemBrownColor")
  (setq doom-modeline-buffer-modification-icon nil)
  (setq doom-modeline-project-detection 'projectile)
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (setq doom-modeline-env-version nil)
  (setq doom-modeline-window-width-limit fill-column)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-indent-info nil)
  (doom-modeline-mode 1)
  )

(use-package beacon
  :pin melpa
  :ensure t
  :bind (("<f12>" . beacon-blink))
  ;; :config
  ;; (beacon-mode 1)
  ;; (setq beacon-push-mark 35)
  ;; (setq beacon-color "#666600")
  )

(use-package rainbow-delimiters
  :pin melpa-stable
  :ensure t
  :hook (prog-mode-hook . rainbow-delimiters-mode)
  )

;; line number
;; (set-face-foreground 'line-number "darkgrey")
(global-set-key (kbd "M-s l") 'display-line-numbers-mode)

(use-package linum-relative
  :pin melpa-stable
  :ensure t
  :bind (("M-s r" . linum-relative-toggle))
  :init
  (setq linum-relative-backend 'display-line-numbers-mode)
  )

(use-package display-fill-column-indicator
  :pin manual
  :custom
  (display-fill-column-indicator-column 120)
  (display-fill-column-indicator-character ?\u2502)
  :config
  (global-set-key (kbd "M-s n") 'display-fill-column-indicator-mode)
)

(use-package cnfonts
  :if window-system
  :pin melpa
  :ensure t
  :init
  ;; https://github.com/tumashu/cnfonts/issues/138
  (setq cnfonts-use-face-font-rescale t)
  (setq cnfonts-use-system-type t)
  :config
  (cnfonts-mode 1)
  ;; https://stackoverflow.com/questions/1817257/how-to-determine-operating-system-in-elisp
  (if (eq system-type 'darwin)
	  (cnfonts--select-profile "profile1"))
  (if (eq system-type 'windows-nt)
	  (cnfonts--select-profile "profile2"))
  (if (eq system-type 'gnu/linux)
	  (cnfonts--select-profile "profile3"))
  (define-key cnfonts-mode-map (kbd "C--") #'cnfonts-decrease-fontsize)
  (define-key cnfonts-mode-map (kbd "C-=") #'cnfonts-increase-fontsize)
  )

(provide 'init-ui)
