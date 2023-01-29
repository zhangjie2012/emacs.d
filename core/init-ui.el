(use-package all-the-icons
  :pin melpa
  :ensure t
  :if (display-graphic-p)
  :init
  (setq inhibit-compacting-font-caches t)
  )

(use-package all-the-icons-completion
  :pin melpa
  :ensure t
  :if (display-graphic-p)
  :config
  (all-the-icons-completion-mode)
  )

(use-package all-the-icons-dired
  :pin melpa
  :ensure t
  :if (display-graphic-p)
  :init (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  )

;; https://protesilaos.com/emacs/modus-themes
(use-package modus-themes
  :pin melpa
  :ensure t
  :demand t
  :config
  (setq modus-themes-italic-constructs nil
        modus-themes-bold-constructs nil
		modus-themes-org-blocks 'gray-background
		modus-themes-mixed-fonts t
		modus-themes-headings
		'((1 . (1.1))
          (2 . (1.05))
          (t . (1.0)))
		)
  (setq modus-themes-common-palette-overrides
		'(
		  (border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)
		  (fringe unspecified)
		  (bg-hover bg-yellow-intense)
		  )
		)
  (load-theme 'modus-operandi :no-confirm)
  :bind ("<f5>" . modus-themes-toggle)
  )

(use-package doom-modeline
  :pin melpa-stable
  :ensure t
  :config
  (setq doom-modeline-buffer-modification-icon nil)
  (setq doom-modeline-project-detection 'projectile)
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
