;; https://protesilaos.com/emacs/modus-themes
(use-package modus-themes
  :pin melpa
  :ensure t
  :demand t
  :config
  (setq modus-themes-italic-constructs nil
        modus-themes-bold-constructs nil
		modus-themes-org-blocks 'gray-background
		;; modus-themes-mixed-fonts t
		modus-themes-headings
		'((1 . (1.2))
          (2 . (1.1))
		  (3 . (1.05))
          (t . (1.0)))
		)
  (setq modus-themes-common-palette-overrides
		'(
		  (border-mode-line-active unspecified)
          (border-mode-line-inactive unspecified)
		  (fringe unspecified)
		  (bg-hover bg-yellow-intense)

		  (fg-heading-1 blue-warmer)
		  (bg-heading-1 bg-blue-nuanced)
          (fg-heading-2 yellow-cooler)
		  (bg-heading-2 bg-yellow-nuanced)
          (fg-heading-3 cyan-cooler)
		  (bg-heading-3 bg-cyan-nuanced)

		  (underline-link border)
          (underline-link-visited border)
          (underline-link-symbolic border)
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

(use-package beacon
  :pin melpa
  :ensure t
  :bind (("<f12>" . beacon-blink))
  ;; :config
  ;; (beacon-mode 1)
  ;; (setq beacon-push-mark 35)
  ;; (setq beacon-color "#666600")
  )

(provide 'init-ui)
