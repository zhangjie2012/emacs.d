(use-package modus-themes
  :pin melpa-stable
  :ensure t
  :init
  (setq
   modus-themes-fringes 'subtle
   modus-themes-lang-checkers '(intense)
   modus-themes-syntax nil
   modus-themes-hl-line '(accented)
   modus-themes-deuteranopia nil
   modus-themes-subtle-line-numbers nil
   modus-themes-paren-match '(bold intense)
   modus-themes-prompts nil
   modus-themes-region '(bg-only no-extend)
   modus-themes-diffs 'fg-only-deuteranopia
   modus-themes-org-blocks 'gray-background
   modus-themes-mode-line '(accented borderless)
   modus-themes-links nil
   modus-themes-box-buttons '(variable-pitch flat faint 0.9)
   modus-themes-completions '((matches . (extrabold))
                              (selection . (semibold accented))
                              (popup . (accented intense)))
   ;; modus-themes-mixed-fonts t
   modus-themes-headings ; this is an alist: read the manual or its doc string
   '((1 . (overline background 1.15))
     (2 . (rainbow background 1.0))
     (t . (background 1.0)))
   )

  ;; https://gitlab.com/protesilaos/modus-themes/-/issues/226
  (setq compilation-message-face nil)

  (modus-themes-load-themes)
  :bind ("<f5>" . modus-themes-toggle)
  :config
  (modus-themes-load-operandi)
  )

(use-package doom-modeline
  :pin melpa-stable
  :ensure t
  :config
  ;; (set-face-foreground 'doom-modeline-buffer-modified "brightyellow")
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
  (setq doom-modeline-env-version t)
  (setq doom-modeline-window-width-limit fill-column)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-indent-info t)

  (doom-modeline-mode 1)
  )

(use-package beacon
  :pin melpa-stable
  :ensure t
  :config
  (beacon-mode 1)
  (setq beacon-push-mark 35)
  (setq beacon-color "#666600")
  )

(use-package rainbow-delimiters
  :pin melpa-stable
  :ensure t
  :hook (prog-mode-hook . rainbow-delimiters-mode)
  )

;; line number
(set-face-foreground 'line-number "darkgrey")
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
  ;; (setq cnfonts-use-face-font-rescale t)
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
