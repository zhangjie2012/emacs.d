;; ;; https://protesilaos.com/emacs/modus-themes
;; (use-package modus-themes
;;   :ensure t
;;   :demand t
;;   :config
;;   (setq modus-themes-italic-constructs nil
;;         modus-themes-bold-constructs t
;;         modus-themes-org-blocks 'gray-background
;;         modus-themes-completions
;;         '((matches . (extrabold))
;;           (selection . (semibold text-also)))
;;         modus-themes-headings
;;         '((1 . (1.25))
;;           (2 . (1.15))
;;           (3 . (1.05))
;;           (t . (1.0)))
;;         )

;;   (setq modus-themes-common-palette-overrides
;;         '((bg-mode-line-active bg-blue-intense)
;;           (fg-mode-line-active fg-main)
;;           (border-mode-line-active unspecified)
;;           (border-mode-line-inactive unspecified)

;;           (fringe bg-blue-nuanced)
;;           (bg-hover bg-yellow-intense)

;;           (bg-region bg-lavender)
;;           (fg-region unspecified)

;;           (fg-heading-1 blue-warmer)
;;           (bg-heading-1 bg-blue-nuanced)
;;           ;; (overline-heading-1 blue-faint)

;;           (fg-heading-2 yellow-warmer)
;;           (bg-heading-2 bg-yellow-nuanced)

;;           (fg-heading-3 cyan-warmer)
;;           (bg-heading-3 bg-cyan-nuanced)

;;           (underline-link border)
;;           (underline-link-visited border)
;;           (underline-link-symbolic border)

;;           ;; (underline-err red-faint)
;;           ;; (underline-warning yellow-faint)
;;           ;; (underline-note cyan-faint)

;;           (fg-completion-match-0 blue)
;;           (fg-completion-match-1 magenta-warmer)
;;           (fg-completion-match-2 cyan)
;;           (fg-completion-match-3 red)
;;           ))
;;   (load-theme 'modus-operandi :no-confirm)
;;   :bind ("<f5>" . modus-themes-toggle))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p)
  :config
  (setq inhibit-compacting-font-caches t))

(use-package all-the-icons-dired
  :ensure t
  :if (display-graphic-p)
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package all-the-icons-ivy
  :ensure t
  :config
  (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

(use-package all-the-icons-ivy-rich
  :ensure t
  :config
  (all-the-icons-ivy-rich-mode 1))

(use-package doom-themes
  :ensure t
  :init
  (defun toggle-theme ()
    (interactive)
    (cond ((eq (car custom-enabled-themes) 'doom-solarized-light)
           (mapc #'disable-theme custom-enabled-themes)
           (load-theme 'doom-solarized-dark t))
          ((eq (car custom-enabled-themes) 'doom-solarized-dark)
           (mapc #'disable-theme custom-enabled-themes)
           (load-theme 'doom-solarized-light t))))

  ;; day/night use diff theme: via https://github.com/jakebox/jake-emacs
  (let ((hour (string-to-number (substring (current-time-string) 11 13))))
    (if (or (> hour 19) (< hour 7))
        (load-theme 'doom-solarized-dark t)
      (load-theme 'doom-solarized-light t)))
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic nil)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  (global-set-key (kbd "<f5>") 'toggle-theme))

(use-package doom-modeline
  :ensure t
  :config
  (setq doom-modeline-buffer-modification-icon nil
        doom-modeline-project-detection 'projectile
        doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-modeline-mode 1))

(use-package cnfonts
  :if window-system
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

  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.4))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.3))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
   )
  )

(use-package beacon
  :ensure t
  :bind (("<f12>" . beacon-blink))
  :config
  ;; (beacon-mode 1)
  (setq beacon-push-mark 35)
  (setq beacon-color "#FF8247")
  )

(provide 'init-ui)
