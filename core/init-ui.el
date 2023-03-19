(use-package all-the-icons
  :ensure t
  :if (display-graphic-p)
  :config
  (setq inhibit-compacting-font-caches t))

(use-package all-the-icons-dired
  :after all-the-icons
  :ensure t
  :if (display-graphic-p)
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package doom-themes
  :ensure t
  :init
  (defun toggle-theme ()
    (interactive)
    (cond ((eq (car custom-enabled-themes) 'doom-palenight)
           (mapc #'disable-theme custom-enabled-themes)
           (load-theme 'doom-one-light t))
          ((eq (car custom-enabled-themes) 'doom-one-light)
           (mapc #'disable-theme custom-enabled-themes)
           (load-theme 'doom-palenight t))))

  ;; day/night use diff theme: via https://github.com/jakebox/jake-emacs
  (let ((hour (string-to-number (substring (current-time-string) 11 13))))
    (if (or (> hour 18) (< hour 7))
        (load-theme 'doom-palenight t)
      (load-theme 'doom-one-light t)))
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic nil)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config)
  (global-set-key (kbd "<f5>") 'toggle-theme))

(use-package doom-modeline
  :ensure t
  :config
  (setq doom-modeline-buffer-modification-icon t
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
