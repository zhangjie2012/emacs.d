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
        doom-themes-enable-italic nil)
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
   '(org-level-1 ((t (:inherit outline-1 :height 1.2))))
   '(org-level-2 ((t (:inherit outline-1 :height 1.0))))
   '(org-level-3 ((t (:inherit outline-1 :height 1.0))))))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(global-hl-line-mode +1)

(provide 'init-ui)
