(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :ensure t
  :init
  (vertico-mode)
  ;; (setq vertico-scroll-margin 0)
  (setq vertico-resize nil)
  (setq vertico-cycle nil))

(use-package consult
  :after vertico
  :ensure t
  :bind (("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop)
         ("C-s" . consult-line)
         ("<f9> m" . consult-imenu)
         ("M-s [" . consult-ripgrep)
         ("M-s ]" . consult-git-grep)
         )
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key 'any)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  )

(use-package embark
  :ensure t
  :bind (("C-." . embark-act)
	     ("C-;" . embark-dwim))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package cape
  :ensure t
  :init
  ;; (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-history)
  ;; (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;; (add-to-list 'completion-at-point-functions #'cape-symbol)
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  )

(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.0)
  (corfu-quit-no-match t)
  (corfu-excluded-modes '(org-mode markdown-mode eshell-mode))
  :bind (:map corfu-map
              ("M-SPC" . corfu-insert-separator)
              ("C-n" . corfu-next)
              ("C-p" . corfu-previous))
  :init
  (global-corfu-mode)
  (corfu-history-mode))

(use-package prescient
  :ensure t
  :config
  (setq-default history-length 1000)
  (setq-default prescient-history-length 1000)
  (prescient-persist-mode +1))

(use-package corfu-prescient
  :after corfu
  :ensure t
  :config
  (corfu-prescient-mode +1))

(use-package vertico-prescient
  :after vertico
  :ensure t
  :config
  (vertico-prescient-mode +1))

(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window)
	     ("M-s t" . ace-swap-window))
  :config
  (setq aw-keys '(?1 ?a ?w ?x ?7 ?8 ?9 ?0))
  (set-face-attribute
   'aw-mode-line-face nil
   :inherit 'mode-line-buffer-id
   :foreground "chartreuse"))

(use-package dired-subtree
  :ensure t
  :init
  (setq dired-subtree-use-backgrounds nil)
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             (";" . dired-subtree-remove)
             ("l" . dired-find-file)
             ("h" . dired-up-directory)
             ("j" . dired-next-line)
             ("k" . dired-previous-line)))

(provide 'init-enhance)
