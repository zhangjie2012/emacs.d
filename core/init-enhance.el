(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package nerd-icons-completion
  :ensure t
  :config
  (nerd-icons-completion-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

(use-package vertico
  :ensure t
  :hook ((after-init . vertico-mode)
         (minibuffer-setup . vertico-repeat-save))
  :init
  (setq vertico-resize nil
        vertico-cycle nil))

(use-package consult
  :after vertico
  :ensure t
  :bind (("C-x b" . consult-buffer)
         ("M-y" . consult-yank-pop))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  :config
  ;; filter go.sum/readme.* files
  (setq consult-ripgrep-args "rg --null --line-buffered -M=1000 --path-separator / -S --no-heading -H -n -g \"!{README,readme}.{md,org}\" -g \"!go.sum\"")
  (setq consult-preview-key "M-."))

(use-package rg
  :ensure t)

(use-package embark
  :ensure t
  :bind (("C-." . embark-act))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after embark consult
  :ensure t
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-history))

(use-package corfu
  :ensure t
  :hook (prog-mode . corfu-mode)
  :bind (:map corfu-map
              ("C-n" . corfu-next)
              ("C-p" . corfu-previous))
  :config
  (setq corfu-auto t
        corfu-auto-prefix 1
        corfu-auto-delay 0.1
        corfu-quit-no-match t
        corfu-quit-at-boundary t)
  (add-hook 'multiple-cursors-mode-enabled-hook (lambda () (corfu-mode -1)))
  (add-hook 'multiple-cursors-mode-disabled-hook (lambda () (corfu-mode 1))))

(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window))
  :config
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:foreground "red" :inherit ace-jump-face-foreground :height 1.4)))))
  (setq aw-keys '(?1 ?a ?w ?x ?7 ?8 ?9 ?0)))

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
