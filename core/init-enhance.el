(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package nerd-icons-completion
  :ensure t
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode))

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides
   '((file (styles basic partial-completion)))))

(use-package vertico
  :ensure t
  :hook ((after-init . vertico-mode)
         (minibuffer-setup . vertico-repeat-save))
  :custom
  (vertico-resize nil)
  (vertico-cycle nil))

(use-package consult
  :ensure t
  :after vertico
  :bind (("C-x b" . consult-buffer)
         ("<f8> b" . consult-project-buffer)
         ("M-y" . consult-yank-pop))
  :hook
  (completion-list-mode . consult-preview-at-point-mode)
  :config
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (setq consult-preview-key "M-.")
  (setq consult-ripgrep-args
        "rg --null --line-buffered -M=1000 --path-separator / -S --no-heading -H -n -g \"!{README,readme}.{md,org}\" -g \"!go.sum\" -g \"!*.svg\""))

(use-package rg
  :ensure t
  :defer t)

(use-package embark
  :ensure t
  :bind (("<f8> ." . embark-act)
         ("<f8> ;" . embark-dwim))
  :custom
  (prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :ensure t
  :after embark consult
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window))
  :custom
  (aw-keys '(?1 ?a ?w ?x ?7 ?8 ?9 ?0))
  :config
  (custom-set-faces
   '(aw-leading-char-face
     ((t (:foreground "red" :inherit ace-jump-face-foreground :height 1.4))))))

(use-package dired-subtree
  :ensure t
  :bind ("<f8> d" . dired-jump)
  :custom
  (dired-subtree-use-backgrounds nil)
  :config
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert)
             (";" . dired-subtree-remove)
             ("l" . dired-find-file)
             ("h" . dired-up-directory)
             ("j" . dired-next-line)
             ("k" . dired-previous-line)))

(provide 'init-enhance)
