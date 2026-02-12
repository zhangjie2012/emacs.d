(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

(use-package nerd-icons-completion
  :ensure t
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode))

(use-package vertico
  :ensure t
  :hook ((after-init . vertico-mode)
         (minibuffer-setup . vertico-repeat-save))
  :custom
  (vertico-resize nil)
  (vertico-cycle nil))

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :bind
  (:map corfu-map
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous)
        ("<tab>" . corfu-insert)
        ("TAB" . corfu-insert))
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.0)
  (corfu-auto-prefix 2)
  (corfu-preview-current nil)
  (corfu-preselect 'prompt)
  (corfu-count 10)
  (corfu-min-width 40)
  (corfu-max-width 100)
  (corfu-scroll-margin 2)
  :config
  (corfu-popupinfo-mode 1)
  (setq corfu-popupinfo-delay '(0.5 . 0.2)) ; 首次延迟 0.5s，连续移动延迟 0.2s
  (setq corfu-exclude-modes '(org-mode markdown-mode eshell-mode thrift-mode)))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)     ; 文件路径
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)  ; 当前 buffer 关键词
  (add-to-list 'completion-at-point-functions #'cape-keyword)) ; 语言关键字

(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

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

(use-package corfu
  :ensure t
  :init
  (global-corfu-mode)
  :bind
  (:map corfu-map
        ("C-n" . corfu-next)
        ("C-p" . corfu-previous))
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 2)
  (corfu-count 8)
  (corfu-min-width 40)
  (corfu-max-width 72)
  :config
  (setq corfu-exclude-modes '(org-mode markdown-mode eshell-mode thrift-mode))
  (corfu-popupinfo-mode 1)
  (setq corfu-popupinfo-delay 0.5))

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

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
