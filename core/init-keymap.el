(use-package hydra
  :ensure t
  :config
  (defhydra hydra-jump-file
    (:hint nil)
    "Open file"
    ("1" (find-file "~/gtd/entry.org") "gtd/entry.org" :exit t)
    ("2" (find-file "~/.emacs.d/init.el") "emacs.d/init.el" :exit t)
    ("3" (find-file "~/personal-area/self-growth/index.org") "self-growth/index.org" :exit t)
    ("q" nil "Quit"))
  (global-set-key (kbd "<f2>") 'hydra-jump-file/body))

(provide 'init-keymap)
