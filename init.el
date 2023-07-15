(require 'package)

(setq package-archives
      '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
        ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
        ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-compute-statistics t))

(eval-when-compile
  (require 'use-package))

(add-to-list 'load-path "~/.emacs.d/core/")

(require 'init-base)
(require 'init-ui)
(require 'init-modern)
(require 'init-enhance)
(require 'init-document)
(require 'init-lang)
(require 'init-config)
(require 'init-feed)
(require 'init-keymap)

(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))
