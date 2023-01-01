(require 'package)

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
		("melpa" . "https://melpa.org/packages/")
		("melpa-stable" . "https://stable.melpa.org/packages/")
		("nongnu" . "https://elpa.nongnu.org/nongnu/")
		))

(setq url-proxy-services
      '(("no_proxy" . "^\\(localhost\\|10.*\\)")
        ("http" . "127.0.0.1:1087")
        ("https" . "127.0.0.1:1087")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-verbose t)
  (setq use-package-always-pin t)
  (setq use-package-compute-statistics t)
  (setq use-package-hook-name-suffix nil)
  )

(eval-when-compile
  (require 'use-package))

(add-to-list 'load-path "~/.emacs.d/core/")

(require 'init-base)
(require 'init-ui)
(require 'init-modernization)
(require 'init-document)
(require 'init-config)
;; coding IDE
(require 'init-ide-base)
(require 'init-ide-go)
(require 'init-ide-web)
(require 'init-ide-python)
;; rss feed
(require 'init-feed)

(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))
