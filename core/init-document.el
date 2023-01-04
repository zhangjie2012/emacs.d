(use-package markdown-mode
  :pin melpa-stable
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
		 ("\\.md\\'" . markdown-mode)
		 ("\\.markdown\\'" . markdown-mode))
  )

(use-package htmlize
  :pin melpa
  :ensure t
  :defer t
  )

;; https://emacs.stackexchange.com/questions/17710/use-package-with-config-to-set-variables
(use-package org
  :pin nongnu
  :ensure org-contrib
  :bind (("<f9> t l" . org-toggle-link-display)
		 ("<f9> t f" . org-footnote-new)
		 ("<f9> t i" . org-toggle-inline-images)
		 ("<f9> t a" . org-agenda)
		 ("<f9> t t" . org-capture)
		 ("<f9> t m" . org-show-todo-tree)
		 ("<f9> t h" . org-show-all)
		 ;; ("M-[". org-previous-visible-heading)
		 ;; ("M-]". org-next-visible-heading)
		 )
  :init
  (setq org-adapt-indentation nil
		org-log-done t
		org-src-tab-acts-natively nil
		org-pretty-entities t
		;; org-hide-emphasis-markers t
		org-startup-folded t
		org-startup-with-inline-images t
		org-image-actual-width '(1024))

  ;; GTD setting
  (require 'org-inlinetask)
  (setq org-todo-keywords
		'((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)" "CANCELED(d)")))
  (setq org-todo-keyword-faces
		'(("TODO" . "coral")
          ("WAIT" . "burlywood")
          ("DONE" . "darkcyan")
		  ("CANCELED" . "darkgrey")))
  ;; agenda
  (setq org-agenda-files '("~/personal-area/todo.org"))
  (setq org-agenda-skip-scheduled-if-done t
		org-agenda-skip-deadline-if-done t
		org-agenda-include-deadlines t
		org-agenda-include-diary nil
		org-agenda-block-separator nil
		org-agenda-compact-blocks t
		org-agenda-start-with-log-mode t)
  (setq org-agenda-breadcrumbs-separator " ❱ "
		org-agenda-current-time-string "⏰ ┈┈┈┈┈┈┈┈┈┈┈ now"
		org-agenda-time-grid '((weekly today require-timed)
                               (800 1000 1200 1400 1600 1800 2000)
                               "---" "┈┈┈┈┈┈┈┈┈┈┈┈┈")
		org-agenda-prefix-format '((agenda . "%i %-12:c%?-12t%b% s")
                                   (todo . " %i %-12:c")
                                   (tags . " %i %-12:c")
                                   (search . " %i %-12:c")))
  (setq org-agenda-format-date (lambda (date) (concat "\n" (make-string (window-width) 9472)
                                                      "\n"
                                                      (org-agenda-format-date-aligned date))))
  (setq org-cycle-separator-lines 2)

  :config
  (require 'org-tempo)

  (use-package ob-go
	:pin melpa
	:ensure t
	)

  ;; programming languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
	 (emacs-lisp . t)
	 (css . t)
	 (js . t)
	 (org . t)
	 (python . t)
	 (sed . t)
	 (sql . t)
	 (R . t)
	 (go . t)
	 )
   )

  (use-package org-superstar
	:pin melpa
	:ensure t
	:init
	(setq org-superstar-configure-like-org-bullets t
		  org-superstar-special-todo-items nil
		  )
	(add-hook 'org-mode-hook (lambda () (org-superstar-mode 1)))
	)

  (use-package toc-org
	:pin melpa
	:ensure t
	:init
	(add-hook 'org-mode-hook 'toc-org-mode)
	)
  )

;; blog
(use-package ox-publish
  :pin melpa
  :defer t
  :init
  (setq org-html-validation-link nil)

  ;; nil: do not checking and always publish all file
  ;; Non-nil(t): use timestamp checking, default set 't'
  (setq org-publish-use-timestamps-flag t)

  (setq org-html-postamble t
		org-html-postamble-format
		'(("en" "<p class=\"postamble\">First created: %d <br />Last updated: %C <br />Power by %c</p>")))

  (setq org-publish-project-alist
		'(
		  ;; notes component
		  ("site-orgs"
		   :base-directory "~/site/org"
		   :base-extension "org"
		   :html-link-home "index.html"
		   :publishing-directory "~/site-html"
		   :recursive t
		   :publishing-function org-html-publish-to-html
		   :headline-levels 5
		   :auto-sitemap t
		   :sitemap-filename "sitemap.org"
		   :sitemap-title "Sitemap"
		   )
		  ;; static component
		  ("site-static"
		   :base-directory "~/site/static"
		   :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
		   :publishing-directory "~/site-html/static/"
		   :recursive t
		   :publishing-function org-publish-attachment
		   )
		  ;; publish component
		  ("site" :components ("site-orgs" "site-static"))
		  ))
  )

(provide 'init-document)
