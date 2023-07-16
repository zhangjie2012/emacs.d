(defvar current-date-time-format "%Y-%m-%d %H:%M:%S"
  "Format of date to insert with `insert-current-date-time' func
See help of `format-time-string' for possible replacements")

(defun insert-current-date-time ()
  "insert the current date and time into current buffer.
Uses `current-date-time-format' for the formatting the date/time."
  (interactive)
  (insert (format-time-string current-date-time-format (current-time)))
  )
(defun toggle-frame-alpha ()
  (interactive)
  (let* ((pair (or (frame-parameter nil 'alpha) '(100 100)))
         (alpha (apply '+ pair)))
    (set-frame-parameter nil
                         'alpha
                         (if (or (null alpha) (eq alpha 200) (eq alpha 2.0))
                             '(80 60) '(100 100)))))
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(use-package emacs
  :ensure nil
  :init
  (defun match-paren (arg)
    "Go to the matching paren if on a paren; otherwise insert %."
    (interactive "p")
    (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
	      ((looking-at "\\s)") (forward-char 1) (backward-list 1))
	      (t (self-insert-command (or arg 1)))))
  (global-set-key (kbd "C-2") 'set-mark-command) ;; actual is C-@
  (global-set-key (kbd "C-x k") 'kill-this-buffer) ;; kill-this-buffer replace kill-buffer
  (global-set-key (kbd "M-*") 'match-paren)
  (global-set-key (kbd "S-<backspace>") 'kill-whole-line))

(use-package hydra
  :ensure t
  :config
  (defhydra hydra-default
    (:hint nil :idle 1)
    ("f" find-file "find file" :exit t :column "1. common")
    ("b" switch-to-buffer "switch buffer" :exit t)
    ("'" show-file-name "file name" :exit t)
    ("n" display-line-numbers-mode "absolute line number")
    ("N" linum-relative-toggle "relative line number")
    ("i" insert-current-date-time "insert date time")
    ("w" save-buffer "save buffer" :exit t)
    ("o" consult-outline "outline" :exit t)
    ("s" consult-line "search buffer" :exit t)
    ("e" consult-ripgrep "ripgrep project" :exit t)
    ("E" rg "repgrep dir" :exit t)

    ("1" delete-other-windows "delete other" :exit t :column "2. window management")
    ("2" split-window-below "split below" :exit t )
    ("3" split-window-horizontally "split horizontally" :exit t)
    ("x" toggle-frame-fullscreen "toggle fullscreen" :exit t)
    ("X" toggle-frame-alpha "toggle alpha" :exit t)

    ("g" git-gutter+-mode "git gutter" :exit t :column "3. coding")
    ("r" git-gutter+-revert-hunks "revert hucks")
    ("j" git-gutter+-next-hunk "next huck")
    ("k" git-gutter+-previous-hunk "previous huck")
    ("m" consult-imenu "imenu" :exit t)
    ("M" consult-imenu-menu "imenu menu" :exit t)

    (".1" (find-file "~/gtd/entry.org") "gtd/entry.org" :exit t :column "4. open")
    (".2" (find-file "~/.emacs.d/init.el") "emacs.d/init.el" :exit t)
    (".3" (find-file "~/personal-area/self-growth/index.org") "self-growth/index.org" :exit t)

    ("ti" display-fill-column-indicator-mode "toggle column indicator" :column "x. other")

    ("<SPC>" nil "quit" :column nil))
  (global-set-key (kbd "<f9>") 'hydra-default/body)

  ;; just move cursor, no copy/edit
  (defhydra hydra-key-fly
    (:hint nil :idle 1)
    ;; control
    ("n" next-line "next line")
    ("p" previous-line "prev line")
    ("b" backward-char "backward char")
    ("f" forward-char "forward char")
    ("a" move-beginning-of-line "begin line")
    ("e" move-end-of-line "end line")
    ("v" scroll-up-command "scroll up")
    ("h" recenter-top-bottom "recenter")

    ;; meta
    ("A" backward-sentence "backward sentence")
    ("F" forward-word "foward word")
    ("B" backward-word "backward word")
    ("E" forward-sentence "forward sentence")
    ("V" scroll-down-command "scroll down")
    ("," beginning-of-buffer "begin buf")
    ("." end-of-buffer "end buf")

    ("<SPC>" nil :column nil))
  (global-set-key (kbd "<escape>") 'hydra-key-fly/body))

(provide 'init-keymap)
