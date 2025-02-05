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
                             '(85 60) '(100 100)))))
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(defun match-paren (arg)
    "Go to the matching paren if on a paren; otherwise insert %."
    (interactive "p")
    (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
	      ((looking-at "\\s)") (forward-char 1) (backward-list 1))
	      (t (self-insert-command (or arg 1)))))

(use-package emacs
  :ensure nil
  :init
  (global-set-key (kbd "C-2") 'set-mark-command) ;; actual is C-@
  (global-set-key (kbd "C-x k") 'kill-this-buffer) ;; kill-this-buffer replace kill-buffer
  (global-set-key (kbd "M-*") 'match-paren)
  (global-set-key (kbd "S-<backspace>") 'kill-whole-line))

(use-package hydra
  :ensure t
  :config
  (defhydra hydra-default
    (:hint nil :idle 1)

    ("r" replace-string "replace string" :exit t :column "1. edit")
    ("w" save-buffer "save buffer" :exit t)
    ("o" consult-outline "outline" :exit t)
    ("s" consult-line "search buffer" :exit t)
    ("e" consult-ripgrep "ripgrep project" :exit t)
    ("i" insert-current-date-time "insert date time" :exit t)
    ("l" align-regexp "align text" :exit t)

    ("f" find-file "find file" :exit t :column "2. buffer")
    ("b" switch-to-buffer "switch buffer" :exit t)
    ("'" show-file-name "file name" :exit t)
    ("n" display-line-numbers-mode "absolute line number")
    ("N" linum-relative-toggle "relative line number")
    ("u" revert-buffer "revert buffer" :exit t)
	("j" consult-goto-line "goto line" :exit t)

    ("1" delete-other-windows "delete other" :exit t :column "3. window management")
    ("2" split-window-below "split below" :exit t )
    ("3" split-window-horizontally "split horizontally" :exit t)
    ("x" toggle-frame-fullscreen "toggle fullscreen" :exit t)
    ("X" toggle-frame-alpha "toggle alpha" :exit t)

    ("ti" display-fill-column-indicator-mode "toggle column indicator" :column "x. other")
    ("m" consult-imenu "imenu" :exit t)
    ("M" consult-outline "consult outline" :exit t)
    ("c" eshell "open eshell" :exit t)
    ("q" nil "quit"))
  (global-set-key (kbd "<f9>") 'hydra-default/body))

(provide 'init-keymap)
