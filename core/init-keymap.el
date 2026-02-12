(defconst current-date-time-format "%Y-%m-%d %H:%M:%S"
  "Format used by `insert-current-date-time'.")

(defun insert-current-date-time ()
  "Insert the current date/time using `current-date-time-format'."
  (interactive)
  (insert (format-time-string current-date-time-format)))

(defun toggle-frame-alpha ()
  "Toggle frame transparency between normal and semi-transparent."
  (interactive)
  (let* ((pair (frame-parameter nil 'alpha))
         (current (or (car-safe pair) pair 100)))
    (set-frame-parameter
     nil 'alpha
     (if (>= current 100)
         '(85 . 60)
       '(100 . 100)))))

(defun show-file-name ()
  "Show the full path of the current buffer’s file."
  (interactive)
  (message (or (buffer-file-name) "No file")))

(defun match-paren (arg)
  "Jump to matching paren, otherwise insert the character ARG times."
  (interactive "p")
  (cond
   ((looking-at "\\s(") (forward-list 1) (backward-char))
   ((looking-at "\\s)") (forward-char) (backward-list 1))
   (t (self-insert-command (or arg 1)))))

(use-package emacs
  :ensure nil
  :init
  (global-set-key (kbd "C-2") #'set-mark-command)
  (global-set-key (kbd "C-x k") #'kill-current-buffer)
  (global-set-key (kbd "M-*") #'match-paren)
  (global-set-key (kbd "S-<backspace>") #'kill-whole-line))

(use-package hydra
  :ensure t
  :config
  (defhydra hydra-default
    (:hint nil :idle 1)

    ;; ---- 1. edit ----
    ("r" replace-string "replace string" :exit t :column "1. edit")
    ("w" save-buffer "save buffer" :exit t)
    ("o" consult-outline "outline" :exit t)
    ("s" consult-line "search buffer" :exit t)
    ("e" consult-ripgrep "ripgrep project" :exit t)
    ("i" insert-current-date-time "insert date time" :exit t)
    ("l" align-regexp "align text" :exit t)

    ;; ---- 2. buffer ----
    ("f" find-file "find file" :exit t :column "2. buffer")
    ("b" switch-to-buffer "switch buffer" :exit t)
    ("'" show-file-name "file name" :exit t)
    ("n" display-line-numbers-mode "toggle abs numbers")
    ("N" linum-relative-toggle "toggle rel numbers")
    ("u" revert-buffer "revert buffer" :exit t)
    ("j" consult-goto-line "goto line" :exit t)

    ;; ---- 3. window mgmt ----
    ("1" delete-other-windows "delete other" :exit t :column "3. windows")
    ("2" split-window-below "split below" :exit t)
    ("3" split-window-horizontally "split horizontally" :exit t)
    ("x" toggle-frame-fullscreen "fullscreen" :exit t)
    ("X" toggle-frame-alpha "toggle alpha" :exit t)

    ;; ---- x. other ----
    ("ti" display-fill-column-indicator-mode "toggle column indicator" :column "other")
    ("m" consult-imenu "imenu" :exit t)
    ("M" consult-outline "outline" :exit t)
    ("c" eshell "eshell" :exit t)

    ("q" nil "quit"))

  (global-set-key (kbd "<f9>") #'hydra-default/body))

(provide 'init-keymap)
