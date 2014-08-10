(add-to-list 'load-path "~/.emacs.d/")

(require 'package)
(package-initialize)

;; INIT

(setq inhibit-splash-screen t)
(menu-bar-mode 0)

;; FILE BINDINGS
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))


;; GIT & MAGIT

(require 'magit)
(setq magit-save-some-buffers 'dontask
      magit-set-upstream-on-push t)

;; extra newline to separate commit mesage from git commentary
(defun magit-commit-mode-init ()
  (when (looking-at "\n")
    (open-line 1)))
(add-hook 'git-commit-mode-hook 'magit-commit-mode-init)

;; close popup when committing
;; (defadvice git-commit-commit (after delete-window activate)
;;   (delete-window))

;; (require 'git-gutter-fringe)
;; (set-face-foreground 'git-gutter-fr:modified "orange")
;; (set-face-background 'git-gutter-fr:modified "gray20")
;; (set-face-foreground 'git-gutter-fr:added    "green")
;; (set-face-background 'git-gutter-fr:added    "gray20")
;; (set-face-foreground 'git-gutter-fr:deleted  "red")
;; (set-face-background 'git-gutter-fr:deleted  "gray20")
;; (setq git-gutter-fr:side 'right-fringe)
;; (global-git-gutter-mode)

(global-set-key (kbd "C-x g") 'magit-status)

;; put all backups in same folder
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))


;; ignore case when searching
(setq case-fold-search t)

;; WHITESPACE & TABS

(defun untabify-buffer ()
  "Remove all tabs from the current buffer."
  (interactive)
  (untabify (point-min) (point-max)))

(setq require-final-newline t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defadvice whitespace-cleanup
  (around whitespace-cleanup-indent-tab activate)
  "Fix whitespace-cleanup indent-tabs-mode bug"
  (let ((whitespace-indent-tabs-mode indent-tabs-mode)
	(whitespace-tab-width tab-width))
    ad-do-it))
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'python-mode-hook
  '(lambda ()
     (add-hook 'before-save-hook 'untabify-buffer)))
(add-hook 'cc-mode-hook
  '(lambda ()
     (add-hook 'before-save-hook 'untabify-buffer)))
(add-hook 'makefile-gmake-mode-hook 'indent-tabs-mode)


;; NEWLINES

(add-hook 'lisp-mode-hook 'set-newline-and-indent)
(add-hook 'python-mode-hook 'set-newline-and-indent)
(add-hook 'c-mode-hook 'set-newline-and-indent)

(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
	   (and (not current-prefix-arg)
		(member major-mode '(emacs-lisp-mode
				     lisp-mode
				     scheme-mode
				     haskell-mode
				     c-mode
				     c++-mode
				     latex-mode
				     python-mode))
		(let ((mark-even-if-inactive transient-mark-mode))
		  (indent-region (region-beginning (region-end) nil)))))))

;; APPEARANCE
(setq ring-bell-function 'ignore)
(visual-line-mode 1)

;; line wrap
(setq line-move-visual t)

;; mark transient
(transient-mark-mode t)

;; text decoration
(require 'font-lock)
(global-font-lock-mode t)

;; EXTRA KEYBINDINGS

(defun kill-this-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x C-k") 'kill-this-buffer)

(defun start-shell ()
  (interactive)
  (ansi-term "/usr/bin/zsh"))

(global-set-key [f1] 'start-shell)
(global-set-key [f2] 'rgrep)
(global-set-key [f5] 'goto-line)
(global-set-key (kbd "C-a") 'smarter-move-beginning-of-line)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-n") 'end-of-defun)
(global-set-key (kbd "M-p") 'beginning-of-defun)


;; FUNCTIONS

(defadvice zap-to-char (after my-zap-to-char-advice (arg char) activate)
    "Kill up to the ARG'th occurence of CHAR, and leave CHAR. If
  you are deleting forward, the CHAR is replaced and the point is
  put before CHAR"
    (insert char)
    (if (< 0 arg) (forward-char -1)))

(defalias 'yes-or-no-p 'y-or-n-p)

(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

  Move point to the first non-whitespace character on this line.
  If point is already there, move to the beginning of the line.
  Effectively toggle between the first non-whitespace character and
  the beginning of the line.

  If ARG is not nil or 1, move forward ARG - 1 lines first. If
  point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; quickly open init file
(defun find-user-init-file-other-window ()
  "Edit teh `user-init-file' in another window."
  (interactive)
  (find-file-other-window user-init-file))
(global-set-key (kbd "C-c I") 'find-user-init-file-other-window)

(defun rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
	(message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
	(cond
	 ((vc-backend filename) (vc-rename-file filename new-name))
	 (t
	  (rename-file filename new-name t)
	  (set-visited-file-name new-name t t)))))))
(global-set-key (kbd "C-c r") 'rename-file-and-buffer)

(defun swap-windows ()
  "Iff two windows are open, they get swapped."
  (interactive)
  (if (/= (count-windows) 2)
      (message "You need exactly two windows open to do this.")
    (let* ((w1 (car (window-list)))
	   (w2 (cadr (window-list)))
	   (b1 (window-buffer w1))
	   (b2 (window-buffer w2))
	   (s1 (window-start w1))
	   (s2 (window-start w2)))
      (set-window-buffer w1 b2)
      (set-window-buffer w2 b1)
      (set-window-start w1 w2)
      (set-window-start w2 s1)))
  (other-window 1))

(global-set-key (kbd "C-c o") 'swap-windows)


(if (string= "x" window-system)
    (load-library "windowed"))
