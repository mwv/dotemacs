;; PACKAGES

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))
(defvar my-packages '(ac-c-headers
		      ac-geiser
		      ac-octave
		      auctex
		      auto-complete
		      dash
		      ein
		      elpy
		      fic-mode
		      find-file-in-project
		      flx
		      flx-ido
		      fringe-helper
		      fuzzy
		      geiser
		      git-commit-mode
		      git-gutter-fringe
		      highlight-indentation
		      ido-ubiquitous
		      ido-vertical-mode
		      idomenu
		      iedit
		      ignoramus
		      info+
		      lua-mode
		      magit
		      moe-theme
		      multiple-cursors
		      nose
		      pkg-info
		      popup
		      pymacs
		      pyvenv
		      quack
		      rainbow-mode
		      request
		      s
		      smex
		      virtualenv
		      websocket
		      yasnippet
		      zenburn-theme))
(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(setq inhibit-splash-screen t)
(menu-bar-mode 0)
(if window-system
    (scroll-bar-mode 0))
(mouse-avoidance-mode t)
(if window-system
    (tool-bar-mode -1))

;; (server-start)

;; WINDOWS & FRAMES

(defvar frame-params-file
  (locate-user-emacs-file ".frameparams")
  "File to save frame parameters to on exit")

(defconst frame-params-to-save
  '(left top width height maximized fullscreen)
  "Frame parameters to save and restore")

(defun save-frame-parameters ()
  "Save frame parameters of the selected frame.

Save selected parameters (see `frame-params-to-save')
to `frame-params-file'."
  (when (display-graphic-p)
    (condition-case nil
	(let ((params (--filter (memq (car it) frame-params-to-save)
				(frame-parameters))))
	  (when (and params (display-graphic-p))
	    (with-temp-file frame-params-file
	      (prin1 params (current-buffer))
	      (terpri (current-buffer)))
	    t))
      (file-error nil))))

(defun restore-frame-parameters ()
  "Restore the frame parameters of the selected frame.

Restores selected parameters (see `frame-params-to-save')
from `frame-params-file'."
  (when (display-graphic-p)
    (condition-case nil
	(-when-let*  ((read-params
		       (with-temp-buffer
			 (insert-file-contents frame-params-file)
			 (goto-char (point-min))
			 (read (current-buffer))))
		      (allowed-params
		       (--filter (memq (car it) frame-params-to-save)
				 read-params)))
	  (setq initial-frame-alist
		(append (--filter (assq (car it) allowed-params)
				  initial-frame-alist)
			allowed-params nil)))
      (error-nil))))

(unless noninteractive
  (add-hook 'kill-emacs-hook 'save-frame-parameters)
  (add-hook 'after-init-hook 'restore-frame-parameters))

(require 'dash)

(require 'saveplace)
(setq-default save-place t)

;; BYTE-COMPILE .emacs.d/

(defun remove-elc-on-save ()
  "Remove .elc file after saves."
  (add-hook 'after-save-hook
	    (lambda ()
	      (if (file-exists-p (concat buffer-file-name "c"))
		  (delete-file (concat buffer-file-name "c"))))
	    nil
	    t))

(add-hook 'emacs-lisp-mode-hook 'remove-elc-on-save)

(defun byte-compile-init-dir ()
  "Byte-recompile all files in .emacs.d."
  (interactive)
  (byte-recompile-directory user-emacs-directory 0))

;; (byte-compile-init-dir)

;; FONTS

(defconst preferred-monospace-fonts
  `(
    ("DejaVu Sans Mono" . 100)
    ;; ("Monospace" . 110)
    ;; ("Inconsolata" . 120)
    ;; ("Anonymous Pro" . 110)
    )
  "Preferred monospace fonts")

(defun font-existsp (font)
  "Check if font exists"
  (if (member font (font-family-list))
      t
    nil))

(defun first-existing-font (fonts)
  "Get the first existing font from FONTS."
  (--first (font-existsp (car it)) fonts))

(defun choose-best-fonts ()
  "Choose the best fonts."
  (interactive)
  (-when-let (font (first-existing-font preferred-monospace-fonts))
    (--each '(default fixed-pitch)
      (set-face-attribute it nil :family (car font) :height (cdr font)))))

(choose-best-fonts)


;; COLORTHEME

;; (load-theme 'zenburn :no-confirm)

(load-theme 'moe-dark t)
(set-face-foreground 'show-paren-match "black")
(set-face-background 'show-paren-match "green")
(set-face-foreground 'show-paren-mismatch "black")
(set-face-background 'show-paren-mismatch "red")

(require 'fill-column-indicator)
(setq fci-rule-column 79)
(setq fci-rule-color "gray40")
(setq fci-rule-width 1)
(define-globalized-minor-mode global-fci-mode fci-mode
  (lambda () (fci-mode 1)))
(global-fci-mode 1)

(set-face-attribute 'fringe nil
		    :background "gray20")


(require 'highlight-indentation)
(set-face-background 'highlight-indentation-face "#3f3f3f")

;; MINIBUFFER HISTORY

(require 'savehist)
(setq savehist-save-minibuffer-history t
      savehist-autosave-interval 180)
(savehist-mode t)

;; ORG MODE
(require 'org)
(setq org-directory (expand-file-name "~/Dropbox/Org")
      org-agenda-files (list (expand-file-name "work.org" org-directory)
			     (expand-file-name "thesis.org" org-directory)
			     (expand-file-name "home.org" org-directory))
      org-default-notes-file (expand-file-name "notes.org" org-directory)
      org-completion-use-ido t
      org-yank-adjusted-subtrees t)
(make-directory org-directory :with-parents)

(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(require 'org-mobile)
(setq org-mobile-directory (expand-file-name "~/Dropbox/Org/Mobile")
      org-mobile-inbox-for-pull
      (expand-file-name "from-mobile.org" org-directory))
(make-directory org-mobile-directory :with-parents)

;; (require 'org-latex)
;; (unless (boundp 'org-export-latex-classes)
;;   (setq org-export-latex-classes nil))
;; (add-to-list 'org-export-latex-classes
;;           '("article"
;;             "\\documentclass{article}"
;;             ("\\section{%s}" . "\\section*{%s}")))
;; (setq org-latex-pdf-process
;;       (quote ("texi2dvi --pdf --verbose --batch %f"
;;            "bibtex %b"
;;            "texi2dvi --pdf --clean --verbose --batch %f"
;;            "texi2dvi --pdf --clean --verbose --batch %f")))

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c C") 'org-capture)


;; RECENT FILES
(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode t)
(defun ido-find-recentf ()
  "Find a recent file with IDO."
  (interactive)
  (let ((file (ido-completing-read "Find recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(global-set-key (kbd "C-c f r") 'ido-find-recentf)


;; YASNIPPET

(require 'yasnippet)
(yas-global-mode 1)
(setq yas-snippet-dirs '("~/.emacs.d/snippets/"))


;; PROJECTILE
(require 'projectile)
(add-hook 'python-mode-hook 'projectile-on)
(global-set-key (kbd "C-c p f") 'projectile-find-file)
(global-set-key (kbd "C-c p d") 'projectile-find-dir)
(global-set-key (kbd "C-c p t") 'projectile-toggle-between-implementation-and-test)
(global-set-key (kbd "C-c p T") 'projectile-find-test-file)
(global-set-key (kbd "C-c p g") 'projectile-grep)
(global-set-key (kbd "C-c p o") 'projectile-multi-occur)
(global-set-key (kbd "C-c p p") 'projectile-test-project)
(global-set-key (kbd "C-c p R") 'projectile-regenerate-tags)
(global-set-key (kbd "C-c p c") 'projectile-compile-project)

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

(require 'git-gutter-fringe)
(set-face-foreground 'git-gutter-fr:modified "cyan")
(set-face-background 'git-gutter-fr:modified "gray20")
(set-face-foreground 'git-gutter-fr:added    "green")
(set-face-background 'git-gutter-fr:added    "gray20")
(set-face-foreground 'git-gutter-fr:deleted  "red")
(set-face-background 'git-gutter-fr:deleted  "gray20")
(setq git-gutter-fr:side 'right-fringe)
(global-git-gutter-mode)

(global-set-key (kbd "C-x g") 'magit-status)


;; PYTHON

(elpy-enable)
(elpy-use-ipython)
(elpy-clean-modeline)
;; (elpy-set-backend "jedi")

;; HASKELL
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)


;; AUTOCOMPLETE

(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(defun ac-cc-mode-init ()
  (require 'ac-c-headers)
  (setq 'ac-sources (append '(ac-source-c-headers
			      ac-source-c-header-symbols)
			    ac-sources))
  (add-to-list 'cc-search-directories
	       '"/usr/lib/gcc/x86_64-linux-gnu/4.8/include"))
(add-hook 'c-mode-hook 'ac-cc-mode-init)

(defun ac-emacs-lisp-mode-setup ()
  (setq ac-sources '(ac-source-features
		     ac-source-functions
		     ac-source-variables
		     ac-source-symbols
		     ac-source-abbrev
		     ac-source-dictionary
		     ac-source-words-in-same-mode-buffers)))
(global-auto-complete-mode)

(require 'ein)
(setq ein:use-auto-complete t
      ein:use-smartrep nil)

;; FILE BINDINGS
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))

;; put all backups in same folder
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))

;; Ignore uninteresting files
(ignoramus-setup)

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


(winner-mode)

(windmove-default-keybindings) ;; Shift + Arrow to move between windows


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

(defun toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

(global-set-key [f11] 'toggle-fullscreen)

(defun kill-other-buffers ()
  "Kill all buffers except for the current one.
  Leaves special buffers in place."
  (interactive)
  (dolist (buffer (buffer-list))
    (unless (or (eql buffer (current-buffer))
		(not (buffer-file-name buffer)))
      (kill-buffer buffer))))

(global-set-key (kbd "C-c k") 'kill-other-buffers)

(global-set-key (kbd "C-M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-M-<up>") 'enlarge-window)
(global-set-key (kbd "C-M-<down>") 'shrink-window)

;; ignore case when searching
(setq case-fold-search t)

;; WHITESPACE & TABS

(defun untabify-buffer ()
  "Remove all tabs from the current buffer."
  (interactive)
  (untabify (point-min) (point-max)))

;; always use spaces, never tabs for indentation
;; (setq-default indent-tabs-mode nil)

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
(add-hook 'snippet-mode-hook 'indent-tabs-mode)

;; SPEEDBAR

;; (require 'sr-speedbar)
;; (setq speedbar-hide-button-brackets-flag t
;;       speedbar-show-unknown-files t
;;       speedbar-smart-directory-expand-flag t
;;       speedbar-smart-directory-expand-flag 'trim
;;       speedbar-use-images nil
;;       speedbar-indentation-width 2
;;       speedbar-use-imenu-flag t
;;       ;; speedbar-file-unshown-regexp "flycheck-.*"
;;       speedbar-file-unshown-regexp "*.pyc"
;;       sr-speedbar-width 30
;;       sr-speedbar-width-x 30
;;       sr-speedbar-auto-refresh nil
;;       sr-speedbar-skip-other-window-p t
;;       sr-speedbar-right-side nil)
;; (add-hook 'speedbar-reconfigure-keymaps-hook
;;        '(lambda ()
;;           (define-key speedbar-mode-map [C-up] 'speedbar-up-directory)
;;           (define-key speedbar-mode-map [right] 'speedbar-flush-expand-line)
;;           (define-key speedbar-mode-map [left] 'speedbar-contract-line)))
;; (add-hook 'speedbar-mode-hook '(lambda () (hl-line-mode 1)))

;; (make-face 'speedbar-face)
;; (if window-system
;;     (set-face-font 'speedbar-face "Monospace")
;;     (set-face-attribute 'speedbar-face nil :height 50))
;; (setq speedbar-mode-hook '(lambda () (buffer-face-set 'speedbar-face)))

;; (global-set-key (kbd "C-x t") 'sr-speedbar-toggle)


;; fic-mode
(require 'fic-mode)
(add-hook 'prog-mode-hook 'turn-on-fic-mode)


;; IDO

(require 'ido)

;; disable ido faces to see flx highlights.
(setq ido-use-faces nil)
(setq ido-use-filename-at-point 'guess
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-default-file-method 'selected-window)
(ido-mode t)
(require 'flx-ido)
;; (ido-mode 1)
(ido-everywhere nil)
(flx-ido-mode t)


(require 'ido-ubiquitous)

(ido-ubiquitous-mode t)
(ido-vertical-mode t)

(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

(setq smex-save-file (locate-user-emacs-file ".smex-items"))
(defun smex-update-after-load (unused)
  (when (boundp 'smex-cache)
    (smex-update)))
(add-hook 'after-load-functions 'smex-update-after-load)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")


;; WHICH-FUNCTION-MODE
;; Show the current function name in the header line
(which-function-mode)
;; set which-func to header line instead of mode-line
(setq-default header-line-format
	      '((which-func-mode ("" which-func-format " "))))
(setq mode-line-misc-info
      (assq-delete-all 'which-func-mode mode-line-misc-info))

(set-face-attribute 'header-line nil
		    :foreground "gray90" :background "gray20"
		    :inverse-video nil
		    :box '(:line-width 1 :color "gray30" :style nil)
		    :family "DejaVu Sans Mono"
		    :height 100)
(set-face-background 'which-func "gray20")

;;
;; C-MODE
;;

(require 'compile)

;; make compile shut up
(setq compilation-ask-about-save nil)
;; don't save anything
(setq compilation-save-buffers-predicate '(lambda () nil))
;; some sane defaults for compilation in case there's no makefile
(add-hook 'cc-mode-hook
	  (lambda ()
	    (unless (file-exists-p "Makefile")
	      (set (make-local-variable 'compile-command)
		   ;; emulate make's .c.o implicit pattern rule,
		   ;; but with different defaults for the CC, CPPFLAGS
		   ;; and CFLAGS
		   ;; variables:
		   ;; $(CC) -c -
		   (let ((file (file-name-nondirectory buffer-file-name)))
		     (format "%s -c -o %s.o %s %s %s"
			     (or (getenv "CC") "gcc")
			     (file-name-sans-extension file)
			     (or (getenv "CPPFLAGS") "-DDEBUG=9")
			     (or (getenv "CFLAGS") "-std=gnu99 -pedantic -Wall -g")
			     file))))))

(require 'eldoc)
(require 'etags)

;; make function-synopsis a new thing for thing-at-point
(put 'function-synopsis 'beginning-op
     (lambda ()
       (if (bolp) (forward-line -1) (beginning-of-line))
       (skip-chars-forward "^{")
       (dotimes (i 3) (backward-sexp))))
(put 'function-synopsis 'end-op
     (lambda () (skip-chars-forward "^{")))

;; override eldoc-mode's doc- printer
(defadvice eldoc-print-current-symbol-info
  (around eldoc-show-c-tag activate)
  (if (eq major-mode 'c-mode)
      (show-tag-in-minibuffer)
    ad-do-it))
(defun cleanup-function-synopsis (f)
  ;; nuke newlines
  (setq f (replace-regexp-in-string "\n" " " f))
  ;; nuke comments
  (setq f (replace-regexp-in-string "/\\*.*?\\*/" " " f))
  ;; just on space
  (setq f (replace-regexp-in-string "[ \t]+" " " f))
  f)

;; fetch a tag, jump to it, grab what looks like a function synopsis,
;; and output it in the minibuffer
(defun show-tag-in-minibuffer ()
  (when tags-table-list
    (save-excursion
      ;; shadow some etags globals so they won't be modified
      (let ((tags-location-ring (make-ring find-tag-marker-ring-length))
	    (find-tag-marker-ring (make-ring find-tag-marker-ring-length))
	    (last-tag nil))
	(let* ((tag (funcall
		     (or find-tag-default-function
			 (get major-mode 'find-tag-default-function)
			 'find-tag-default)))
	       (tag-regex (format "\\(^\\|[ \t\n*]\\)%s\\($\\|(\\)"
				  (regexp-quote tag))))
	  (set-buffer (find-tag-noselect tag-regex nil t))
	  (let ((synopsis (or (thing-at-point 'function-synopsis)
			      (thing-at-point 'line))))
	    (when synopsis
	      (eldoc-message "%s"
			     (cleanup-function-synopsis)))))))))

(add-hook 'c-mode 'turn-on-eldoc-mode)


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

;; AUCTEX
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;;
;; APPEARANCE
;;

(setq ring-bell-function 'ignore)

(visual-line-mode 1)
(require 'fringe)
(fringe-mode 5)
(setq overflow-newline-into-fringe nil)


;; LINE NUMBERS & LINUM

(require 'linum)
(defcustom linum-disabled-modes-list
  '(eshell-mode wl-summary-mode compilation-mode dired-mode speedbar-mode)
  "List of modes that have linum disabled when global linum mode is on"
  :type '(repeat (sexp :tag "Major mode"))
  :tag " Major modes that have linum disabled: "
  :group 'linum
)
(defcustom linum-disable-starred-buffers 't
  "Disable linum in starred buffers"
  :type 'boolean
  :group 'linum)
(defun linum-on ()
  "When linum is running globally, disable line numbering in modes defined in linum-disabled-modes-list"
  (unless (or (minibufferp) (member major-mode linum-disabled-modes-list)
	      (and linum-disable-starred-buffers (string-match "*" (buffer-name))))
    (linum-mode 1)))
(set-face-attribute 'linum nil
		    :background "gray20" :foreground "gray40")
(global-linum-mode t)

;; line wrap
(setq line-move-visual t)

;; mark transient
(transient-mark-mode t)

;; text decoration
(require 'font-lock)
(setq font-lock-maximum-decoration t)
(global-font-lock-mode t)
(global-hi-lock-mode nil)
(setq jit-lock-contextually t)
(setq jit-lock-stealth-verbose t)

;; globally enable syntax highlighting
(global-font-lock-mode t)

;; highlight parentheses
(require 'paren)
(show-paren-mode t)


;; MODELINE

(setq-default
  mode-line-format
  '(;; position
    (:propertize "%4l:" face mode-line-position-face)
    (:eval (propertize "%3c" 'face
		       (if (>= (current-column) 80)
			   'mode-line-80col-face
			 'mode-line-position-face)))
    " "
    (:eval
     (cond (buffer-read-only
	    (propertize " RO " 'face 'mode-line-read-only-face))
	   ((buffer-modified-p)
	    (propertize " ** " 'face 'mode-line-modified-face))
	   (t " -- ")))
    " "
    ;; directory and buffer name
    (:propertize (:eval (shorten-directory default-directory 15))
		 face mode-line-folder-face)
    (:propertize "%b"
		 face mode-line-filename-face)
    ;; narrow
    " %n "
    ;; mode indicators: vc, recursive edit, major mode, minor modes, process, global
    (vc-mode vc-mode)
    "  %["
    (:propertize mode-name
		 face mode-line-mode-face)
    "%] "
    "    "))

(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
	(output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)

(set-face-attribute 'mode-line nil
    :foreground "gray90" :background "gray30"
    :inverse-video nil
    :box '(:line-width 1 :color "gray30" :style nil)
    :family "DejaVu Sans Mono"
    :height 100)
(set-face-attribute 'mode-line-inactive nil
    :foreground "gray50" :background "gray20"
    :inverse-video nil
    :box '(:line-width 1 :color "gray20" :style nil)
    :family "DejaVu Sans Mono"
    :height 100)
(set-face-attribute 'mode-line-read-only-face nil
    :inherit 'mode-line-face
    :foreground "#4271ae"
    :box '(:line-width 1 :color "#4271ae"))
(set-face-attribute 'mode-line-modified-face nil
    :inherit 'mode-line-face
    :foreground "#c82829"
    :background "#ffffff"
    :box '(:line-width 1 :color "#c82829"))
(set-face-attribute 'mode-line-folder-face nil
    :inherit 'mode-line-face
    :foreground "gray60")
(set-face-attribute 'mode-line-filename-face nil
    :inherit 'mode-line-face
    :foreground "#eab700"
    :weight 'bold)
(set-face-attribute 'mode-line-position-face nil
    :inherit 'mode-line-face
    ;; :family "Menlo"
    :family "DejaVu Sans Mono"
    :height 100)
(set-face-attribute 'mode-line-mode-face nil
    :inherit 'mode-line-face
    :foreground "gray80")
(set-face-attribute 'mode-line-minor-mode-face nil
    :inherit 'mode-line-mode-face
    :foreground "gray40"
    :height 100)
(set-face-attribute 'mode-line-process-face nil
    :inherit 'mode-line-face
    :foreground "#718c00")
(set-face-attribute 'mode-line-80col-face nil
    :inherit 'mode-line-position-face
    :foreground "black" :background "#eab700")

;; EXTRA KEYBINDINGS

(defun kill-this-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x C-k") 'kill-this-buffer)
(global-set-key (kbd "C-x |") 'align)

(defun start-shell ()
  (interactive)
  (ansi-term "/usr/bin/zsh"))

(global-set-key [f1] 'start-shell)
(global-set-key [f2] 'rgrep)
(global-set-key [f5] 'goto-line)
(global-set-key (kbd "M-^") 'top-join-line)
(global-set-key (kbd "C-a") 'smarter-move-beginning-of-line)
(define-key global-map (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-c C-i") 'yas-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-o") 'other-window)


;;
;; FUNCTIONS
;;


(defadvice zap-to-char (after my-zap-to-char-advice (arg char) activate)
    "Kill up to the ARG'th occurence of CHAR, and leave CHAR. If
  you are deleting forward, the CHAR is replaced and the point is
  put before CHAR"
    (insert char)
    (if (< 0 arg) (forward-char -1)))

(defalias 'yes-or-no-p 'y-or-n-p)

(defun top-join-line ()
  "Join the current line with the line beneath it."
  (interactive)
  (delete-indentation 1))

(defun set-newline-and-indent ()
  (local-set-key (kbd "RET") 'newline-and-indent))

(defun yank-and-indent ()
  "Yank and then indent the newly formed region according to mode."
  (interactive)
  (yank)
  (call-interactively 'indent-region))

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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("11d069fbfb0510e2b32a5787e26b762898c7e480364cbc0779fe841662e4cf5d" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
