(add-to-list 'load-path "~/.emacs.d/elpa")

(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("melpa" . "http://melpa.milkbox.net/packages/")
			 ("elpy" . "http://jorgenschaefer.github.io/packages/")))
(package-initialize)
(defvar my-packages '(ac-c-headers
		      ac-geiser
		      ac-octave
		      ;; auctex
		      auto-complete
		      ;; cython-mode
		      dash
		      ;; ein
		      elpy
		      fic-mode
		      find-file-in-project
		      fill-column-indicator
		      flx
		      flx-ido
		      fringe-helper
		      fuzzy
		      geiser
		      git-commit-mode
		      git-gutter-fringe
		      highlight-indentation
		      haskell-mode
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
		      projectile
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

;; INIT

(setq inhibit-splash-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(require 'avoid)
(mouse-avoidance-mode 'animate)


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

;; FILE BINDINGS
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

(if (string= "x" window-system)
    (progn
      (require 'moe-theme)
      (moe-dark)
      ;; (load-theme 'moe-dark t)
      (set-face-foreground 'show-paren-match "black")
      (set-face-background 'show-paren-match "green")
      (set-face-foreground 'font-lock-comment-face "#00d7af")
      (set-face-foreground 'font-lock-comment-delimiter-face "#00d7af")
      ;; (set-face-foreground 'iedit-occurrence "black")
      ;; (set-face-background 'iedit-occurrence "yellow")
      ;; (set-face-foreground 'show-paren-mismatch "black")
      ;; (set-face-background 'show-paren-mismatch "red"))
      )
  (load-theme 'darkburn :no-confirm))

;; WINDOWS & FRAMES
(if (string= "x" window-system)
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
)

;; HELM

(require 'helm)
(require 'helm-config)
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))
(helm-mode 1)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
(define-key helm-map (kbd "C-z") 'helm-select-action)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(when (executable-find "ack")
  (setq helm-grep-default-command "ack -Hn --no-group --no-color %e %p %f"
	helm-grep-default-recurse-command "ack -H --no-group --no-color %e %p %f"))

(setq helm-quick-update                     t ; do not display invisible candidates
      helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(semantic-mode 1)

(require 'helm-projectile)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)
(helm-mode 1)

;; fic-mode
(require 'fic-mode)
(add-hook 'prog-mode-hook 'turn-on-fic-mode)

;; WHICH-FUNCTION-MODE
;; Show the current function name in the header line
(which-function-mode)
(set-face-background 'which-func "gray30")


;; GIT & MAGIT

(require 'magit)
(setq magit-save-some-buffers 'dontask
      magit-set-upstream-on-push t)

;; extra newline to separate commit mesage from git commentary
(defun magit-commit-mode-init ()
  (when (looking-at "\n")
    (open-line 1)))
(add-hook 'git-commit-mode-hook 'magit-commit-mode-init)

(require 'git-gutter-fringe)
(set-face-foreground 'git-gutter-fr:modified "orange")
(set-face-background 'git-gutter-fr:modified "gray20")
(set-face-foreground 'git-gutter-fr:added    "green")
(set-face-background 'git-gutter-fr:added    "gray20")
(set-face-foreground 'git-gutter-fr:deleted  "red")
(set-face-background 'git-gutter-fr:deleted  "gray20")
(setq git-gutter-fr:side 'left-fringe)
(global-git-gutter-mode)

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

(add-hook 'rust-mode-hook 'flycheck-mode)


;; NEWLINES

(add-hook 'lisp-mode-hook 'set-newline-and-indent)
(add-hook 'python-mode-hook 'set-newline-and-indent)
(add-hook 'c-mode-hook 'set-newline-and-indent)

;; (dolist (command '(yank yank-pop))
;;   (eval `(defadvice ,command (after indent-region activate)
;;         (and (not current-prefix-arg)
;;              (member major-mode '(emacs-lisp-mode
;;                                   lisp-mode
;;                                   scheme-mode
;;                                   haskell-mode
;;                                   c-mode
;;                                   c++-mode
;;                                   latex-mode
;;                                   python-mode))
;;              (let ((mark-even-if-inactive transient-mark-mode))
;;                (indent-region (region-beginning (region-end) nil)))))))



;; APPEARANCE
(setq ring-bell-function 'ignore)
(visual-line-mode 1)
(set-face-attribute 'default nil :family "Monaco" :height 100)



(setq ring-bell-function 'ignore)

(visual-line-mode 1)
(require 'fringe)
(fringe-mode 20)
(setq overflow-newline-into-fringe nil)


;; LINE NUMBERS & LINUM

(require 'linum)
(defcustom linum-disabled-modes-list
  '(eshell-mode wl-summary-mode compilation-mode dired-mode speedbar-mode)
  "List of modes that have linum disabled when global linum mode is on"
  :type '(repeat (sexp :tag "Major mode"))
  :tag " Major modes that have linum disabled: "
  :group 'linum)

(defcustom linum-disable-starred-buffers 't
  "Disable linum in starred buffers"
  :type 'boolean
  :group 'linum)
(defun linum-on ()
  "When linum is running globally, disable line numbering in modes defined in
  linum-disabled-modes-list"
  (unless (or (minibufferp) (member major-mode linum-disabled-modes-list)
	      (and linum-disable-starred-buffers
		   (string-match "*" (buffer-name))))
    (linum-mode 1)))
(set-face-attribute 'linum nil
		    :background "gray20" :foreground "gray40")
(global-linum-mode t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

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

(setq org-directory (expand-file-name "~/ownCloud/org")
      org-agenda-files (list (expand-file-name "todo.org" org-directory)
			     (expand-file-name "thesis.org" org-directory)
			     (expand-file-name "baby.org" org-directory)
			     )
      org-default-notes-file (expand-file-name "notes.org" org-directory)
      org-completion-use-ido t
      org-yank-adjusted-subtrees t)
;; (setq org-directory (expand-file-name "~/Dropbox/Org")
;;       org-agenda-files (list (expand-file-name "work.org" org-directory)
;;                           (expand-file-name "thesis.org" org-directory)
;;                           (expand-file-name "home.org" org-directory))
;;       org-default-notes-file (expand-file-name "notes.org" org-directory)
;;       org-completion-use-ido t
;;       org-yank-adjusted-subtrees t)
(make-directory org-directory :with-parents)

(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c C") 'org-capture)

(require 'ox-publish)
(setq org-publish-project-alist
      '(
	("website-pages"
	 :base-directory "~/ownCloud/website/org/"
	 :base-extension "org"
	 :publishing-directory "~/ownCloud/website/public_html/"
	 :recursive t
	 :publishing-function org-html-publish-to-html
	 :headline-levels 4
	 :auto-preamble t
	 :auto-sitemap t
	 :sitemap-filename "sitemap.org"
	 :sitemap-title "Sitemap"
	 :export-creator-info nil
	 :export-author-info nil
	 :auto-postamle nil
	 :table-of-contents t
	 :section-numbers nil
	 :html-postamble "    <p class=\"postamble\">Last Updated %d.</p> "
	 :style-include-default nil)
	("website-static"
	 :base-directory "~/ownCloud/website/org/"
	 :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|ogg\\|swf"
	 :publishing-directory "~/ownCloud/website/public_html/"
	 :recursive t
	 :publishing-function org-publish-attachment)
	("website"
	 :components ("website-pages" "website-static"))))


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

(ignoramus-setup)

(winner-mode)

(windmove-default-keybindings) ;; Shift + Arrow to move between windows

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

(add-hook 'snippet-mode-hook 'indent-tabs-mode)

;; PYTHON
(elpy-enable)
(setq elpy-rpc-python-command "/usr/bin/python2")
(elpy-set-test-runner 'elpy-test-pytest-runner)
;; (setq python-check-command "python2 -m pyflakes")
(setq python-check-command "/usr/bin/pyflakes")
(add-hook 'python-mode-hook
	  (lambda ()
	    (define-key python-mode-map (kbd "C-c C-v") nil)))
(add-hook 'python-mode-hook
	  (lambda ()
	    (define-key python-mode-map (kbd "C-c C-v") 'elpy-check)))
(elpy-use-ipython)

;; until cython-mode works
(add-to-list 'auto-mode-alist '("\\.pyx\\'" . python-mode))

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
;; highlight parentheses
(require 'paren)
(show-paren-mode t)

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
;; (global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-n") 'end-of-defun)
(global-set-key (kbd "M-p") 'beginning-of-defun)


(defun large-file-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 1024 1024))
    ;; (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))

(add-hook 'find-file-hooks 'large-file-hook)

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

;; MODELINE

(setq-default
  mode-line-format
  '(;; position
    ;; (:propertize "%4l:" face mode-line-position-face)
    ;; (:eval (propertize "%3c" 'face
    ;;                 (if (>= (current-column) 80)
    ;;                     'mode-line-80col-face
    ;;                   'mode-line-position-face)))
    ;; " "
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
    " "
    (:propertize which-func-format
		 face mode-line-mode-face)

    ;; mode indicators: vc, recursive edit, major mode,
    ;; minor modes, process, global
    (vc-mode vc-mode)
    " "
    ;; " \["

    (:propertize mode-name
	      face mode-line-mode-face)
    ;; "\] "
	;; narrow
    " %n "
    "  "))

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
    :foreground "gray70" :background "gray25"
    :inverse-video nil
    :box '(:line-width 1 :color "gray30" :style nil)
    :family "DejaVu Sans Mono"
    :height 80)
(set-face-attribute 'mode-line-inactive nil
    :foreground "gray50" :background "gray20"
    :inverse-video nil
    :box '(:line-width 1 :color "gray20" :style nil)
    :family "DejaVu Sans Mono"
    :height 80)
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
    :height 80)
(set-face-attribute 'mode-line-mode-face nil
    :inherit 'mode-line-face
    :foreground "gray80")
(set-face-attribute 'mode-line-minor-mode-face nil
    :inherit 'mode-line-mode-face
    :foreground "gray40"
    :height 80)
(set-face-attribute 'mode-line-process-face nil
    :inherit 'mode-line-face
    :foreground "#718c00")
(set-face-attribute 'mode-line-80col-face nil
    :inherit 'mode-line-position-face
    :foreground "black" :background "#eab700")
;; (if (string= "x" window-system)
;;     (load-library "~/.emacs.d/windowed"))
