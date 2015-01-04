(setq inhibit-startup-message t)

(require 'package)

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")
                         ("elpy" . "http://jorgenschaefer.github.io/packages/")))
(package-initialize)

(defconst my-packages
  '(anzu
    clean-aindent-mode
    company
    comment-dwim-2
    dash
    dtrt-indent
    duplicate-thing
    ein
    elpy
    fic-mode
    fill-column-indicator
    function-args
    git-gutter-fringe
    ggtags
    haskell-mode
    helm
    helm-gtags
    helm-projectile
    helm-swoop
    iedit
    ignoramus
    info+
    magit
    moe-theme
    nose
    projectile
    rainbow-mode
    s
    smartparens
    sml-mode
    undo-tree
    volatile-highlights
    ws-butler
    yasnippet
    zygospore))

(defun install-packages ()
  "Install my packages."
  (interactive)
  (unless package-archive-contents
    (package-refresh-contents))
  (dolist (package my-packages)
    (unless (package-installed-p package)
      (package-install package))))

(install-packages)

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

(add-to-list 'load-path "~/.emacs.d/custom")

(require 'setup-appearance)
(require 'setup-helm)
(require 'setup-projectile)
(require 'setup-magit)
(require 'setup-cedet)
(require 'setup-cc)
(require 'setup-python)
(require 'setup-haskell)
(require 'setup-editing)
(require 'setup-org)

(require 'saveplace)
(setq-default save-place t)

;; put all backups in same folder
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))


;; MINIBUFFER HISTORY
(require 'savehist)
(setq savehist-save-minibuffer-history t
      savehist-autosave-interval 180)
(savehist-mode t)

;; RECENT FILES
(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode t)

;; Package zygospore
(global-set-key (kbd "C-x 1") 'zygospore-toggle-delete-other-windows)

;; fix for company and yasnippet

(define-key company-active-map "\t" 'company-yasnippet-or-completion)
(defun company-yasnippet-or-completion ()
  (interactive)
  (if (yas/expansion-at-point)
      (progn (company-abort)
             (yas/expand))
    (company-complete-common)))
(defun yas/expansion-at-point ()
  "tested with v0.6.1. Extracted from `yas/expand'"
  (first (yas/current-key)))


;; bind helm-imenu
(global-set-key (kbd "C-c C-j") 'helm-imenu)
(global-set-key (kbd "C-.") 'helm-imenu)
