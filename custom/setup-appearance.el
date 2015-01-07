(setq inhibit-startup-message t)
(setq inhibit-splash-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq ring-bell-function 'ignore)
(visual-line-mode t)
(set-face-attribute 'default nil :family "Monaco" :height 110)

(require 'fringe)
(fringe-mode 20)
(setq overflow-newline-into-fringe nil)


(require 'avoid)
(mouse-avoidance-mode 'animate)

(load-theme 'grandshell t)
;; (require 'moe-theme)
;; (moe-dark)
;; (set-face-attribute 'font-lock-comment-face nil
;;                     :foreground "#00d7af")
;; (set-face-attribute 'font-lock-comment-delimiter-face nil
;;                     :foreground "#00d7af")

;; (require 'rainbow-delimiters)
;; (set-face-attribute 'rainbow-delimiters-unmatched-face nil
;;                     :foreground "black"
;;                     :background "red")
;; (set-face-attribute 'rainbow-delimiters-mismatched-face nil
;;                     :foreground "black"
;;                     :background "red")
;; (set-face-attribute 'rainbow-delimiters-depth-1-face nil
;;                     :foreground "#c0c0c0"
;;                     :background "black")
;; (set-face-attribute 'rainbow-delimiters-depth-2-face nil
;;                     :foreground "#5faf00")

;; (set-face-attribute 'show-paren-mismatch nil
;;                      :foreground "black"
;;                      :background "red")
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(require 'iedit)
(set-face-foreground 'iedit-occurrence "black")
(set-face-background 'iedit-occurrence "yellow")

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


;; fic-mode: TODO/FIXME/BUG/KLUDGE in special face in comments only
(require 'fic-mode)
(add-hook 'prog-mode-hook 'turn-on-fic-mode)

;; WHICH-FUNCTION-MODE
;; Show the current function name in the header line
(which-function-mode)
(set-face-background 'which-func "gray30")

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
                    :background "black" :foreground "gray30")
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
                    :background "black")

(require 'company)
(defvar-local company-fci-mode-on-p nil)
(defun company-turn-off-fci (&rest ignore)
  (when (boundp 'fci-mode)
    (setq company-fci-mode-on-p fci-mode)
    (when fci-mode (fci-mode -1))))

(defun company-maybe-turn-on-fci (&rest ignore)
  (when company-fci-mode-on-p (fci-mode 1)))

(add-hook 'company-completion-started-hook 'company-turn-off-fci)
(add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
(add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)


(require 'highlight-indentation)
(set-face-background 'highlight-indentation-face "#101010")

;; mark transient
(transient-mark-mode t)

;; text decoration
(require 'font-lock)

(setq font-lock-maximum-decoration t)
(global-font-lock-mode t)
(global-hi-lock-mode nil)
(setq jit-lock-contextually nil)
(setq jit-lock-stealth-verbose nil)

;; highlight parentheses
(require 'paren)

(show-paren-mode t)

;; MODELINE

(setq-default
 mode-line-format
 '((:eval
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
                    :foreground "gray70" :background "grey25"
                    :inverse-video nil
                    :box '(:line-width 1 :color "gray30" :style nil)
                    :family "Monaco"
                    :height 110)
(set-face-attribute 'mode-line-inactive nil
                    :foreground "gray50" :background "gray20"
                    :inverse-video nil
                    :box '(:line-width 1 :color "gray20" :style nil)
                    :family "Monaco"
                    :height 110)
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
                    :family "Monaco"
                    :height 110)
(set-face-attribute 'mode-line-mode-face nil
                    :inherit 'mode-line-face
                    :foreground "gray80")
(set-face-attribute 'mode-line-minor-mode-face nil
                    :inherit 'mode-line-mode-face
                    :foreground "gray40"
                    :height 110)
(set-face-attribute 'mode-line-process-face nil
                    :inherit 'mode-line-face
                    :foreground "#718c00")
(set-face-attribute 'mode-line-80col-face nil
                    :inherit 'mode-line-position-face
                    :foreground "black" :background "#eab700")

(provide 'setup-appearance)
