;; FILE BINDINGS
(add-to-list 'auto-mode-alist '("\\.zsh\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; ignore case when searching
(setq case-fold-search t)

;; GROUP: Editing -> Editing Basics

(setq global-mark-ring-max 5000         ; increase mark ring to contains 5000 entries
      mark-ring-max 5000                ; increase kill ring to contains 5000 entries
      mode-require-final-newline t      ; add a newline to end of file
      tab-width 4                       ; default to 4 visible spaces to display a tab
      )

(add-hook 'sh-mode-hook (lambda ()
              (setq tab-width 4)))

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-language-environment "UTF-8")
(prefer-coding-system 'utf-8)

(setq-default indent-tabs-mode nil)
(delete-selection-mode)
(global-set-key (kbd "RET") 'newline-and-indent)

;; GROUP: Editing -> Killing
(setq kill-ring-max 5000 ; increase kill-ring capacity
      kill-whole-line t  ; if NIL, kill whole line and move the next line up
      )

;; show whitespace in diff-mode
(add-hook 'diff-mode-hook (lambda ()
                (setq-local whitespace-style
                    '(face
                      tabs
                      tab-mark
                      spaces
                      space-mark
                      trailing
                      indentation::space
                      indentation::tab
                      newline
                      newline-mark))
                (whitespace-mode 1)))

;; Package: volatile-highlights
;; GROUP: Editing -> Volatile Highlights
(require 'volatile-highlights)
(volatile-highlights-mode t)

;; Package: clean-aindent-mode
;; GROUP: Editing -> Indent -> Clean Aindent
(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)


;; PACKAGE: dtrt-indent
(require 'dtrt-indent)
(dtrt-indent-mode 1)
(setq dtrt-indent-verbosity 0)

;; PACKAGE: ws-butler
(require 'ws-butler)
(add-hook 'c-mode-common-hook 'ws-butler-mode)
(add-hook 'text-mode 'ws-butler-mode)
(add-hook 'fundamental-mode 'ws-butler-mode)

;; Package: undo-tree
;; GROUP: Editing -> Undo -> Undo Tree
(require 'undo-tree)
(global-undo-tree-mode)

(require 'saveplace)
(setq-default save-place t)

(require 'iedit)


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

;; Package: yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets/"))
(yas-global-mode 1)

;; PACKAGE: smartparens
(require 'smartparens-config)
(setq sp-base-key-bindings 'paredit)
(setq sp-autoskip-closing-pair 'always)
(setq sp-hybrid-kill-entire-symbol nil)
(sp-use-paredit-bindings)

(show-smartparens-global-mode +1)
(smartparens-global-mode 1)

;; PACKAGE: comment-dwim-2
(global-set-key (kbd "M-;") 'comment-dwim-2)

;; Jump to end of snippet definition
(define-key yas-keymap (kbd "<return>") 'yas/exit-all-snippets)

;; Inter-field navigation
(defun yas/goto-end-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
     (position (yas--field-end (yas--snippet-active-field snippet))))
    (if (= (point) position)
    (move-end-of-line 1)
      (goto-char position))))

(defun yas/goto-start-of-active-field ()
  (interactive)
  (let* ((snippet (car (yas--snippets-at-point)))
     (position (yas--field-start (yas--snippet-active-field snippet))))
    (if (= (point) position)
    (move-beginning-of-line 1)
      (goto-char position))))

(define-key yas-keymap (kbd "C-e") 'yas/goto-end-of-active-field)
(define-key yas-keymap (kbd "C-a") 'yas/goto-start-of-active-field)
;; (define-key yas-minor-mode-map [(tab)] nil)
;; (define-key yas-minor-mode-map (kbd "TAB") nil)
;; (define-key yas-minor-mode-map (kbd "C-<tab>") 'yas-expand)
;; No dropdowns please, yas
(setq yas-prompt-functions '(yas/ido-prompt yas/completing-prompt))

;; No need to be so verbose
(setq yas-verbosity 1)

;; Wrap around region
(setq yas-wrap-around-region t)

(add-hook 'term-mode-hook (lambda() (setq yas-dont-activate t)))

;; PACKAGE: anzu
;; GROUP: Editing -> Matching -> Isearch -> Anzu
(require 'anzu)
(global-anzu-mode)
(global-set-key (kbd "M-%") 'anzu-query-replace)
(global-set-key (kbd "C-M-%") 'anzu-query-replace-regexp)

;; PACKAGE: iedit
(setq iedit-toggle-key-default nil)
(require 'iedit)
(global-set-key (kbd "C-;") 'iedit-mode)

;; PACKAGE: duplicate-thing
(require 'duplicate-thing)
(global-set-key (kbd "M-c") 'duplicate-thing)

;; Customized functions
(defun prelude-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first. If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(global-set-key (kbd "C-a") 'prelude-move-beginning-of-line)

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single
line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
       (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single
  line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
       (line-beginning-position 2)))))

;; kill a line, including whitespace characters until next non-whiepsace character
;; of next line
(defadvice kill-line (before check-position activate)
  (if (member major-mode
          '(emacs-lisp-mode scheme-mode lisp-mode
                c-mode c++-mode objc-mode
                latex-mode plain-tex-mode))
      (if (and (eolp) (not (bolp)))
      (progn (forward-char 1)
         (just-one-space 0)
         (backward-char 1)))))

;; taken from prelude-editor.el
;; automatically indenting yanked text if in programming-modes
(defvar yank-indent-modes
  '(LaTeX-mode TeX-mode)
  "Modes in which to indent regions that are yanked (or yank-popped).
Only modes that don't derive from `prog-mode' should be listed here.")

(defvar yank-indent-blacklisted-modes
  '(python-mode slim-mode haml-mode)
  "Modes for which auto-indenting is suppressed.")

(defvar yank-advised-indent-threshold 1000
  "Threshold (# chars) over which indentation does not automatically occur.")

(defun yank-advised-indent-function (beg end)
  "Do indentation, as long as the region isn't too large."
  (if (<= (- end beg) yank-advised-indent-threshold)
      (indent-region beg end nil)))

(defadvice yank (after yank-indent activate)
  "If current mode is one of 'yank-indent-modes,
indent yanked text (with prefix arg don't indent)."
  (if (and (not (ad-get-arg 0))
       (not (member major-mode yank-indent-blacklisted-modes))
       (or (derived-mode-p 'prog-mode)
           (member major-mode yank-indent-modes)))
      (let ((transient-mark-mode nil))
    (yank-advised-indent-function (region-beginning) (region-end)))))

(defadvice yank-pop (after yank-pop-indent activate)
  "If current mode is one of `yank-indent-modes',
indent yanked text (with prefix arg don't indent)."
  (when (and (not (ad-get-arg 0))
         (not (member major-mode yank-indent-blacklisted-modes))
         (or (derived-mode-p 'prog-mode)
         (member major-mode yank-indent-modes)))
    (let ((transient-mark-mode nil))
      (yank-advised-indent-function (region-beginning) (region-end)))))

;; prelude-core.el
(defun indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

;; prelude-editing.el
(defcustom prelude-indent-sensitive-modes
  '(coffee-mode python-mode slim-mode haml-mode yaml-mode)
  "Modes for which auto-indenting is suppressed."
  :type 'list)

(defun indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (unless (member major-mode prelude-indent-sensitive-modes)
    (save-excursion
      (if (region-active-p)
      (progn
        (indent-region (region-beginning) (region-end))
        (message "Indented selected region."))
    (progn
      (indent-buffer)
      (message "Indented buffer.")))
      (whitespace-cleanup))))

(global-set-key (kbd "C-c i") 'indent-region-or-buffer)

;; add duplicate line function from Prelude
;; taken from prelude-core.el
(defun prelude-get-positions-of-line-or-region ()
  "Return positions (beg . end) of the current line
or region."
  (let (beg end)
    (if (and mark-active (> (point) (mark)))
    (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
    (exchange-point-and-mark))
    (setq end (line-end-position))
    (cons beg end)))

;; smart openline
(defun prelude-smart-open-line (arg)
  "Insert an empty line after the current line.
Position the cursor at its beginning, according to the current mode.
With a prefix ARG open line above the current line."
  (interactive "P")
  (if arg
      (prelude-smart-open-line-above)
    (progn
      (move-end-of-line nil)
      (newline-and-indent))))

(defun prelude-smart-open-line-above ()
  "Insert an empty line above the current line.
Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key (kbd "M-o") 'prelude-smart-open-line)
(global-set-key (kbd "M-o") 'open-line)


;; NEWLINES

(add-hook 'lisp-mode-hook 'set-newline-and-indent)
(add-hook 'python-mode-hook 'set-newline-and-indent)
(add-hook 'c-mode-hook 'set-newline-and-indent)


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

(ignoramus-setup)

(require 'align)
(global-set-key (kbd "C-x a r") 'align-regexp)

(add-to-list 'align-rules-list
         '(haskell-types
           (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
           (modes quote (haskell-mode literate-haskell-mode))))
(add-to-list 'align-rules-list
         '(haskell-assignment
           (regexp . "\\(\\s-+\\)=\\s-+")
           (modes quote (haskell-mode literate-haskell-mode))))
(add-to-list 'align-rules-list
         '(haskell-arrows
           (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
           (modes quote (haskell-mode literate-haskell-mode))))
(add-to-list 'align-rules-list
         '(haskell-left-arrows
           (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
           (modes quote (haskell-mode literate-haskell-mode))))

(defun kill-this-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x C-k") 'kill-this-buffer)

;; line wrap
(setq line-move-visual t)

(defun start-shell ()
  (interactive)
  (ansi-term "/usr/bin/zsh"))

(global-set-key [f1] 'start-shell)
(global-set-key [f2] 'rgrep)
(global-set-key [f5] 'goto-line)
(global-set-key (kbd "C-a") 'smarter-move-beginning-of-line)
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

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))

;; use space to indent by default
(setq-default indent-tabs-mode nil)

;; set appearance of a tab that is represented by 4 spaces
(setq-default tab-width 4)

;; Package: clean-aindent-mode
(require 'clean-aindent-mode)
(add-hook 'prog-mode-hook 'clean-aindent-mode)

;; Package: dtrt-indent
(require 'dtrt-indent)
(dtrt-indent-mode 1)

;; Package: ws-butler
(require 'ws-butler)
(add-hook 'prog-mode-hook 'ws-butler-mode)

(provide 'setup-editing)
