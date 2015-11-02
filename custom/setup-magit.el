(require 'magit)
(setq magit-save-some-buffers 'dontask
      magit-set-upstream-on-push t)

(setq magit-auto-revert-mode nil)
;; (setq magit-last-seen-setup-instructions "1.4.0")

;; extra newline to separate commit mesage from git commentary
;; (defun magit-commit-mode-init ()
;;   (when (looking-at "\n")
;;     (open-line 1)))
;; (add-hook 'git-commit-mode-hook 'magit-commit-mode-init)

(global-set-key (kbd "C-x g") 'magit-status)

;;;;;;;;;;;;;;;;
;; git-gutter ;;
;;;;;;;;;;;;;;;;

;; (require 'git-gutter-fringe)

;; (set-face-foreground 'git-gutter-fr:modified "orange")
;; ;; (set-face-background 'git-gutter-fr:modified "black")
;; (set-face-foreground 'git-gutter-fr:added    "green")
;; ;; (set-face-background 'git-gutter-fr:added    "black")
;; (set-face-foreground 'git-gutter-fr:deleted  "red")
;; ;; (set-face-background 'git-gutter-fr:deleted  "black")
;; (setq git-gutter-fr:side 'left-fringe)
;; (global-git-gutter-mode)

(provide 'setup-magit)
