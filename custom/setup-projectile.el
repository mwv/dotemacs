(require 'projectile)
(projectile-global-mode)
(setq projectile-enable-caching t)

;;;;;;;;;;;;;;;;;;;;;
;; helm-projectile ;;
;;;;;;;;;;;;;;;;;;;;;

(require 'helm-projectile)

(setq projectile-completion-system 'helm)
(helm-projectile-on)

(provide 'setup-projectile)
