;; HASKELL
(setq haskell-font-lock-symbols t)
(add-hook 'haskell-mode-hook
          'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook
          'interactive-haskell-mode)
(setq haskell-process-type 'cabal-repl)
(eval-after-load "haskell-mode"
  '(progn
     (define-key haskell-mode-map (kbd "C-,") 'haskell-move-nested-left)
     (define-key haskell-mode-map (kbd "C-.") 'haskell-move-nested-right)
     (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
     (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
     (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
     (define-key haskell-mode-map (kbd "C-c C-b") 'haskell-process-cabal-build)
     (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-check)
     (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
     (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
     (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
     )
  )

(custom-set-variables
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-interactive-popup-errors nil)
 '(haskell-process-type (quote cabal-repl))
 '(haskell-process-auto-import-loaded-modules t))

(provide 'setup-haskell)
