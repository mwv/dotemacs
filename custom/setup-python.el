;; PYTHON
(elpy-enable)
(setq elpy-rpc-python-command "/usr/bin/python2")
(elpy-set-test-runner 'elpy-test-pytest-runner)
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

(provide 'setup-python)
