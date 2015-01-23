;; PYTHON
(elpy-enable)
(setq elpy-rpc-python-command "/usr/bin/python2")

(defun elpy-test-pytest-verbose-runner (top file module test)
  "Test the project using the py.test test runner and print verbose test results."
  (interactive (elpy-test-at-point))
  (cond
   (test
    (let ((test-list (split-string test "\\.")))
      (elpy-test-run top
                     "py.test" "-vv" (mapconcat #'identity
                                                (cons file test-list)
                                                "::"))))
   (module
    (elpy-test-run top
                   "py.test" "-vv" file))
   (t
    (elpy-test-run top
                   "py.test" "-vv"))))
(put 'elpy-test-pytest-verbose-runner 'elpy-test-runner-p t)
(elpy-set-test-runner 'elpy-test-pytest-verbose-runner)

(setq python-check-command "/usr/bin/pyflakes")
(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map (kbd "C-c C-v") nil)))
(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map (kbd "C-c C-v") 'elpy-check)))
(elpy-use-ipython)

(setq elpy-rpc-backend "jedi")

;; until cython-mode works
(add-to-list 'auto-mode-alist '("\\.pyx\\'" . python-mode))

(require 'ein)

(provide 'setup-python)
