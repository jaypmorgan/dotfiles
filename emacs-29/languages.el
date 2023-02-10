(use-package treesit
  :hook ((python-mode . python-ts-mode)))

(use-package eglot)

(use-package python
  :init
  (use-package conda
    :ensure t
    :init
    (setq conda-env-current-name nil)
    (defun conda-activate-once (env)
      "Activate an environment if not already activated"
      (interactive)
      (unless (string= env conda-env-current-name)
	(conda-env-activate env))))
  (use-package isend-mode
    :ensure t
    :init
    (setq isend-send-region-function 'isend--ipython-cpaste))
  (use-package code-cells :ensure t))

(use-package csv-mode :ensure t)
(use-package sly :ensure t)  ;; common-lisp

(use-package ess
  :ensure t
  :init
  (setq org-babel-R-command "/usr/bin/R --slave --no-save"))

(use-package paredit
  :ensure t
  :hook ((lisp-mode . paredit-mode)
	 (emacs-lisp-mode . paredit-mode)))

