(use-package treesit
  :hook ((python-mode . python-ts-mode)))

(use-package eldoc
  :init
  (setq eldoc-echo-area-use-multiline-p nil))

(use-package eglot
  :hook (eglot-managed-mode-hook . eglot-inlay-hints-mode))

(use-package python
  :init

  ;; use anaconda/miniconda to manage the virtual
  ;; environments. conda.el does very well here to just swap between
  ;; these existing environments. I don't need much more than that.
  (use-package conda
    :ensure t
    :init
    (setq conda-env-current-name nil)
    (defun conda-activate-once (env)
      "Activate an environment if not already activated"
      (interactive)
      (unless (string= env conda-env-current-name)
	(conda-env-activate env))))

  ;; When creating a script, it is very useful to be able to send code
  ;; to a vterm window. The combination of the code cells using # %%
  ;; and isend-mode makes this possible.
  (use-package isend-mode
    :ensure t
    :init
    (setq isend-send-region-function 'isend--ipython-cpaste))
  (use-package code-cells :ensure t)
  (use-package ein :ensure t))

(use-package markdown-mode :ensure t)
(use-package csv-mode :ensure t)
(use-package sly :ensure t)  ;; common-lisp
(use-package cider :ensure t) ;; clojure

(use-package geiser-chez :ensure t) ;; chez-scheme
(use-package geiser-guile :ensure t)
(use-package geiser-mit :ensure t)

(use-package racket-mode
  :hook ((racket-mode . racket-xp-mode)
	 (racket-mode . paredit-mode))
  :ensure t
  :init
  (setq racket-program "/usr/local/racket/bin/racket"))
(use-package geiser-racket :ensure t
  :init
  (setq geiser-racket-binary "/usr/local/racket/bin/racket"
	geiser-default-implementation 'racket))

(use-package ess
  :ensure t
  :init
  (setq org-babel-R-command "/usr/bin/R --slave --no-save"))

(use-package paredit
  :ensure t
  :hook ((lisp-mode . paredit-mode)
	 (scheme-mode . paredit-mode)
	 (emacs-lisp-mode . paredit-mode)))

