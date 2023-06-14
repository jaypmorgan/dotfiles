(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode)
  :init
  (use-package yasnippet-snippets
    :ensure t)
  (yas-global-mode))

(use-package treesit
  :disabled t
  :hook ((python-mode . python-ts-mode)))

(use-package eldoc
  :init
  (setq eldoc-echo-area-use-multiline-p nil))

(use-package eglot
  :ensure t
  :hook (eglot-managed-mode-hook . eglot-inlay-hints-mode))

(use-package python
  :init

  (use-package blacken
    :ensure t)

  (use-package numpydoc
    :ensure t)

  ;; use anaconda/miniconda to manage the virtual
  ;; environments. conda.el does very well here to just swap between
  ;; these existing environments. I don't need much more than that.
  (use-package pyvenv
    :ensure t
    :init
    (setq python-env-current-name nil)
    (defun conda-activate-once (env)
      "Activate an environment if not already activated"
      (interactive)
      (unless (string= env python-env-current-name)
	(pyvenv-activate env)
        (setq python-env-current-name env))))

  (defun run-python-remote (remote-path)
    (interactive "sRemote path")
    (eshell/cd remote-path)
    (run-python))

  ;; When creating a script, it is very useful to be able to send code
  ;; to a vterm window. The combination of the code cells using # %%
  ;; and isend-mode makes this possible.
  (use-package isend-mode
    :ensure t
    :init
    (setq isend-send-region-function 'isend--ipython-cpaste))
  (use-package code-cells :ensure t)
  (use-package ein :ensure t))

(defun lisp-insert-header (header)
  (interactive "sHeader: ")
  (let ((boundary-size 1)
	(num-comments  3)
	(comment-val   ";")
	(line-val      "-")
	(size (string-width header))
	(final-size (/ 70 2)))
    (dotimes (i num-comments)
      (insert comment-val))
    (dotimes (i (- (- final-size (/ size 2)) boundary-size))
      (insert line-val))
    (dotimes (i boundary-size)
      (insert " "))
    (insert header)
    (dotimes (i boundary-size)
      (insert " "))
    (dotimes (i (+ 1 (- (- final-size (/ size 2)) boundary-size)))
      (insert line-val))))

(use-package markdown-mode :ensure t)
(use-package csv-mode :ensure t)
(use-package sly :ensure t)  ;; common-lisp
(use-package cider :ensure t) ;; clojure

(use-package geiser-chez :ensure t) ;; chez-scheme
(use-package geiser-guile
  :ensure t
  :init
  (setq geiser-default-implementation 'guile))
(use-package geiser-mit :ensure t)

(use-package racket-mode
  :hook ((racket-mode . racket-xp-mode)
	 (racket-mode . paredit-mode))
  :ensure t
  :init
  (setq racket-program "/usr/local/racket/bin/racket"))
(use-package geiser-racket :ensure t
  :init
  (setq geiser-racket-binary "/usr/local/racket/bin/racket"))

(use-package ess
  :ensure t
  :init
  (setq org-babel-R-command "/usr/bin/R --slave --no-save"))

(use-package paredit
  :ensure t
  :hook ((lisp-mode . paredit-mode)
	 (scheme-mode . paredit-mode)
	 (emacs-lisp-mode . paredit-mode)))

