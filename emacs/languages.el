(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode)
  :init
  (use-package yasnippet-snippets
    :ensure t)
  (yas-global-mode))

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
    (let ((workon-home (expand-file-name "~/.bin/miniconda3/envs")))
      (setenv "WORKON_HOME" workon-home)
      (setenv "VIRTUALENVWRAPPER_HOOK_DIR" workon-home))
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
  (use-package code-cells :ensure t))

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
  (setq org-babel-R-command "/usr/bin/R --slave --no-save")
  (add-hook 'ess-mode (lambda ()
                      (make-local-variable 'tab-width)
                      (setq tab-width 2))))

(use-package paredit
  :ensure t
  :hook ((lisp-mode . paredit-mode)
	 (scheme-mode . paredit-mode)
	 (emacs-lisp-mode . paredit-mode)))


(use-package cern-root-mode
  :ensure t
  :bind (:map c++-mode-map
	     (("C-c C-c" . cern-root-eval-defun)
	      ("C-c C-b" . cern-root-eval-buffer)
	      ("C-c C-l" . cern-root-eval-file)
	      ("C-c C-r" . cern-root-eval-region)))
  :init
  (setq cern-root-filepath "/opt/homebrew/bin/root"))
