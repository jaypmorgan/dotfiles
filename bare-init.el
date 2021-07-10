(setq auto-save-default nil
      backup-inhibited t
      custom-file (concat user-emacs-directory "custom.el")
      revert-without-query t
      require-final-newline t
      indent-tabs-mode nil)

;; setup straight.el instead of package.el
(setq package-enable-at-startup nil)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package company
  :hook (after-init . global-company-mode))

(use-package projectile
  :init
  (projectile-mode t)
  (setq projectile-project-search-path '("~/workspace/"))
  (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map))

(use-package vertico
  :init
  (vertico-mode t))

(defun insert-line-above ()
  "Insert and indent to the next line"
  (interactive)
  (beginning-of-visual-line)
  (newline-and-indent)
  (previous-line)
  (indent-according-to-mode))

(defun insert-line-below ()
  "Insert and indent to the previous line"
  (interactive)
  (end-of-visual-line)
  (newline-and-indent))

(global-set-key (kbd "C-o") #'insert-line-below)
(global-set-key (kbd "C-S-o") #'insert-line-above)

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package python-mode
  (use-package elpy
    :init
    (use-package pyvenv
      :init
      (setenv "WORKON_HOME" (expand-file-name "~/miniconda3/envs"))
      (pyvenv-mode t))
    (setq elpy-rpc-backend "jedi")
    (setq python-indent-offset 2)
    (elpy-enable)))

(use-package ess)

(use-package slurp-mode
  :straight (slurp-mode :type git :host github :repo "jaypmorgan/slurp-mode")
  :init
  (setq slurp-repl-location "~/workspace/slurp/slurp"))

(use-package slurp-repl-mode
  :straight (slurp-repl-mode :type git :host github :repo "jaypmorgan/slurp-mode")
  :bind (:map slurp-mode-map
              ("C-c C-c" . slurp-repl-send-line)
              ("C-c C-z" . run-slurp-other-window)))

(defun conda-activate-once (name)
  (interactive))
