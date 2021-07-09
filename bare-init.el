(setq auto-save-default nil)
(setq backup-inhibited t)
(setq custom-file (concat user-emacs-directory "custom.el"))

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
  (setq projectile-project-search-path '("/media/hdd/workspace/"))
  (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map))

(use-package which-key
  :init
  (which-key-mode t))

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
  :init
  (setq py-keep-windows-configuration t))

(use-package ess)

(defun conda-activate-once (name)
  (interactive))
