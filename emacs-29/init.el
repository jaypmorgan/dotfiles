(defun add-to-exec-path (path)
  (let ((fp (expand-file-name path)))
    (unless (member fp exec-path)
      (setq exec-path (cons fp exec-path)))))

(add-to-exec-path "~/.bin")
(add-to-exec-path "~/.bin/miniconda3/bin")

(delete-selection-mode)	; delete whats highlighted if user types/pastes something

(setq custom-file (concat user-emacs-directory "custom.el")
      make-backup-files nil
      create-lockfiles nil
      backup-directory-alist `(("." . "~/.cache/saves"))
      use-package-always-defer t)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)

(defalias 'yes-or-no-p 'y-or-n-p)            ; easier to type a single letter
(set-face-attribute 'default nil :height 90) ; make font slightly smaller

(use-package dired
  :init
  (setq dired-dwim-target t
	dired-listing-switches "-alh"	     ; add human-readable sizes
	dired-auto-revert-buffer t))

(use-package magit
  :bind (("C-x p v" . magit-project-status)) ; replace built-in vc
  :ensure t
  :init
  (require 'magit-extras))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))

(use-package corfu
  :ensure t
  :init
  (setq corfu-auto t
	tab-always-indent 'complete
	corfu-auto-delay 0.75)
  (global-corfu-mode))

(use-package vertico
  :ensure t
  :init
  (vertico-mode t)
  (use-package orderless
    :ensure t
    :init
    (setq completion-styles '(orderless basic)
          completion-category-defaults nil
          completion-category-overrides '((file (styles partial-completion))))))

(use-package avy
  :ensure t
  :bind (("M-n" . avy-goto-char-2)))

(use-package vterm
  :ensure t
  :init
  ;; decrease the input delay -- need to test for receiving large
  ;; outputs though
  (setq vterm-timer-delay 0.01))

(when (file-exists-p "/usr/local/share/emacs/site-lisp/mu4e/mu4e.el")
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
  (require 'org-mu4e)
  (use-package async :ensure t)
  (setq mail-user-agent 'mu4e-user-agent)
  (setq org-mu4e-convert-to-html t)
  (let ((mu4e-config (concat user-emacs-directory "mu4e-init.el")))
    (when (file-exists-p mu4e-config)
      (load mu4e-config))))

(use-package org
  :hook ((org-mode . auto-fill-mode))
  :init
  (use-package ob-async :ensure t)
  (use-package mixed-pitch
    :ensure t
    :hook (org-mode . mixed-pitch-mode))
  (use-package pdf-tools ;; better PDF viewing
    :ensure t
    :init
    (pdf-tools-install))
  (setq org-babel-lisp-eval-fn #'sly-eval
	org-src-window-setup 'current-window)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((R . t)
     (python . t)
     (emacs-lisp . t)
     (shell . t)
     (lisp . t)
     (ein . t)
     (scheme . t))))

(defun insert-line-above ()
  "Insert and indent to the next line"
  (interactive)
  (beginning-of-visual-line)
  (newline-and-indent)
  (previous-line)
  (indent-according-to-mode))

(defun insert-line-below ()
  "Insert and indent from any point in a line"
  (interactive)
  (end-of-visual-line)
  (newline-and-indent))

(defun morg/list-jobs ()
  "List the SLURM jobs in queue on supercomputer."
  (interactive)
  (async-shell-command
   "ssh lis.me squeue -u jay.morgan"
   "*SLURM jobs*"))

;; load the external files from the emacs directory.
(cl-flet ((load-subsection
	    (filename)
	    (load (concat user-emacs-directory filename))))
  (load-subsection "rsync.el")
  (load-subsection "languages.el")
  (load-subsection "keybindings.el")
  (load-subsection "notes.el")
  (load-subsection "theme.el"))
