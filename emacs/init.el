;;--------------------------
;; EMACS Configuration File
;; Author: Jay Morgan
;;--------------------------

(tool-bar-mode -1)
(scroll-bar-mode -1)
;; menu bar for osx is fine.
(when (not (eq system-type 'darwin))
  (menu-bar-mode -1))

;; no splash screen
(setq inhibit-splash-screen t)

;; set the modifiers for MacOS
(when (eq system-type 'darwin)
  (setq mac-command-modifier 'super
        mac-option-modifier 'meta
        mac-control-modifier 'control))

(setq ring-bell-function 'ignore)
(setq use-file-dialog nil)
(setq-default indent-tabs-mode nil) ; by default, do not use tabs for indentation.
(delete-selection-mode)	; delete whats highlighted if user types/pastes something

(setq custom-file (concat user-emacs-directory "custom.el")
      backup-directory-alist `(("." . "~/.cache/saves"))
      auto-save-file-name-transforms `((".*" "~/.cache/saves" t))
      make-backup-files nil
      create-lockfiles nil
      use-package-always-defer t)

(setq-default tab-width 4)

(when (file-exists-p custom-file)
  (load custom-file))

;; add paths to executable paths so that certain executables can be
;; found by Emacs.
(defun add-to-exec-path (path)
  (let ((fp (expand-file-name path)))
    (unless (member fp exec-path)
      (setq exec-path (cons fp exec-path)))))

(add-to-exec-path "~/miniconda3/bin")

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(defalias 'yes-or-no-p 'y-or-n-p)            ; easier to type a single letter

;; Automatically pair parentheses
(electric-pair-mode t)

(use-package gcmh
  :ensure t
  :init
  (gcmh-mode 1))

(use-package dired
  :init
  (use-package async :ensure t)
  (dired-async-mode t)
  (setq dired-dwim-target t
	    dired-listing-switches "-alh"   ; add human-readable sizes
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
  :bind (("C-M-i" . corfu-complete))
  :init
  (setq corfu-auto t
	tab-always-indent 'complete
	corfu-auto-delay 0.5
        corfu-popupinfo-delay '(0.5 . 0.5)
        corfu-popupinfo-max-height 20
        corfu-popupinfo-min-height 10)
  (global-corfu-mode t)
  (corfu-popupinfo-mode t))

(use-package vertico
  :ensure t
  :init
  (vertico-mode t)
  ; (vertico-flat-mode t)
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
  :bind (:map vterm-mode-map
              ("M-p" . vterm--self-insert))
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
  :bind (:map org-mode-map
              ("M-p" . org-babel-previous-src-block)
	          ("M-n" . org-babel-next-src-block)
              ("C-c S-<return>" . org-babel-execute-and-next)
              ("C-c C-c" . (lambda () (interactive) (org-ctrl-c-ctrl-c) (org-display-inline-images))))
  :init
  (defun org-babel-execute-and-next ()
    "Execute this current org-babel block and move onto the next one"
    (interactive)
    (org-ctrl-c-ctrl-c)
    (org-babel-next-src-block)
    (recenter-top-bottom))
  
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
     (gnuplot . t)
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

(use-package popper
  :ensure t
  :bind (("C-`" . popper-toggle)
         ("C-¬" . popper-cycle))
  :init
  (setq popper-reference-buffers
        '("\\*R:.*\\*"
          "\\*R\\*"
          "\\*Python\\*"
          "\\*vterm\\*")
        popper-window-height 30)
  (popper-mode t)
  (popper-echo-mode t))

(use-package ace-window
  :ensure t
  :bind ("M-o" . ace-window))

;; load the external files from the emacs directory.
(cl-flet ((load-subsection
	        (filename)
	        (load-file (concat user-emacs-directory filename))))
  (load-subsection "rsync.el")
  (load-subsection "languages.el")
  (load-subsection "keybindings.el")
                                        ;(load-subsection "notes.el")
  (load-subsection "theme.el")
  (load-subsection "project-management.el"))

