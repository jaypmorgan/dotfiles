;;--------------------------
;; EMACS Configuration File
;; Author: Jay Morgan
;;--------------------------
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024))

(defun my/add-to-exec (new-path)
  (let ((new-path (expand-file-name new-path)))
  (setq exec-path (push new-path exec-path))
  (setenv "PATH" (format "%s:%s" (getenv "PATH") new-path))))
(my/add-to-exec "~/miniconda3/bin")
(my/add-to-exec "~/.fzf/bin")

;; Manually installed plugins/packages
(add-to-list 'load-path (expand-file-name "~/.emacs.d/plugins/"))
(when (file-exists-p "/usr/local/share/emacs/site-lisp/mu4e/mu4e.el")
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e/")
  (require 'mu4e))
(when (file-exists-p "~/.emacs.d/mu4e-init.el")
    (load "~/.emacs.d/mu4e-init.el"))

;; Setup package.el to work with MELPA
(setq package-check-signature nil)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-refresh-contents)
(package-initialize)

(setq evil-want-keybinding nil)
(setq x-wait-for-event-timeout nil)

;; =====================================================================
;; Install & Configure Packages
;; =====================================================================

;; Install function define a function to check if a package is
;; installed, if it not we can install it. From this, we may quickly
;; and easily install packages.
(defun my/check-and-install (pkg)
  (unless (package-installed-p pkg)
    (package-install pkg))
  (require pkg))

(my/check-and-install 'use-package)
(setq use-package-always-ensure t)
(use-package use-package-ensure-system-package)

(use-package evil
  :config
  (evil-mode 1))

(use-package hydra)
(use-package docker
  :bind ("C-c d" . docker))

(use-package which-key
  :config
  (setq which-key-idle-delay 1)
  (which-key-mode 1))

(use-package evil-collection
  :after (evil)
  :config
  (evil-collection-init))

(use-package company
  :config
  (global-company-mode 1)
  (setq company-idle-delay 0.001))

(use-package projectile
  :config
  (projectile-mode 1)
  (setq projectile-git-submodule-command nil)
  (setq projectile-mode-line-function '(lambda () (format " Proj[%s]" (projectile-project-name))))
  (setq projectile-project-search-path '("~/workspace/")))

(use-package blacken)
(use-package itail)
(use-package diminish)

(use-package julia-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.jmd\\'" . markdown-mode))
  (use-package julia-repl
    :hook
    ((julia-mode-hook . julia-repl-mode)
     (julia-repl-hook . julia-repl-use-emacsclient))
    :init
    (setenv "JULIA_NUM_THREADS" "4")
    (setq julia-repl-executable-records
          '((default "julia")
            (master "/usr/bin/julia")
            (chemistry "ssh -t chemistry.me julia")))))

(use-package clojure-mode)
(use-package markdown-mode)
(use-package magit)

(use-package mu4e-alert
  :init
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display))

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  (setq doom-modeline-height 25
        doom-modeline-mu4e t
        doom-modeline-icon t))

(use-package disable-mouse)
(use-package imenu-list)
(use-package linum-relative)
(use-package slime)
(use-package htmlize)
(use-package base16-theme)
(use-package cider)
(use-package php-mode)
(use-package ace-window)
(use-package focus)
(use-package web-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode)))

(use-package quelpa)
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

(use-package python-mode
  :config
  (use-package ess)
  (use-package conda
    :init
    (conda-env-initialize-eshell)
    (setq conda-anaconda-home (expand-file-name "~/miniconda3/")
          conda-env-home-directory (expand-file-name "~/miniconda3/"))))

(use-package lsp-mode
  :quelpa t
  :hook ((python-mode . lsp)
         (julia-mode . lsp)
         (ess-julia-mode . lsp)
         (sh-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :init
  (require 'lsp-clients)
  (quelpa '(lsp-julia :fetcher github
                      :repo "non-Jedi/lsp-julia"
                      :files (:defaults "languageserver")))
  (require 'lsp-julia)
  (setq lsp-diagnostics-modeline-scope :project)
  (setq lsp-enable-links nil)
  (setq lsp-modeline-code-actions-enable nil)
  (setq lsp-lens-mode nil)
  (setq lsp-idle-delay 1000)
  (add-hook 'lsp-managed-mode-hook 'lsp-diagnostics-modeline-mode)
  (add-hook 'lsp-managed-mode-hook 'lsp-modeline-code-actions-mode)
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (use-package lsp-ui
    :config
    (setq lsp-ui-doc-enable t)
    (setq lsp-ui-sideline--code-actions nil)
    (setq lsp-ui-sideline-show-code-actions nil)
    (setq lsp-ui-peek-enable nil)
    (setq lsp-ui-doc-enable nil)
    (add-hook 'lsp-mode-hook 'lsp-ui-mode))
  (use-package company-lsp
    :requires company
    :config
    (push 'company-lsp company-backends)
    (setq company-lsp-async t)))

(use-package org
  :after cider
  :ensure org-plus-contrib
  :init
  (setq org-startup-indented t)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook '(lambda () (set-fill-column 80)))
  (add-hook 'org-mode-hook #'auto-fill-mode)
  (require 'ob-clojure)
  (require 'cider)
  (use-package ob-async)
  (use-package ox-pandoc)
  (use-package ox-gfm)
  (use-package org-ref
    :init
    (setq reftex-default-bibliography "~/Dropbox/Notes/Wiki/library.bib"
          org-ref-default-bibliography '("~/Dropbox/Notes/Wiki/library.bib")))
  (use-package helm-bibtex
    :init
    (setq bibtex-completion-bibliography "~/Dropbox/Notes/Wiki/library.bib"
          bibtex-completion-pdf-open-function 'org-open-file))
  (add-to-list 'org-latex-packages-alist '("" "tikz" t))
  (add-to-list 'org-latex-compilers "tectonic")
  (add-hook 'org-mode-hook 'turn-on-auto-fill)
  (eval-after-load "preview" '(add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzpicture}" t))

  (require 'ox-latex)
  (add-to-list 'org-latex-classes
               '("thesis"
                 "\\documentclass{book}\n
                  \\usepackage{amssymb}
                  \\usepackage{gensymb}
                  \\usepackage[margin=1.5in]{geometry}
                  \\usepackage[T1]{fontenc}
                  \\usepackage{kpfonts,baskervald}
                  \\usepackage{units}
                  \\setlength{\\parskip}{11pt}
                  \\setlength{\\parindent}{0pt}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

  ;; set variables
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.4)
        inferior-julia-program-name "/usr/bin/julia"
        org-confirm-babel-evaluate nil
        org-babel-clojure-backend 'cider
        org-fontify-done-headline t)
        org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)"))

  ;; list of languages for org-mode to support
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((shell . t)
                                 (emacs-lisp . t)
                                 (julia . t)
                                 (gnuplot . t)
                                 (dot . t))))

(use-package yasnippet
  :config
  (use-package yasnippet-snippets)
  :init
  (yas-global-mode))

(use-package olivetti
  :init
  (setq olivetti-body-width 90)
  (add-hook 'olivetti-mode-hook (lambda ()
                                  (progn
                                    (linum-relative-mode nil)
                                    (fringe-mode nil)
                                    (hl-line-mode nil)))))

(use-package paredit
  :init
  (add-hook 'clojure-mode-hook #'enable-paredit-mode))

(use-package pdf-tools
  :init
  (pdf-loader-install)
  (setq auto-revert-interval 0.5)
  (add-hook 'pdf-view-mode-hook (lambda () (linum-mode -1))))

(use-package flyspell
  :init
  (setq flyspell-default-dictionary "british"))

(use-package flycheck
  :init
  (flycheck-add-mode 'proselint 'org-mode)
  (add-hook 'python-mode-hook 'flycheck-mode)
  (setq flycheck-check-syntax-automatically '(mode-enabled save)
        flycheck-highlighting-mode 'lines
        flycheck-indication-mode 'left-fringe
        flycheck-checker-error-threshold 200
        python-interp "~/miniconda3/bin/python"
        flycheck-python-flake8-executable python-interp))

(use-package eyebrowse
  :config
  (eyebrowse-mode 1)
  (setq eyebrowse-new-workspace t))

(use-package helm
  :config
  (helm-mode 1)
  (use-package helm-projectile)
  (use-package helm-ag
    :ensure-system-package (ag . silversearcher-ag))
  (setq helm-use-frame-when-more-than-two-windows nil
        helm-split-window-in-side nil
        helm-display-function 'pop-to-buffer
        helm-idle-delay 0.0
        helm-input-idle-delay 0.01))

(use-package vterm
  :commands (vterm vterm-other-window)
  :custom (vterm-kill-buffer-on-exit t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("ed4c48eb91d07c2e447b445e2491ef17e9b326d43a60022297fd56af4749e772" "c614d2423075491e6b7f38a4b7ea1c68f31764b9b815e35c9741e9490119efc0" "aea30125ef2e48831f46695418677b9d676c3babf43959c8e978c0ad672a7329" "196df8815910c1a3422b5f7c1f45a72edfa851f6a1d672b7b727d9551bb7c7ba" "1025e775a6d93981454680ddef169b6c51cc14cea8cb02d1872f9d3ce7a1da66" "8e51e44e5b079b2862335fcc5ff0f1e761dc595c7ccdb8398094fb8e088b2d50" "c2efd2e2e96b052dd91940b100d86885337a37be1245167642451cf6da5b924a" "65f35d1e0d0858947f854dc898bfd830e832189d5555e875705a939836b53054" "819d24b9aba8fcb446aecfb59f87d1817a6d3eb07de7fdec67743ef32194438b" "f5f3a6fb685fe5e1587bafd07db3bf25a0655f3ddc579ed9d331b6b19827ea46" "ffac21ab88a0f4603969a24b96993bd73a13fe0989db7ed76d94c305891fad64" "45a8b89e995faa5c69aa79920acff5d7cb14978fbf140cdd53621b09d782edcf" "41eb3fe4c6b80c7ad156a8c52e9dd6093e8856c7bbf2b92cc3a4108ceb385087" "fc7fd2530b82a722ceb5b211f9e732d15ad41d5306c011253a0ba43aaf93dccc" "3e34e9bf818cf6301fcabae2005bba8e61b1caba97d95509c8da78cff5f2ec8e" "cabc32838ccceea97404f6fcb7ce791c6e38491fd19baa0fcfb336dcc5f6e23c" "1d079355c721b517fdc9891f0fda927fe3f87288f2e6cc3b8566655a64ca5453" "d1af5ef9b24d25f50f00d455bd51c1d586ede1949c5d2863bef763c60ddf703a" "16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "667e02142a56f63861288cc585bee81643ded48a19e36bfdf02b66d745bcc626" default))
 '(ede-project-directories '("/home/jaymorgan/workspace/cristallo/energy-estimation"))
 '(org-agenda-files '("~/Dropbox/Notes/tasks.org"))
 '(package-selected-packages
   '(web-mode html-mode docker mu4e-alert doom-modeline julia-repl quelpa-use-package fzf org-latex focus ace-window lsp-julia quelpa atom-one-dark-theme one-dark-theme php-mode org-ref ox-gfm ox-pandoc ox-md esqlite calibre-mode olivetti use-package-ensure-system-package helm-ag pdf-tools blacken black which-key slime projectile powerline markdown-mode magit linum-relative julia-mode imenu-list hydra htmlize helm git-gutter eyebrowse evil-collection disable-mouse diminish base14-theme adaptive-wrap))
 '(powerline-display-hud t)
 '(send-mail-function 'smtpmail-send-it)
 '(undo-tree-visualizer-diff t)
 '(vterm-kill-buffer-on-exit t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; (evil-mode 1)
;; (helm-mode 1)
;; (projectile-mode 1)
;; (eyebrowse-mode 1)
;; (which-key-mode)

;; ===============================================================
;; Keyboard Shortcuts
;; ===============================================================

(require 'hydra)
(require 'evil)
(require 'ace-window)
(define-key evil-motion-state-map " " nil)
(global-set-key (kbd "M-x") 'helm-M-x)


(defun ml/bash ()
  "start a (or connect to existing) terminal emulator in a new window"
  (interactive)
  (split-window-below)
  (other-window 1)
  (if (get-buffer "vterm")
      (switch-to-buffer "vterm")
    (vterm)))

(defmacro bind-evil-key (binding func)
  `(define-key evil-motion-state-map (kbd ,binding) (quote ,func)))

(defhydra hydra-helm-files (:color blue :hint nil)
  "Helm Files"
  ("f" helm-find-files "Find Files")
  ("r" helm-recentf "File Recent Files"))
(bind-evil-key "SPC f" hydra-helm-files/body)

(defhydra hydra-helm (:color blue :hint nil)
  "Helm"
  ("r" helm-regexp "Regex")
  ("i" helm-imenu "Imenu")
  ("f" helm-find "Find")
  ("g" helm-do-ag "AG Search"))
(bind-evil-key "SPC h" hydra-helm/body)

(bind-evil-key "SPC p" projectile-command-map)
(bind-evil-key "SPC g" magit-status)
(bind-evil-key "SPC a" org-agenda)
(bind-evil-key "SPC w" ace-window)

(bind-evil-key "SPC s v"
               (lambda ()
                 (interactive)
                 (let ((p-name (projectile-project-name)))
                   (evil-window-vsplit)
                   (other-window 1)
                   (if p-name
                       (helm-projectile-find-file)
                     (switch-to-buffer "*scratch*")))))
(bind-evil-key "SPC s h"
               (lambda ()
                 (interactive)
                 (let ((p-name (projectile-project-name)))
                   (evil-window-split)
                   (other-window 1)
                   ;; activate projectile-project if original source
                   ;; was in one.
                   (if p-name
                       (helm-projectile-find-file)
                     (switch-to-buffer "*scratch*")))))


(defhydra hydra-eyebrowse (:color blue :hint nil)
  "Workspaces"
  ("s" eyebrowse-switch-to-window-config "Show workspaces")
  ("1" eyebrowse-switch-to-window-config-1 "Workspace 1")
  ("2" eyebrowse-switch-to-window-config-2 "Workspace 2")
  ("3" eyebrowse-switch-to-window-config-3 "Workspace 3")
  ("4" eyebrowse-switch-to-window-config-4 "Workspace 4")
  ("5" eyebrowse-switch-to-window-config-5 "Workspace 5")
  ("6" eyebrowse-switch-to-window-config-6 "Workspace 6")
  ("7" eyebrowse-switch-to-window-config-7 "Workspace 7")
  ("8" eyebrowse-switch-to-window-config-8 "Workspace 8")
  ("9" eyebrowse-switch-to-window-config-9 "Workspace 9"))
(define-key evil-motion-state-map
  (kbd "SPC TAB") 'hydra-eyebrowse/body)

(define-key evil-motion-state-map
  (kbd "SPC SPC") 'helm-buffers-list)

(defhydra hydra-openbuffer (:color blue :hint nil)
  "Open Buffer"
  ("s" ml/bash "Shell")
  ("S" vterm "Big Shell")
  ("d" (dired-at-point ".") "Dired")
  ("D" (progn (split-window-sensibly) (dired-at-point ".")) "Dired in another window")
  ("c" (find-file "~/.emacs.d/init.el") "Open Emacs Config")
  ("t" (find-file "~/Dropbox/Notes/tasks.org") "Open tasks")
  ("i" imenu-list-smart-toggle "Open Menu Buffer")
  ("u" undo-tree-visualize "Undo-tree")
  ("m" mu4e "Open Mailbox"))
(define-key evil-motion-state-map
  (kbd "SPC o") 'hydra-openbuffer/body)

(defhydra hydra-insert (:color blue :hint nil)
  "Insert into Buffer"
  ("s" yas-insert-snippet "Insert Snippet"))
(define-key evil-motion-state-map
  (kbd "SPC i") 'hydra-insert/body)

(defhydra hydra-remote-hosts (:color blue :hint nil)
  "Browse remote hosts"
  ("l" (dired-at-point "/ssh:lis.me:~/workspace") "LIS Lab")
  ("s" (dired-at-point "/ssh:sunbird.me:~/workspace") "Sunbird Swansea")
  ("i" (dired-at-point "/ssh:ibex.me:~") "KAUST Ibex")
  ("c" (dired-at-point "/ssh:chemistry.me:~/workspace") "Chemistry Swanasea"))
(define-key evil-motion-state-map
  (kbd "SPC r") 'hydra-remote-hosts/body)

(defhydra hydra-modify-buffers (:color blue :hint nil)
  "Modify buffer"
  ("w" (write-file (buffer-file-name)) "Write")
  ("o" olivetti-mode "Olivetti Mode")
  ("b" ibuffer "Edit Buffers")
  ("q" (kill-buffer-and-window) "Close"))
(define-key evil-motion-state-map
  (kbd "SPC m") 'hydra-modify-buffers/body)

;; Disable mouse!!
;; While it may be nice to use the mouse, I find it more preferable to
;; use emacs as a 'cmd-line' application, rather than graphical
;; point-and-click. I use disable-mouse package to disable all mouse
;; operations in evil mode.
(global-disable-mouse-mode)
(mapc #'disable-mouse-in-keymap
  (list evil-motion-state-map
        evil-normal-state-map
        evil-visual-state-map
        evil-insert-state-map))

;; ===============================================================
;; Display themes
;; ===============================================================

;; Display line numbers
(global-linum-mode)
(linum-relative-on)
(setq completion-auto-help t)
(global-auto-revert-mode t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun my/disable-line-numbers-in-term ()
  "Disable the line numbers when entering the terminal"
  (add-hook 'after-change-major-mode-hook
            (lambda () (linum-mode 0))
            :append :local))
(add-hook 'term-mode-hook 'my/disable-line-numbers-in-term)

(load-theme 'base16-espresso)
(set-frame-font "Roboto Mono-10.5")
(setq default-frame-alist '((font . "Roboto Mono-10.5")))

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(setq-default indent-tabs-mode nil)
(setq tab-stop 4)

;; Remove line continue character
(setf (cdr (assq 'continuation fringe-indicator-alist))
      '(nil nil) ;; no continuation indicators
      ;; '(nil right-curly-arrow) ;; right indicator only
      ;; '(left-curly-arrow nil) ;; left indicator only
      ;; '(left-curly-arrow right-curly-arrow) ;; default
      )

;; Suppress the splash screen
(setq-default inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq auto-save-default nil)
(setq backup-directory-alist '(("" . "~/.Trash")))
(put 'dired-find-alternate-file 'disabled nil)
(setq confirm-kill-processes nil)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq revert-without-query 1)

;; Close the compilation window if there was no error at all.
(setq compilation-exit-message-function
    (lambda (status code msg)
        ;; If M-x compile exists with a 0
        (when (and (eq status 'exit) (zerop code))
        ;; then bury the *compilation* buffer, so that C-x b doesn't go there
        (bury-buffer "*compilation*")
        ;; and return to whatever were looking at before
        (replace-buffer-in-windows "*compilation*"))
        ;; Always return the anticipated result of compilation-exit-message-function
    (cons msg code)))

(recentf-mode 1)
(setq recentf-max-menu 50
      recentf-max-saved-items 50)
(global-prettify-symbols-mode +1)

;; Remove the GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
