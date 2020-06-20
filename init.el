;;--------------------------
; EMACS Configuration File
;; Author: Jay Morgan
;;--------------------------

(add-to-list 'load-path "~/.emacs.d/plugins/")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e/")
(require 'mu4e)

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


;; ===========================================================================================================

(use-package evil
  :config
  (evil-mode 1))

(use-package which-key
  :config
  (which-key-mode 1))

(setq use-package-always-ensure t)
(use-package evil-collection
  :after (evil)
  :config
  (evil-collection-init))

(use-package company
  :init
  (global-company-mode)
  (setq company-idle-delay 0.001))

(use-package company-anaconda
  :after (company anaconda-mode)
  :init
  (add-to-list 'company-backends '(company-anaconda))
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  (add-hook 'python-mode-hook (lambda () (setq-local company-idle-delay 1)))

(use-package pyenv-mode
  :after (company-anaconda)
  :functions projectile-pyenv-mode-set
  :init
  (let ((pyenv-path (expand-file-name "~/.pyenv/bin")))
    (setenv "PATH" (concat pyenv-path ":" (getenv "PATH")))
    (add-to-list 'exec-path pyenv-path))
  :config
  (pyenv-mode)
  (add-hook 'projectile-after-switch-project-hook 'projectile-pyenv-mode-set)
  (defun projectile-pyenv-mode-set ()
    "Set pyenv version matching project name."
    (let ((project (projectile-project-name)))
      (if (member project (pyenv-mode-versions))
          (pyenv-mode-set project)
        (pyenv-mode-unset)))))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package projectile)
(use-package anaconda-mode)
(use-package blacken)
(use-package itail)
(use-package julia-mode)
(use-package clojure-mode)
(use-package markdown-mode)
(use-package ranger)
(use-package magit)
(use-package powerline)
(use-package disable-mouse)
(use-package hydra)
(use-package imenu-list)
(use-package linum-relative)
(use-package diminish)
(use-package slime)
(use-package htmlize)
(use-package base16-theme)
(use-package helm-projectile)
(use-package cider)
(use-package org
  :after (cider)
  :ensure org-plus-contrib
  :init
  (require 'ob-clojure)
  (require 'cider)
  (setq inferior-julia-program-name "/usr/bin/julia")
  (setq org-confirm-babel-evaluate nil)
  (setq org-babel-clojure-backend 'cider)
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((shell . t)
                                 (emacs-lisp . t)
                                 (julia . t))))
(use-package ob-async)
(use-package yasnippet
  :init
  (yas-reload-all)
  (add-hook 'org-mode-hook #'yas-minor-mode))
(use-package yasnippet-snippets
  :after (yasnippet))

(use-package paredit
  :init
  (add-hook 'clojure-mode-hook #'enable-paredit-mode))

(use-package pdf-tools
  :init
  (pdf-loader-install)
  (add-hook 'pdf-view-mode-hook (lambda () (linum-mode -1))))

(use-package flycheck
  :init
  (add-hook 'python-mode-hook 'flycheck-mode)
  (setq flycheck-check-syntax-automatically '(mode-enabled save)
        flycheck-highlighting-mode 'lines
        flycheck-indication-mode 'left-fringe
        flycheck-checker-error-threshold 200
        python-interp "~/miniconda3/bin/python"
        flycheck-python-flake8-executable python-interp))

(use-package eyebrowse
  :config
  (setq eyebrowse-new-workspace t))

(use-package helm
  :init
  (require 'helm-config)
  :config
  (helm-mode 1)
  (setq helm-use-frame-when-more-than-two-windows nil
        helm-split-window-in-side nil
        helm-display-function 'pop-to-buffer
        helm-idle-delay 0.0
        helm-input-idle-delay 0.01))

(use-package shackle
  :after (helm)
  (shackle-mode)
  :config
  (setq shackle-rules '((compilation-mode :noselect t))
        shackle-default-rule '(:align 'below :size 0.3)))

(use-package vterm
  :commands (vterm vterm-other-window)
  :init
  (add-hook 'vterm-exit-hook (lambda ()
                               (let ((buffer (get-buffer))
                                     (window (get-buffer-window)))
                                 (when window
                                   (delete-window window))
                                 (kill-buffer buffer))))
  :custom (vterm-kill-buffer-on-exit t))

;; Enable Packages & Config
;;-------------------
(setq which-key-idle-delay 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("669e02142a56f63861288cc585bee81643ded48a19e36bfdf02b66d745bcc626" default))
 '(org-agenda-files '("~/Dropbox/Notes/tasks.org"))
 '(package-selected-packages
   '(pdf-tools blacken black which-key slime projectile powerline markdown-mode magit linum-relative julia-mode imenu-list hydra htmlize helm git-gutter eyebrowse evil-collection disable-mouse diminish base16-theme adaptive-wrap))
 '(powerline-display-hud t)
 '(send-mail-function 'smtpmail-send-it)
 '(vterm-kill-buffer-on-exit t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(projectile-mode 1)
(eyebrowse-mode 1)
(which-key-mode)
;; spelling
(setq ispell-dictionary "british")

;; ;; Lisp configuration
;; (setq inferior-lisp-program "sbcl")
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((gnuplot . t) (dot . t)))

;; Org Mode
(with-eval-after-load 'org
  (setq org-startup-indented t)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook '(lambda () (set-fill-column 80)))
  (add-hook 'org-mode-hook #'auto-fill-mode))
(global-auto-revert-mode t)

;; mu4e Config
;;---------------
(setq mu4e-confirm-quit nil)
(setq
 mu4e-sent-folder   "/envoyée"
 mu4e-drafts-folder "/brouillon"
 mu4e-trash-folder  "/trash"
 mu4e-refile-folder "/archiver"
 mu4e-get-mail-command "offlineimap -o -u quiet")
(setq
 user-full-name     "Jay Morgan"
 user-mail-address "jaymiles17@gmail.com")

;; Julia Markdown
(add-to-list 'auto-mode-alist '("\\.jmd\\'" . markdown-mode))

(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq projectile-git-submodule-command nil)
(setq completion-auto-help nil)

;; =============================================================================================================

;; Keyboard Shortcuts
;;--------------------
;; Terminal
(define-key evil-motion-state-map " " nil)

(defun ml/bash ()
  "start a terminal emulator in a new window"
  (interactive)
  (split-window-below 55)
  (other-window 1)
  (if (get-buffer "vterm")
      (switch-to-buffer "vterm")
    (vterm)))

;; Helm shortcuts
(defhydra hydra-helm (:color blue :hint nil)
  "Helm Files"
  ("f" helm-find-files "Find Files")
  ("r" helm-recentf "File Recent Files"))
(define-key evil-motion-state-map (kbd "SPC f") 'hydra-helm/body)
(global-set-key (kbd "M-x") 'helm-M-x)

(define-key evil-motion-state-map (kbd "SPC p") 'projectile-command-map)
(define-key evil-motion-state-map (kbd "SPC d") 'dired)
(define-key evil-motion-state-map (kbd "SPC g") 'magit-status)
(define-key evil-motion-state-map (kbd "SPC a") 'org-agenda)


;; Eyebrowse
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

;; Buffer management
(define-key evil-motion-state-map
  (kbd "SPC SPC") 'helm-buffers-list)

;; Hydra Open Window
(defhydra hydra-openbuffer (:color blue :hint nil)
  "Open Buffer"
  ("s" ml/bash "Shell Terminal")
  ("d" (dired-directory ".") "Dired")
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

;; Multi-cursors
(defhydra hydra-multipleCursors (:color blue :hint nil)
  "Multiple Cursors"
  ("e" mc/edit-lines "Edit Lines"))
(define-key evil-motion-state-map
  (kbd "SPC e") 'hydra-multipleCursors/body)

;; Remote hosts
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
  ("w" write-file "Write")
  ("q" quit-window "Close"))
(define-key evil-motion-state-map
  (kbd "SPC m") 'hydra-modify-buffers/body)

;; Disable mouse!!
;; While it may be nice to use the mouse, I find it more preferable to use emacs as a 'cmd-line' application, rather than graphical point-and-click. I use disable-mouse package to disable all mouse operations in evil mode.
(global-disable-mouse-mode)
(mapc #'disable-mouse-in-keymap
  (list evil-motion-state-map
        evil-normal-state-map
        evil-visual-state-map
        evil-insert-state-map))

(defhydra hydra-project-map (:color blue :hint nil)
  "
  ^Execute^       ^Remote Actions^
  ^^^^^^^^^^-------------------------
  _r_: run shell  _u_: upload
"
  ("r" ignore)
  ("u" ignore))
(define-key evil-motion-state-map
  (kbd "<f5>") 'hydra-project-map/body)


(setq projectile-project-rsyncs
      '(("cristallo" . "chemistry.me:~/workspace/cristallo")
        ("ogd" . "lis.me:~/workspace/ogd/classifier")))
(setq projectile-project-virtualenvs
      '(("cristallo" . "~/miniconda3/envs/cristallo")
        ("ogd" . "~/miniconda3/envs/ema")))

(defun rsync-project (dir location)
  (interactive)
  (start-process-shell-command "" nil "dorsync" dir location)))


(rsync-project "~/workspace/cristallo/energy-estimation/src" "chemistry.me:~/workspace/cristallo/src")


;; =========================================================================================================

;; Display themes
;;---------------------
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-hl-line-mode 1)
(load-theme 'base16-default-dark 1)
(powerline-default-theme)
;; Set the cursor color based on the evil state
(defvar my/base16-colors base16-default-dark-colors)
(setq evil-emacs-state-cursor   `(,(plist-get my/base16-colors :base0D) box)
      evil-insert-state-cursor  `(,(plist-get my/base16-colors :base0D) bar)
      evil-motion-state-cursor  `(,(plist-get my/base16-colors :base0E) box)
      evil-normal-state-cursor  `(,(plist-get my/base16-colors :base0B) box)
      evil-replace-state-cursor `(,(plist-get my/base16-colors :base08) bar)
      evil-visual-state-cursor  `(,(plist-get my/base16-colors :base09) box))

(set-frame-font "JetBrains Mono-10")
(setq default-frame-alist '((font . "JetBrains Mono-10")))
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

;; Display line numbers
(global-linum-mode)
(linum-relative-on)
(add-hook 'term-mode-hook
    (lambda () (linum-relative-toggle)))

;; Suppress the splash screen
(setq-default inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq auto-save-default nil)
(setq backup-directory-alist '(("" . "~/.Trash"))) ; put backup files into the trash bin -- still there but not in working dir
(put 'dired-find-alternate-file 'disabled nil)

(setq org-todo-keywords '((sequence "TODO(t)" "WAIT(w)" "|" "DONE(d)")))

(defun hide-minor-modes ()
  "Who wants to be reminded of active modes"
  (interactive)
  (mapc (lambda (mode) (diminish mode))
        minor-mode-list))
(hide-minor-modes)

(scroll-bar-mode -1)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq revert-without-query 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("669e02142a56f63861288cc585bee81643ded48a19e36bfdf02b66d745bcc626" default))
 '(org-agenda-files '("~/Dropbox/Notes/tasks.org"))
 '(package-selected-packages
   '(ob-julia ob-async ob-clojure org-plus-contrib org-mode yasnippet-snippets yasnippet cider paredit pdf-tools blacken black which-key slime projectile powerline markdown-mode magit linum-relative julia-mode imenu-list hydra htmlize helm git-gutter eyebrowse evil-collection disable-mouse diminish base16-theme adaptive-wrap))
 '(powerline-display-hud t)
 '(send-mail-function 'smtpmail-send-it)
 '(vterm-kill-buffer-on-exit t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
