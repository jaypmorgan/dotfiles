;;--------------------------
;; EMACS Configuration File
;; Author: Jay Morgan
;;--------------------------

;; Setup package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
        '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives 
        '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives 
        '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)

;; Install function
;; define a function to check if a package is installed, if it not we can install it. From this, we may quickly and easily install packages.
(defun check-and-install (pkg)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; List of packages to be installed
;; Instead of writing many lines of `check-and-install', we will define a list of packages to install, then loop through the list, calling the function for each element in this list. To install a new package (or just add it to the base installation), add the package to this list.
(setq local-packages '(evil helm powerline atom-one-dark-theme disable-mouse projectile auto-complete epc jedi julia-mode which-key ispell markdown-mode magit hydra eyebrowse company))

;; Iterate through the list of packages to be installed and call the check-and-install function for each package.
(dolist (pkg local-packages)
  (check-and-install pkg))
;; Require packages -- package imports
(dolist (pkg local-packages)
  (require pkg))

;; Enable Packages
;;-------------------
(evil-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files (quote ("~/Dropbox/Notes/tasks.org")))
 '(package-selected-packages
   (quote
    (company company-mode markdown-mode powerline helm evil)))
 '(powerline-display-hud nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(helm-mode 1)
(projectile-mode 1)
(eyebrowse-mode 1)
(which-key-mode 1)
(add-hook 'after-init-hook 'global-company-mode)
(which-key-setup-side-window-bottom)

;; LaTeX spelling
(add-hook 'LaTex-mode-hook 'flyspell-mode)
(setq ispell-dictionary "british")
(add-hook 'LaTeX-mode-hook 'spell)

;; Keyboard Shortcuts
;;--------------------
;; Terminal
;; I prefer the ansi-terminal rather than `shell'. Ansi-term, instead of shell, allows for normal history cycling out-of-the-box. Moreover, we can define a function to use ansi-term much like how shell operates.
(define-key evil-motion-state-map " " nil)

(defun ml/bash ()
  "start a terminal emulator in a new window"
  (interactive)
  (split-window-sensibly)
  (other-window 1)
  (ansi-term (executable-find "bash")))
(define-key evil-motion-state-map (kbd "SPC t") #'ml/bash)

;; Helm shortcuts
(defhydra hydra-helm (:color blue :hint nil)
  "Helm Files"
  ("f" helm-find-files "Find Files")
  ("r" helm-recentf "File Recent Files"))
(define-key evil-motion-state-map (kbd "SPC f") 'hydra-helm/body)
(global-set-key (kbd "M-x") 'helm-M-x)

;; Projectile
(define-key evil-motion-state-map (kbd "SPC p") 'projectile-command-map)

;; Dired
(define-key evil-motion-state-map (kbd "SPC d") 'dired)

;; Magit
(define-key evil-motion-state-map (kbd "SPC g") 'magit-status)

;; Org-mode
(define-key evil-motion-state-map (kbd "SPC a") 'org-agenda)

;; Eyebrowse
(defhydra hydra-eyebrowse (:color blue :hint nil)
  "Workspaces"
  ("s" eyebrowse-switch-to-window-config "Show workspaces")
  ("0" eyebrowse-switch-to-window-config-0 "Workspace 0")
  ("1" eyebrowse-switch-to-window-config-1 "Workspace 1")
  ("2" eyebrowse-switch-to-window-config-2 "Workspace 2")
  ("3" eyebrowse-switch-to-window-config-3 "Workspace 3")
  ("4" eyebrowse-switch-to-window-config-4 "Workspace 4")
  ("5" eyebrowse-switch-to-window-config-5 "Workspace 5")
  ("6" eyebrowse-switch-to-window-config-6 "Workspace 6")
  ("7" eyebrowse-switch-to-window-config-7 "Workspace 7")
  ("8" eyebrowse-switch-to-window-config-8 "Workspace 8")
  ("9" eyebrowse-switch-to-window-config-9 "Workspace 9")
  )
(define-key evil-motion-state-map (kbd "SPC TAB") 'hydra-eyebrowse/body)

;; Buffer management
(define-key evil-motion-state-map (kbd "SPC b") 'switch-to-buffer)


;; Disable mouse!!
;; While it may be nice to use the mouse, I find it more preferable to use emacs as a 'cmd-line' application, rather than graphical point-and-click. I use disable-mouse package to disable all mouse operations in evil mode.
(global-disable-mouse-mode)
(mapc #'disable-mouse-in-keymap
  (list evil-motion-state-map
        evil-normal-state-map
        evil-visual-state-map
        evil-insert-state-map))

;; Display themes
;;---------------------
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-hl-line-mode 1)
(load-theme 'atom-one-dark t)
(powerline-default-theme)

(set-default-font "Source Code Pro-10")
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

(set-window-margins (selected-window) 1 1)

;; Remove line continue character
(setf (cdr (assq 'continuation fringe-indicator-alist))
      '(nil nil) ;; no continuation indicators
      ;; '(nil right-curly-arrow) ;; right indicator only
      ;; '(left-curly-arrow nil) ;; left indicator only
      ;; '(left-curly-arrow right-curly-arrow) ;; default
      )

;; Display line numbers
(global-linum-mode 1)
(setq display-line-numbers 'relative)

;; Suppress the splash screen
(setq-default inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
(setq backup-directory-alist '((".*" . "~/.Trash"))) ; put backup files into the trash bin -- still there but not in working dir
