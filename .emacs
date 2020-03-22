;;--------------------------
;; EMACS Configuration File
;; Author: Jay Morgan
;;--------------------------

;; Setup package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Install function
;; define a function to check if a package is installed, if it not we can install it. From this, we may quickly and easily install packages.
(defun check-and-install (pkg)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; List of packages to be installed
;; Instead of writing many lines of `check-and-install', we will define a list of packages to install, then loop through the list, calling the function for each element in this list. To install a new package (or just add it to the base installation), add the package to this list.
(setq local-packages '(evil helm powerline atom-one-dark-theme disable-mouse projectile auto-complete epc jedi julia-mode which-key))

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
 '(package-selected-packages (quote (powerline helm evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(helm-mode 1)
(projectile-mode 1)
(which-key-mode 1)
(which-key-setup-side-window-bottom)

;; Keyboard Shortcuts
;;--------------------
;; Terminal
;; I prefer the ansi-terminal rather than `shell'. Ansi-term, instead of shell, allows for normal history cycling out-of-the-box. Moreover, we can define a function to use ansi-term much like how shell operates.
(defun ml/bash ()
  "start a terminal emulator in a new window"
  (interactive)
  (split-window-sensibly)
  (other-window 1)
  (ansi-term (executable-find "bash")))
(global-set-key (kbd "C-c t") #'ml/bash)

;; Helm shortcuts
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; Projectile
(global-set-key (kbd "C-c p") 'projectile-command-map)

;; Dired
(global-set-key (kbd "C-c d") 'dired)

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

(set-default-font "Tamsyn-11")
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
