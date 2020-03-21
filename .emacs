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
(defun check-and-install (p)
  (unless (package-installed-p p)
    (package-install p)))

;; List of packages to be installed
(setq local-packages '(evil helm powerline atom-one-dark-theme disable-mouse projectile auto-complete epc jedi))

;; Check if package has been installed
;; and if not, install it
(dolist (pkg local-packages)
  (check-and-install pkg))

;; Require packages -- package imports
;;-------------------------------------
(require 'evil)
(require 'helm)
(require 'org)

;; Enable Pacakges
;;-------------------
(evil-mode 1)
(custom-set-variables '(package-selected-packages (quote (powerline helm evil))))
(custom-set-faces)
(helm-mode 1)

;; Keyboard Shortcuts
;;--------------------
;; Terminal
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

;; Disable mouse!!
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

(load-theme 'atom-one-dark t)
(powerline-default-theme)

(set-default-font "Source Code Pro")
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
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
