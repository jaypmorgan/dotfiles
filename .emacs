(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)


;; Setup package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Dowmload Evil
(unless (package-installed-p 'evil)
  (package-install 'evil))

;; Enable Evil
(require 'evil)
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

;; Download Helm
(unless (package-installed-p 'helm)
  (package-install 'helm))

;; Terminal
(defun ml/bash ()
  "start a terminal emulator in a new window"
  (interactive)
  (split-window-sensibly)
  (other-window 1)
  (ansi-term (executable-find "bash")))
(global-set-key (kbd "C-c t") #'ml/bash)

;; Enable Helm
(require 'helm)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; Org-mode
(require 'org)

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

;; Install dev environment packages
(defvar local-packages '(projectile auto-complete epc jedi))
;; filter the list to find uninstalled packages
;; install only those that are not installed

(unless (package-installed-p 'disable-mouse)
  (package-install 'disable-mouse))
(global-disable-mouse-mode)
(mapc #'disable-mouse-in-keymap
  (list evil-motion-state-map
        evil-normal-state-map
        evil-visual-state-map
        evil-insert-state-map))

;; Display themes
(unless (package-installed-p 'atom-one-dark-theme)
  (package-install 'atom-one-dark-theme))
(load-theme 'atom-one-dark t)

(unless (package-installed-p 'powerline)
  (package-install 'powerline))
(powerline-default-theme)

(set-default-font "Source Code Pro")
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
