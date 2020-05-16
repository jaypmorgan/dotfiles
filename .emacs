;;--------------------------
; EMACS Configuration File
;; Author: Jay Morgan
;;--------------------------

(require 'cl)
(add-to-list 'load-path "~/.emacs.d/plugins/")
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e/")
(require 'mu4e)

;; Setup package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-refresh-contents)
(package-initialize)

;; Install function
;; define a function to check if a package is installed, if it not we can install it. From this, we may quickly and easily install packages.
(defun my/check-and-install (pkg)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; List of packages to be installed
;; Instead of writing many lines of `check-and-install', we will define a list of packages to install, then loop through the list, calling the function for each element in this list. To install a new package (or just add it to the base installation), add the package to this list.
(setq local-packages '(evil
                       helm
                       powerline
                       disable-mouse
                       projectile
                       julia-mode
                       ispell
                       markdown-mode
                       magit
                       hydra
                       eyebrowse
                       imenu-list
                       linum-relative
                       diminish
                       slime
                       adaptive-wrap
                       htmlize
                       git-gutter
                       evil-collection
                       base16-theme))

;; Iterate through the list of packages to be installed and call the check-and-install function for each package.
(dolist (pkg local-packages) (my/check-and-install pkg))
;; Require packages -- package imports
(dolist (pkg local-packages) (require pkg))

;; ===========================================================================================================

;; Enable Packages & Config
;;-------------------
(setq x-wait-for-event-timeout nil)
(evil-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("669e02142a56f63861288cc585bee81643ded48a19e36bfdf02b66d745bcc626" default)))
 '(org-agenda-files (quote ("~/Dropbox/Notes/tasks.org")))
 '(package-selected-packages
   (quote
    ()))
 '(powerline-display-hud t)
 '(send-mail-function (quote smtpmail-send-it)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(helm-mode 1)
(projectile-mode 1)
(eyebrowse-mode 1)

;; spelling
(setq ispell-dictionary "british")

;; Lisp configuration
(setq inferior-lisp-program "sbcl")
(org-babel-do-load-languages
 'org-babel-load-languages
 '((gnuplot . t) (dot . t)))

;; Org Mode
(with-eval-after-load 'org
  (setq org-startup-indented t)
  (add-hook 'org-mode-hook #'visual-line-mode)
  (add-hook 'org-mode-hook '(lambda () (set-fill-column 80)))
  (add-hook 'org-mode-hook #'auto-fill-mode))
(global-auto-revert-mode t)

;; mu4e Config
;;---------------
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

;; =============================================================================================================

;; Keyboard Shortcuts
;;--------------------
;; Terminal
(define-key evil-motion-state-map " " nil)

(defun ml/bash ()
  "start a terminal emulator in a new window"
  (interactive)
  (split-window-sensibly)
  (other-window 1)
  (ansi-term "/bin/bash"))
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
  ("9" eyebrowse-switch-to-window-config-9 "Workspace 9"))
(define-key evil-motion-state-map
  (kbd "SPC TAB") 'hydra-eyebrowse/body)

;; Buffer management
(define-key evil-motion-state-map
  (kbd "SPC b") 'switch-to-buffer)

;; Hydra Open Window
(defhydra hydra-openbuffer (:color blue :hint nil)
  "Open Buffer"
  ("s" ml/bash "Shell Terminal")
  ("c" (find-file "~/.emacs") "Open Emacs Config")
  ("t" (find-file "~/Dropbox/Notes/tasks.org") "Open tasks")
  ("i" imenu-list-smart-toggle "Open Menu Buffer")
  ("m" mu4e "Open Mailbox"))
(define-key evil-motion-state-map
  (kbd "SPC o") 'hydra-openbuffer/body)

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
  ("c" (dired-at-point "/ssh:chemistry.me:~") "Chemistry Swanasea"))
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

(set-default-font "Tamsyn-11")
(setq default-frame-alist '((font . "Tamsyn-11")))
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

; Whitespace
(whitespace-mode -1)
(setq whiteline-style '(face spaces tabs space-mark tab-mark))
(whitespace-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq revert-without-query 1)
