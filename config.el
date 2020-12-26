(defun my/add-to-exec (new-path)
  " Add the new-path (dir) to the PATH variable "
  (let ((new-path (expand-file-name new-path)))
    (setq exec-path (push new-path exec-path))
    (setenv "PATH" (format "%s:%s" (getenv "PATH") new-path))))

(my/add-to-exec "~/miniconda3/bin")
(my/add-to-exec "~/.fzf/bin")
(my/add-to-exec "~/.cargo/bin")

(setq gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024)
      evil-want-keybinding nil
      x-wait-for-event-timeout nil
      tramp-ssh-controlmaster-options ""
      tramp-default-method "ssh")

;; Manually installed plugins/packages
(add-to-list 'load-path (expand-file-name "~/.emacs.d/plugins/"))

;; Setup package.el to work with MELPA
(setq package-check-signature nil)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-refresh-contents)
(package-initialize)

;; Install function define a function to check if a package is
;; installed, if it not we can install it. From this, we may quickly
;; and easily install packages.
(defun my/check-and-install (pkg)
  (unless (package-installed-p pkg)
    (package-install pkg))
  (require pkg))

(my/check-and-install 'use-package)
;; Don't need a :ensure t in every package
(setq use-package-always-ensure t)
;; Makes it possible to install required binaries
(use-package use-package-ensure-system-package)

(use-package quelpa)
(quelpa
 '(quelpa-use-package
   :fetcher git
   :url "https://github.com/quelpa/quelpa-use-package.git"))
(require 'quelpa-use-package)

(use-package flycheck
  :init
  (add-hook 'sh-mode-hook 'flycheck-mode))
(use-package clojure-mode :init (use-package cider))
(use-package markdown-mode)
(use-package htmlize)
(use-package toml-mode)
(use-package haskell-mode)
(use-package ess) ;; Emacs speaks statistics (R)

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  (add-hook 'c++-mode-hook 'eglot-ensure))

(use-package python-mode
    :config
    (setq python-shell-interpreter "ipython"
          python-shell-interpreter-args "-i")
    (use-package anaconda-mode)
    (use-package blacken
    :init
    (defun blacken-python-hook ()
        (when (eq major-mode 'python-mode)
        (blacken-buffer)))
    (add-hook 'before-save-hook 'blacken-python-hook))
    ;; removed as lsp-mode handles linting in python
    ;; (add-hook 'python-mode-hook (lambda ()
    ;;                                (setq flycheck-pylintrc "/home/jaymorgan/.pylintrc")
    ;;                                (setq flycheck-check-syntax-automatically '(mode-enabled save))
    ;;                                (flycheck-mode)))
    (use-package conda
        :init
        (conda-env-initialize-eshell)
        (setq conda-anaconda-home (expand-file-name "~/miniconda3/")
              conda-env-home-directory (expand-file-name "~/miniconda3/"))))

(use-package julia-mode
    :init
    (add-to-list 'auto-mode-alist '("\\.jmd\\'" . markdown-mode))
    (use-package julia-repl
        :quelpa ((julia-repl :fetcher github :repo "tpapp/julia-repl" :branch "tp/terminal-backends") :upgrade t)
        :init
        (add-hook 'julia-mode-hook 'julia-repl-mode)
        (setenv "JULIA_NUM_THREADS" "4")
        (setq julia-repl-executable-records
            '((default "julia")
                (master "/usr/bin/julia")
                (chemistry "ssh -t chemistry.me julia")
                (lis "ssh -t lis.me ~/workspace/libs/julia/bin/julia")))))

(use-package company
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.0000001
        company-minimum-prefix-length 2
        company-candidates-cache t))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package lsp-mode
  :hook ((python-mode . lsp-deferred)
         (julia-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :init (setq lsp-keymap-prefix "C-c l")
  :config (lsp-enable-which-key-integration t))

(use-package lsp-julia
  :quelpa ((lsp-julia :fetcher github
                      :repo "non-Jedi/lsp-julia"
                      :files (:defaults "languageserver"))
           :upgrade t))

(use-package org
  :after cider
  :ensure org-plus-contrib
  :init
  (add-hook 'org-mode-hook '(lambda ()
                              (set-fill-column 85)
                              (visual-line-mode 1)
                              (auto-fill-mode 1)))
  (add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)
  (require 'ob-clojure)
  (require 'ox-latex)
  (require 'cider)
  (use-package ob-ipython)

  ;; notes/wiki/journal
  (use-package deft
    :init
    (setq deft-extensions '("txt" "tex" "org" "md")
          deft-directory "~/Dropbox/Notes/"
          deft-recursive t
         deft-use-filename-as-title t))
  (use-package org-journal
    :init
    (setq org-journal-dir "~/Dropbox/Notes/"
          org-journal-date-format "%A, %d %B %Y"
          org-journal-file-format "%Y%m%d-journal-entry.org"))
  (use-package org-roam
    :hook (after-init . org-roam-mode)
    :custom (org-roam-directory "~/Dropbox/Notes/"))

  (use-package ox-latex-subfigure
    :load-path "plugins/ox-latex-subfigure"
    :config (require 'ox-latex-subfigure))
    :init
    (setq org-latex-prefer-user-labels t)
  (use-package ox-pandoc)
  (use-package ox-gfm)
  (use-package org-ref
    :init
    (setq reftex-default-bibliography "~/Dropbox/Notes/Wiki/library.bib"
          org-ref-default-bibliography '("~/Dropbox/Notes/Wiki/library.bib"))
    (use-package ivy-bibtex
        :init
        (setq bibtex-completion-bibliography "~/Dropbox/Notes/Wiki/library.bib"
            bibtex-completion-pdf-open-function 'org-open-file)))

  ;; enable tikzpictures in latex export
  (add-to-list 'org-latex-packages-alist '("" "tikz" t))
  (eval-after-load "preview" '
    (add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzpicture}" t))

  ;; set variables
  (setq org-startup-indented t
        org-startup-folded t
        org-src-tab-acts-natively t
        org-hide-leading-stars t
        org-edit-src-content-indentation 0
        org-latex-listings 'minted   ;; color highlighting for source blocks
        org-latex-packages-alist '(("" "minted"))
        org-latex-pdf-process '( "latexmk -shell-escape -bibtex -f -pdf %f")
        ;; org-latex-pdf-process
        ;;     '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        ;;     "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")
        org-format-latex-options (plist-put org-format-latex-options :scale 1.4)
        inferior-julia-program-name "/usr/bin/julia"
        org-babel-clojure-backend 'cider
        org-confirm-babel-evaluate nil
        org-fontify-done-headline t
        org-log-done 'time
        org-todo-keywords '((type "TODO(t)" "WAIT(w)" "|" "DONE(d)" "CANC(c)"))
        org-todo-keyword-faces '(("TODO" . org-warning)
                                 ("WAIT" . "Firebrick")
                                 ("DONE" . (:forground "dim-gray" :strike-through t min-colors 16))
                                 ("CANC" . "red")))

    (add-to-list 'org-latex-classes
            '("book-no-parts"
                "\\documentclass{book}"
                ("\\chapter{%s}" . "\\chapter*{%s}")
                ("\\section{%s}" . "\\section*{%s}")
                ("\\subsection{%s}" . "\\subsection*{%s}")
                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                ("\\paragraph{%s}" . "\\paragraph*{%s}")))
  (custom-set-faces '(org-headline-done
                        ((((class color)
                        (min-colors 16))
                        (:foreground "dim gray" :strike-through t)))))

  ;; list of languages for org-mode to support
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((shell . t)
                                 (python . t)
                                 (R . t)
                                 (ipython . t)
                                 (clojure . t)
                                 (emacs-lisp . t)
                                 (julia . t)
                                 (gnuplot . t)
                                 (dot . t))))

(use-package toc-org
  :init
  (add-hook 'markdown-mode-hook 'toc-org-mode)
  (add-hook 'org-mode-hook 'toc-org-mode))

(use-package avy)
(use-package swiper)
(use-package itail)
(use-package magit)
(use-package disable-mouse)
(use-package imenu-list)
(use-package linum-relative)
(use-package ace-window)
(use-package focus)
(use-package iedit)
(use-package ripgrep)

(use-package elfeed
  :init
  (use-package elfeed-org
    :config
    (elfeed-org)
    (setq rmh-elfeed-org-files '("~/Dropbox/Notes/feeds.org"))))

(use-package undo-tree
  :init
  (setq undo-tree-visualizer-timestamps t)
  (global-undo-tree-mode))

(use-package csv-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.csv\\'" . csv-align-mode)))

(use-package yasnippet
  :init
  (use-package yasnippet-snippets
    :init
    (yas-global-mode 1))
  (yas-global-mode 1))

(use-package olivetti
  :init
  (setq olivetti-body-width 90)
  (defun set-editing-buffer ()
    (interactive)
    (linum-mode -1)
    (set-window-fringes (selected-window) 0 0)
    (hl-line-mode -1))
  (add-hook 'olivetti-mode-hook 'set-editing-buffer))

(use-package pdf-tools
  :init
  (pdf-loader-install)
  (setq auto-revert-interval 0.5)
  (add-hook 'pdf-view-mode-hook (lambda () (linum-mode -1))))

(use-package flyspell
  :init
  (setq flyspell-default-dictionary "british"))

;; Prevent Helm from taking up random windows -- makes the UI more consistent
;; and predictable.
;; (use-package shackle
;;   :after helm
;;   :init
;;   (shackle-mode 1)
;;   (setq shackle-rules '(("\\`\\*helm.*?\\*\\'" :regexp t :align t :ratio 0.3))))

(use-package evil
  :init
  (evil-mode 1))

(use-package evil-collection
  :after (evil)
  :config
  (evil-collection-init))

(use-package hydra)

(use-package which-key
  :config
  (setq which-key-idle-delay 1)
  (which-key-mode 1))

(use-package doom-modeline
  :init
  (doom-modeline-mode 1)
  (setq doom-modeline-height 10
        doom-modeline-mu4e t
        doom-modeline-icon nil
        doom-modeline-env-enable-python t))

(use-package projectile
  :config
  (projectile-mode 1)
  (setq projectile-git-submodule-command nil)
  (setq projectile-mode-line-function '(lambda () (format " Proj[%s]" (projectile-project-name))))
  (setq projectile-project-search-path '("~/workspace/")))

(use-package eyebrowse
  :config
  (eyebrowse-mode 1)
  ;; new workspaces are always empty
  (setq eyebrowse-new-workspace t))

(use-package vterm
  :commands (vterm vterm-other-window)
  :custom (vterm-kill-buffer-on-exit t)
  :init
  (add-hook 'vterm-mode-hook (lambda () (linum-mode -1)))
  (add-hook 'vterm-mode-hook (lambda () (company-mode -1)))
  (setq term-prompt-regexp "^[^#$%>\n]*$ *"))

;; (use-package helm
;;   :config
;;   (helm-mode 1)
;;   (use-package helm-projectile)
;;   (use-package helm-ag
;;     :ensure-system-package (ag . silversearcher-ag))
;;   (setq helm-use-frame-when-more-than-two-windows nil
;;         helm-split-window-in-side nil
;;         helm-display-function 'pop-to-buffer
;;         helm-idle-delay 0.0
;;         helm-input-idle-delay 0.01))

(use-package counsel
  :init
  (use-package counsel-projectile
    :init
    (counsel-projectile-mode 1)))

  (use-package ivy-posframe
    :after counsel
    :init
    (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
    (setq ivy-posframe-parameters
        '((left-fringe . 8)
          (right-fringe . 8)))
    (ivy-posframe-mode 1))

(use-package prescient
  :after counsel
  :init
  (use-package ivy-prescient :init (ivy-prescient-mode 1))
  (use-package company-prescient :init (company-prescient-mode 1)))

(require 'hydra)
(require 'evil)
(require 'ace-window)
(define-key evil-motion-state-map " " nil)
(global-set-key (kbd "M-x") 'counsel-M-x)

(defun my/queue ()
  "run slurm's squeue command. Using eshell should run it on the
   server if invoked in tramp buffer"
  (interactive)
  (eshell-command "squeue"))

(defun my/bash ()
  "start a (or connect to existing) terminal emulator in a new window"
  (interactive)
  (split-window-below)
  (other-window 1)
  (if (get-buffer "vterm")
      (progn
        (switch-to-buffer "vterm")
        (shrink-window 10))
    (vterm)))

(defvar dark-theme-p t)
(defun my/toggle-theme ()
  (interactive)
  (let ((light-theme 'modus-operandi)
        (dark-theme 'atom-one-dark))
    (if (eq dark-theme-p t)
        (progn
          (load-theme light-theme t)
          (setq dark-theme-p -1))
      (progn
        (load-theme dark-theme t)
        (setq dark-theme-p t)))))

(defmacro bind-evil-key (binding func)
  `(define-key evil-motion-state-map (kbd ,binding) (quote ,func)))

(defmacro bind-global-key (binding func)
  `(global-set-key (kbd ,binding) (quote, func)))

(with-eval-after-load 'evil-maps
  (define-key evil-normal-state-map (kbd "C-n") nil))
(bind-evil-key "C-n"
  (lambda ()
    (interactive)
    (iedit-mode)
    (iedit-restrict-current-line)))

(defhydra hydra-ivy-files (:color blue :hint nil)
  "Ivy Files"
  ("f" counsel-find-file "Find Files")
  ("r" counsel-recentf "File Recent Files")
  ("d" deft "Deft Find File")
  ("b" swiper "Find in buffer"))
(bind-evil-key "SPC f" hydra-ivy-files/body)

(bind-evil-key "SPC p" projectile-command-map)
(bind-evil-key "SPC p h" counsel-projectile-find-file)
(bind-evil-key "SPC p a" projectile-add-known-project)
(bind-evil-key "SPC g" magit-status)
(bind-evil-key "SPC a" org-agenda)
(bind-evil-key "SPC w" ace-window)
(bind-evil-key "SPC n" avy-goto-char-timer)
(bind-evil-key "SPC e" eww)
(bind-global-key "C-x ," vterm) ;; new terminal in window

(defun my/split (direction)
  (interactive)
  (let ((p-name (projectile-project-name)))
    (if (string= direction "vertical")
        (evil-window-vsplit)
      (evil-window-split))
    (other-window 1)
    (if p-name
        (projectile-find-file)
      (switch-to-buffer "*scratch*"))))

(defun my/split-vertical ()
  (interactive)
  (my/split "vertical"))
(defun my/split-horizontal ()
  (interactive)
  (my/split "horizontal"))

(bind-evil-key "SPC s v" my/split-vertical)
(bind-evil-key "SPC s h" my/split-horizontal)

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
(bind-evil-key "SPC TAB" hydra-eyebrowse/body)

(bind-evil-key "SPC SPC" ivy-switch-buffer)
(bind-global-key "C-x b" ivy-switch-buffer)

(defhydra hydra-open-config (:color blue :hint nil)
  "Open Config"
  ("e" (find-file "~/.emacs.d/config.org") "Emacs Config")
  ("x" (find-file "~/.xmonad/xmonad.hs") "Xmonad Config")
  ("m" (find-file "~/.emacs.d/mu4e-init.el") "Mail Config"))

(defhydra hydra-openbuffer (:color blue :hint nil)
  "Open Buffer"
  ("c" hydra-open-config/body "Config files")
  ("b" org-roam-buffer-toggle-display "Org-roam buffer")
  ("d" (progn (split-window-sensibly) (dired-jump)) "Dired in another window")
  ("D" (dired-jump) "Dired")
  ("e" elfeed "Elfeed")
  ("g" org-roam-graph "Open Org Roam Graph")
  ("i" imenu-list-smart-toggle "Open Menu Buffer")
  ("m" mu4e "Open Mailbox")
  ("s" my/bash "Shell")
  ("S" vterm "Big Shell")
  ("t" (find-file "~/Dropbox/Notes/tasks.org") "Open tasks")
  ("u" undo-tree-visualize "Undo-tree"))
(bind-evil-key "SPC o" hydra-openbuffer/body)

(defhydra hydra-insert (:color blue :hint nil)
  "Insert into Buffer"
  ("s" yas-insert-snippet "Insert Snippet")
  ("r" org-roam-insert "Org Roam Insert")
  ("j" org-journal-new-entry "Insert New Journal Entry"))
(bind-evil-key "SPC i" hydra-insert/body)

(defhydra hydra-remote-hosts (:color blue :hint nil)
  "Browse remote hosts"
  ("l" (dired-at-point "/ssh:lis.me:~/workspace") "LIS Lab")
  ("s" (dired-at-point "/ssh:sunbird.me:~/workspace") "Sunbird Swansea")
  ("c" (dired-at-point "/ssh:chemistry.me:~/workspace") "Chemistry Swanasea"))
(bind-evil-key "SPC r" hydra-remote-hosts/body)

(defhydra hydra-modify-buffers (:color blue :hint nil)
  "Modify buffer"
  ("w" (write-file (buffer-file-name)) "Write")
  ("o" olivetti-mode "Olivetti Mode")
  ("b" ibuffer "Edit Buffers")
  ("q" (kill-buffer-and-window) "Close"))
(bind-evil-key "SPC m" hydra-modify-buffers/body)

(defun get-stats (user host format)
  "Get SLURM status from remote server"
  (eshell-command-result
   (concat
    "cd /ssh:" host ":/ && sacct -u" user " --format=" format)))

(defun slurm-get-stats (user host format)
  "Log into SLURM server and get current running/pending jobs"
  (interactive)
  (let ((stats (get-stats user host format))
        (temp-buffer-name "*slurm-log*"))
    (display-buffer
        (get-buffer-create temp-buffer-name)
        '((display-buffer-below-selected display-buffer-at-bottom)
          (inhibit-same-window . t)
          (window-height . 20)))
    (switch-to-buffer-other-window temp-buffer-name)
    (insert stats)
    (special-mode)))

(setq slurm-host "lis.me"
      slurm-username "jay.morgan"
      slurm-job-format "jobid,jobname%30,state,elapsed")

(bind-evil-key "SPC l l" (lambda ()
                           (interactive)
                           (slurm-get-stats slurm-username
                                            slurm-host
                                            slurm-job-format)))

;; Projectile level syncing between local and remote hosts
;; set the initial variables to nil
;; .dir-local.el should set these at a project level
(setq rsync-source nil
      rsync-destination nil)

(defun dorsync (src dest)
  "Launch an asynchronuous rsync command"
  (interactive)
  (let ((async-value async-shell-command-display-buffer))
    (setq async-shell-command-display-buffer nil)
    (async-shell-command (concat "rsync -a " src " " dest))
    (setq async-shell-command-display-buffer async-value)))

;; Bind a local key to launch rsync
(bind-evil-key "SPC l ;" (lambda ()
                           (interactive)
                           (dorsync rsync-source rsync-destination)))

(when (file-exists-p "/usr/local/share/emacs/site-lisp/mu4e/mu4e.el")
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e/")
  ;; define some custom keybindings
  (require 'mu4e)
  (define-key mu4e-compose-mode-map (kbd "C-c C-a") 'mail-add-attachment)
  (define-key mu4e-view-mode-map (kbd "C-c C-s") 'org-store-link)
  ;; load the configuration details
  (when (file-exists-p "~/.emacs.d/mu4e-init.el")
    (load "~/.emacs.d/mu4e-init.el")
    (add-hook 'mu4e-main-mode-hook '(lambda () (interactive) (linum-mode -1)))))

(use-package mu4e-alert
  :init
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-disable-mouse-mode)
(mapc #'disable-mouse-in-keymap
  (list evil-motion-state-map
        evil-normal-state-map
        evil-visual-state-map
        evil-insert-state-map))

(global-linum-mode)
(linum-relative-on)

(use-package base16-theme)
(use-package modus-operandi-theme)
(use-package modus-vivendi-theme
  :init
  (setq modus-operandi-theme-org-blocks 'greyscale
        modus-operandi-theme-mode-line 'moody)
  (set-face-attribute 'default nil :family "Lilex Regular" :height 110)
  (set-face-attribute 'variable-pitch nil :family "Lilex Regular" :height 1.1)
  (set-face-attribute 'fixed-pitch nil :family "Lilex Regular" :height 1.0))
(use-package atom-one-dark-theme)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'modus-operandi t)
(set-frame-font "Lilex-12.5")
(setq default-frame-alist '((font . "Lilex-12.5")))

(global-auto-revert-mode t)
(setq completion-auto-help t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'image-mode-hook (lambda () (linum-mode -1)))

(use-package ligature
  :load-path "plugins/ligature.el"
  :config
  ;; Enable ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("->" "==" "===" "<=" ">=" "<-" "!=" "/>"))
  (global-ligature-mode t))

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

(setq auto-save-default nil)
(setq backup-directory-alist '(("" . "~/.Trash")))
(put 'dired-find-alternate-file 'disabled nil)
(setq confirm-kill-processes nil)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq revert-without-query 1)

(use-package dired-single)
(use-package dired-open)
(setq dired-listing-switches "-alhgo --group-directories-first")

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

(setq-default inhibit-startup-screen t)
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq initial-scratch-message "")
