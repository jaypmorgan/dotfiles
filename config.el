(defun my/add-to-exec (new-path)
  " Add the new-path (dir) to the PATH variable "
  (let ((new-path (expand-file-name new-path)))
    (setq exec-path (push new-path exec-path))
    (setenv "PATH" (format "%s:%s" (getenv "PATH") new-path))))

(my/add-to-exec "~/miniconda3/bin")
(my/add-to-exec "~/bin")

(setq gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024)
      evil-want-keybinding nil
      x-wait-for-event-timeout nil
      tramp-ssh-controlmaster-options ""
      tramp-default-method "ssh")

;; Manually installed plugins/packages
(add-to-list 'load-path (expand-file-name (concat user-emacs-directory "plugins/")))

;; Setup package.el to work with MELPA
(setq package-check-signature nil)
(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/"))
(unless package-archive-contents
  (package-refresh-contents))

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
(use-package quelpa-use-package
  :demand t
  :init
  (setq quelpa-use-package-inhibit-loading-quelpa t)
  (unless (package-installed-p 'quelpa-use-package)
    (quelpa
     '(quelpa-use-package
       :fetcher git
       :url "https://github.com/quelpa/quelpa-use-package.git"))))

(use-package markdown-mode :defer t)
(use-package htmlize :defer t)
(use-package haskell-mode :defer t)
(use-package flycheck)
(use-package yaml-mode)


(defun toggle-repl (repl-name)
  (interactive)
  (let ((curr-buffer (buffer-name)))
    (if (string-equal repl-name curr-buffer)
        (progn
          (select-window (get-buffer-window prev-buffer))
          (goto-char saved-position))
      (setq prev-buffer curr-buffer
            saved-position (point))
      (select-window (get-buffer-window repl-name)))))

(use-package slurp-mode
  :ensure nil
  :quelpa (slurp-mode :fetcher github :repo "jaypmorgan/slurp-mode")
  :init
  (defun toggle-slurp-repl ()
    (interactive)
    (toggle-repl "*SluRp*"))
  (define-key slurp-mode-map (kbd "C-`") #'toggle-slurp-repl)
  (define-key slurp-repl-mode-map (kbd "C-`") #'toggle-slurp-repl))

(use-package slurp-repl-mode
  :ensure nil
  :after slurp-mode
  :quelpa (slurp-mode-repl :fetcher github :repo "jaypmorgan/slurp-mode")
  :commands (run-slurp run-slurp-other-window)
  :bind (:map slurp-mode-map
              ("C-c C-c" . slurp-repl-send-line)
              ("C-c C-r" . slurp-repl-send-region)
              ("C-c C-b" . slurp-repl-send-buffer))
  :init
  (setq slurp-repl-location "~/workspace/slurp/slurp"))

(use-package isend-mode ;; language agnostic send to terminal
  :defer t
  :init
  (setq isend-strip-empty-lines t
        isend-delete-indentation nil
        isend-end-with-empty-line nil
        isend-send-region-function 'isend--ipython-cpaste))

;; C++/C/Objective-C LSP support
(use-package ccls
  :defer t
  :config
  (setq ccls-executable "~/Applications/ccls/Release/ccls"))

 ;; Emacs speaks statistics (R)
(use-package ess
  :defer t
  :config
  (require 'ess-r-mode)
  (use-package ess-view)

  ;; enable company mode completions in the REPL
  (add-hook 'inferior-ess-r-mode-hook 'company-mode)

  (defun r/toggle-r-repl ()
    (interactive)
    (toggle-repl "*R*"))
  (define-key org-mode-map (kbd "C-`") #'r/toggle-r-repl)
  (define-key ess-r-mode-map (kbd "C-`") #'r/toggle-r-repl)
  (define-key inferior-ess-r-mode-map (kbd "C-`") #'r/toggle-r-repl)

  (defun r/open-workspace ()
    " Open side panel containing r-dired and r console "
    (interactive)
    (if (< (window-total-width) 200)
        (split-window-right)
        (split-window-right -120))
    (other-window 1)
    (switch-to-buffer "*R*")
    (split-window-below)
    (switch-to-buffer "*R*")
    (ess-rdired)
    (ess-rdired-mode)
    (other-window -1)
    (set-window-dedicated-p (nth 1 (window-list)) t)
    (set-window-dedicated-p (nth 2 (window-list)) t)
    (imenu-list-smart-toggle))

  (define-key org-mode-map (kbd "<f7>") 'r/open-workspace)
  (define-key ess-r-mode-map (kbd "<f7>") 'r/open-workspace)

  (defun my/ess-style ()
    (ess-set-style 'C++ 'quiet)
    (setq ess-indent-level 2))
  (add-hook 'ess-mode-hook 'my/ess-style)

  (require 'ess-rdired)
  (define-key ess-rdired-mode-map (kbd "C-c p") 'ess-rdired-plot)
  (define-key ess-rdired-mode-map (kbd "C-c e") 'ess-rdired-edit)
  (define-key ess-rdired-mode-map (kbd "C-c v") 'ess-rdired-view)

  ;; define variables scroll to the end of R shell automatically when
  ;; new input is entered.
  (setq comint-scroll-to-bottom-on-input t
        comint-scroll-to-bottom-on-output t
        comint-move-point-for-output t
        ess-eval-visibly 'nowait)

  ;; setup window management
  (setq display-buffer-alist
        `(("\\*R dired\\*"
           (display-buffer-reuse-window display-buffer-same-window)
           (reusable-frames . nil))
          ("\\*R"
           (display-buffer-reuse-window display-buffer-in-side-window)
           (side . bottom)
           (window-width . 0.4)
           (reusable-frames . nil))
          ("\\*help"
           (display-buffer-reuse-window display-buffer-in-side-window)
           (side . bottom)
           (reusable-frames . nil)))))

(use-package python-mode
    :defer t
    :init
    (setq python-shell-interpreter "jupyter"
          python-shell-interpreter-args "console --simple-prompt"
          python-shell-prompt-detect-failure-warning nil
          python-indent-offset 4
          python-indent-guess-indent-offset-verbose nil)

    (use-package blacken
      :config
      (defun blacken-python-hook ()
          (when (eq major-mode 'python-mode)
            (blacken-buffer)))
    (add-hook 'before-save-hook 'blacken-python-hook))

    (use-package conda
          :config
          (setq conda-anaconda-home (expand-file-name "~/miniconda3/")
                conda-env-home-directory (expand-file-name "~/miniconda3/"))))

(use-package julia-mode :defer t)
(use-package julia-repl
   :after julia-mode
   :hook (julia-mode . julia-repl-mode))

(use-package company
  :hook (prog-mode . company-mode)
  :config
  (setq company-idle-delay 0.5
        company-minimum-prefix-length 2
        company-candidates-cache t))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package lsp-mode
  :hook ((python-mode . lsp-deferred))
  :commands (lsp lsp-deferred)
  :config (lsp-enable-which-key-integration t)
  :init
  (setq lsp-file-watch-threshold 2000
        lsp-modeline-code-actions-enable t
        lsp-eldoc-enable-hover nil
        lsp-log-io nil
        lsp-idle-delay 0.5))

(use-package org
  :ensure org-plus-contrib
  :init
  (require 'pdf-view)
  (require 'ox-latex)

  (use-package org-present
    :bind (:map org-present-map
           ("C-c n" . org-present-next)
           ("C-c p" . org-present-prev)))

  (setq org-directory notes-dir)
  (setq org-default-notes-file (concat org-directory "notes.org"))
  (setq org-capture-templates
        '(("t" "Todo" entry (file "notes.org")
           "* TODO %?\n%a\n %i\n")
          ("m" "Meeting" entry (file "meeting.org")
           "* [%T] %?\n")))

  (add-to-list 'org-latex-classes
               '("beamer"
                 "\\documentclass\[presentation\]\{beamer\}"
                 ("\\section\{%s\}" . "\\section*\{%s\}")
                 ("\\subsection\{%s\}" . "\\subsection*\{%s\}")
                 ("\\subsubsection\{%s\}" . "\\subsubsection*\{%s\}")))

  (add-hook 'org-mode-hook #'(lambda ()
                              (set-fill-column 85)
                              (visual-line-mode 1)
                              (auto-fill-mode 1)
                              (flyspell-mode 1)
                              (flyspell-buffer)))
  (add-hook 'org-babel-after-execute-hook #'org-redisplay-inline-images)
  (define-key org-mode-map (kbd "<f5>") #'org-latex-export-to-pdf)

  ;; swap between exported PDF and Org document by pressing F4
  (defun my/toggle-pdf (extension)
    (interactive)
    (let ((filename (file-name-base (buffer-file-name (window-buffer (minibuffer-selected-window))))))
      (find-file (concat filename extension))))
  (defun my/open-to-odf-other-window ()
    (interactive)
    (split-window-right)
    (other-window 1)
    (my/toggle-pdf ".pdf"))
  (defun my/swap-to-pdf () (interactive) (my/toggle-pdf ".pdf"))
  (defun my/swap-to-org () (interactive) (my/toggle-pdf ".org"))
  (define-key org-mode-map (kbd "<f4>") #'my/swap-to-pdf)
  (define-key pdf-view-mode-map (kbd "<f4>") #'my/swap-to-org)
  (define-key org-mode-map (kbd "<f3>") #'my/open-to-odf-other-window)

  (define-key org-mode-map (kbd "C-<right>") #'org-babel-next-src-block)
  (define-key org-mode-map (kbd "C-<left>") #'org-babel-previous-src-block)

  (use-package ox-reveal
    :init
    (setq org-reveal-root "file:///usr/lib/node_modules/reveal.js"))
  (use-package org-noter)
  (use-package ob-ipython)
  ;; notes/wiki/journal
  (use-package ox-gfm)
  (use-package org-ref
    :init
    (setq reftex-default-bibliography bib-file-loc
          org-ref-default-bibliography '(bib-file-loc)))
  ;; enable tikzpictures in latex export
  (add-to-list 'org-latex-packages-alist '("" "tikz" t))
  (eval-after-load "preview" '
    (add-to-list 'preview-default-preamble "\\PreviewEnvironment{tikzpicture}" t))

  ;; set variables
  (setq org-startup-indented t
        org-latex-prefer-user-labels t
        org-startup-folded t
        org-src-tab-acts-natively t
        org-src-window-setup 'split-window-below
        org-hide-leading-stars t
        org-edit-src-content-indentation 0
        org-footnote-auto-adjust t
        org-latex-listings 'minted   ;; color highlighting for source blocks
        org-latex-packages-alist '(("" "minted"))
        org-latex-pdf-process '("latexmk -shell-escape -bibtex -f -pdf %f")
        org-format-latex-options (plist-put org-format-latex-options :scale 1.4)
        inferior-julia-program-name "/usr/bin/julia"
        org-confirm-babel-evaluate nil
        org-fontify-done-headline t
        org-log-done 'time
        org-todo-keywords '((type "TODO(t)" "WAIT(w)" "INPROGRESS(p)" "|" "DONE(d)" "CANC(c)"))
        org-todo-keyword-faces '(("TODO" . org-warning)
                                 ("WAIT" . "Firebrick")
                                 ("INPROGRESS" . "SeaGreen3")
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
                                 (emacs-lisp . t)
                                 (julia . t)
                                 (gnuplot . t)
                                 (dot . t)
                                 (plantuml . t))))

(use-package tikz
  :after org)

(use-package toc-org
  :init
  (add-hook 'markdown-mode-hook #'toc-org-mode)
  (add-hook 'org-mode-hook #'toc-org-mode))

(use-package swiper)
(use-package magit)
;; (use-package linum-relative)
(use-package ace-window)
(use-package iedit)
(use-package cheat-sh)

(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :init (require 'smartparens-config))

(use-package plantuml-mode
  :defer t
  :mode ("\\.plantuml\\'" . plantum-mode)
  :init
  (unless (file-exists-p (expand-file-name "~/plantuml.jar"))
    (switch-to-buffer (make-temp-name "plantuml"))
    (ignore-errors (plantuml-mode))
    (plantuml-download-jar))
  (setq plantuml-jar-path (expand-file-name "~/plantuml.jar")
        plantuml-default-exec-mode 'jar
        org-plantuml-jar-path plantuml-jar-path))

(use-package imenu-list
  :defer t
  :init
  (setq imenu-list-size 0.1
        imenu-list-position 'left))

(use-package undo-tree
  :init
  (setq undo-tree-visualizer-timestamps t)
  (global-undo-tree-mode))

(use-package csv-mode
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.csv\\'" . csv-align-mode)))

(use-package yasnippet
  :defer t
  :config
  (yas-global-mode 1))

(use-package olivetti
  :defer t
  :init
  (setq olivetti-body-width 100))

(use-package pdf-tools
  :defer t
  :init
  (pdf-loader-install)
  (setq auto-revert-interval 0.5)
  (add-hook 'pdf-view-mode-hook #'(lambda () (linum-mode -1))))

(use-package flyspell
  :init
  (setq flyspell-default-dictionary "british"))

(use-package writegood-mode)

(use-package popper
 :ensure t
 :bind (("C-1" . popper-toggle-latest)
        ("C-2" . popper-cycle)
        ("C-3" . popper-toggle-type))
 :init
 (setq popper-reference-buffers
       '("\\*Messages\\*"
         "Output\\*$"
         "\\*Flycheck Errors\\*"
         "\\*slurm-log\\*"
         "\\*Warnings\\*"
         help-mode
         helm-mode
         compilation-mode))
 (popper-mode +1))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(global-set-key [tab] #'tab-indent-or-complete)
(define-key magit-mode-map [tab] #'magit-section-toggle)

(use-package evil
  :init
  (use-package undo-fu)
  (setq evil-undo-system 'undo-fu)
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

(use-package mood-line
  :init
  (mood-line-mode))

(use-package projectile
  :config
  (projectile-mode 1)
  (setq projectile-git-submodule-command nil)
  (setq projectile-mode-line-function '(lambda () (format " Proj[%s]" (projectile-project-name))))
  (setq projectile-project-search-path '("/media/hdd/workspace/")))

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

(use-package vertico
  :init
  (vertico-mode)
  (define-key vertico-map "?" #'minibuffer-completion-help)
  (define-key vertico-map (kbd "M-RET") #'minibuffer-force-complete-and-exit)
  (define-key vertico-map (kbd "M-TAB") #'minibuffer-complete)

  (use-package consult)
  (use-package savehist :init (savehist-mode))
  (use-package marginalia :init (marginalia-mode))
  (use-package orderless
    :init
    (setq completion-styles '(substring orderless)
          completion-category-defaults nil
          completion-category-override '((file (styles . (partial-completion)))))))

(use-package bibtex-actions
  :custom
  (bibtex-completion-bibliography bib-file-loc)
  :init

  (defun bibtex-actions-add-citation (citation)
    (interactive (list (read-from-minibuffer "Bibtex citation: ")))
    (write-region (concat "\n" citation "\n") nil bibtex-completion-bibliography 'append)
    (bibtex-actions-refresh))

  (defun bibtex-actions-open-library ()
    (interactive)
    (split-window-sensibly)
    (find-file bibtex-completion-bibliography)))

(require 'hydra)
(require 'evil)
(require 'ace-window)
(define-key evil-motion-state-map " " nil)

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

(defmacro bind-evil-normal-key (binding func)
  `(define-key evil-motion-state-map (kbd ,binding) (quote ,func)))

(defmacro bind-evil-visual-key (binding func)
  `(define-key evil-visual-state-map (kbd ,binding) (quote ,func)))

(defmacro bind-global-key (binding func)
  `(global-set-key (kbd ,binding) (quote ,func)))

(with-eval-after-load 'evil-maps
  (define-key evil-normal-state-map (kbd "C-n") nil))
(bind-evil-normal-key "C-n"
  (lambda ()
    (interactive)
    (iedit-mode)
    (iedit-restrict-current-line)))

(bind-evil-visual-key "SPC l f" align-regexp)
(bind-global-key "M-/" comment-line)

(defhydra hydra-find-things (:color blue :hint nil)
  "Ivy Files"
  ("f" find-file "Find Files")
  ("c" consult-find "Find File via Regex")
  ("o" consult-recent-file "File Recently Opened Files")
  ("b" swiper "Find in buffer")
  ("r" consult-ripgrep "Find with Ripgrep"))
(bind-evil-normal-key "SPC f" hydra-find-things/body)

(defhydra hydra-lsp-common (:color blue :hint nil)
  "LSP Common"
  ("r" lsp-rename "Rename symbol")
  ("l" flycheck-list-errors "List warnings/errors"))
(bind-evil-normal-key "SPC c" hydra-lsp-common/body)

(bind-evil-normal-key "SPC p" projectile-command-map)
(bind-evil-normal-key "SPC p a" projectile-add-known-project)
(bind-evil-normal-key "SPC /" consult-ripgrep)
(bind-evil-normal-key "SPC g" magit-status)
(bind-evil-normal-key "SPC a" org-agenda)
(bind-evil-normal-key "SPC w" ace-window)
(bind-evil-normal-key "SPC n" org-capture)
(bind-evil-normal-key "SPC e" eww)
(bind-evil-normal-key "SPC <return>" consult-bookmark)
(bind-global-key "C-x ," (lambda () (interactive) (vterm t))) ;; new terminal in window

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

(bind-evil-normal-key "SPC s v" my/split-vertical)
(bind-evil-normal-key "SPC s h" my/split-horizontal)

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
(bind-evil-normal-key "SPC TAB" hydra-eyebrowse/body)

(bind-evil-normal-key "SPC SPC" consult-buffer)
(bind-global-key "C-x b" consult-buffer)

(defhydra hydra-open-config (:color blue :hint nil)
  "Open Config"
  ("e" (find-file (concat user-emacs-directory "config.org")) "Emacs Config")
  ("x" (find-file "~/.xmonad/xmonad.hs") "Xmonad Config")
  ("m" (find-file (concat user-emacs-directory "mu4e-init.el")) "Mail Config"))

(defhydra hydra-shell-buffer (:color blue :hint nil)
  "Open Shell"
  ("s" my/bash "Shell")
  ("S" run-slurp-other-window "SluRp")
  ("j" julia-repl "Julia repl")
  ("r" R "R repl")
  ("p" python "Python repl"))

(defhydra hydra-openbuffer (:color blue :hint nil)
  "Open Buffer"
  ("c" hydra-open-config/body "Config files")
  ("C" calendar "Open calendar")
  ("b" bibtex-actions-open "Open Bibliography")
  ("d" (progn (split-window-sensibly) (dired-jump)) "Dired in another window")
  ("D" (dired-jump) "Dired")
  ("e" elfeed "Elfeed")
  ("g" org-roam-graph "Open Org Roam Graph")
  ("i" imenu-list-smart-toggle "Open Menu Buffer")
  ("m" mu4e "Open Mailbox")
  ("s" hydra-shell-buffer/body "Open shell")
  ("t" (find-file tasks-loc) "Open tasks")
  ("u" undo-tree-visualize "Undo-tree")
  ("x" cheat-sh "CheatSheet"))
(bind-evil-normal-key "SPC o" hydra-openbuffer/body)

(defun new-org-note ()
  (interactive)
  (let ((buffer (generate-new-buffer "untitled")))
    (switch-to-buffer buffer)
    (org-mode)))

(defhydra hydra-insert (:color blue :hint nil)
  "Insert into Buffer"
  ("s" yas-insert-snippet "Insert Snippet")
  ("r" org-ref-insert-cite-with-completion "Insert citation")
  ("l" org-roam-insert "Org Roam link")
  ("j" org-journal-new-entry "Insert New Journal Entry")
  ("n" new-org-note "New Org-mode note"))
(bind-evil-normal-key "SPC i" hydra-insert/body)

(defhydra hydra-remote-hosts (:color blue :hint nil)
  "Browse remote hosts"
  ("l" (dired-at-point (concat "/ssh:lis.me:" lis-path)) "LIS Lab")
  ("s" (dired-at-point "/ssh:sunbird.me:~/workspace") "Sunbird Swansea")
  ("c" (dired-at-point "/ssh:chemistry.me:~/workspace") "Chemistry Swanasea"))
(bind-evil-normal-key "SPC r" hydra-remote-hosts/body)

(defhydra hydra-modify-buffers (:color blue :hint nil)
  "Modify buffer"
  ("w" (write-file (buffer-file-name)) "Write")
  ("o" olivetti-mode "Olivetti Mode")
  ("b" ibuffer "Edit Buffers")
  ("q" (kill-buffer-and-window) "Close"))
(bind-evil-normal-key "SPC m" hydra-modify-buffers/body)

(define-minor-mode writing-room-mode
  "A minor mode for distractionless writing"
  :lighter " Writing-Room"
  (set (make-local-variable 'line-spacing) 20)
  (add-hook 'writing-room-mode-hook 'linum-mode)
  (add-hook 'writing-room-mode-hook 'olivetti-mode)
  (add-hook 'writing-room-mode-hook 'writegood-mode)
  (add-hook 'writing-room-mode-hook 'flyspell-mode)
  (add-hook 'writing-room-mode-hook 'variable-pitch-mode))

(defun get-stats (user host format)
  "Get SLURM status from remote server"
  (eshell-command-result
   (concat
    "cd /ssh:" host ":/ && sacct -u" user " --format=" format "| grep -v '\\(.ex\\|.ba\\)'")))

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

(bind-evil-normal-key "SPC l l" (lambda ()
                           (interactive)
                           (slurm-get-stats slurm-username
                                            slurm-host
                                            slurm-job-format)))

;; Projectile level syncing between local and remote hosts
;; set the initial variables to nil
;; .dir-local.el should set these at a project level
(setq rsync-source nil
      rsync-destination nil
      rsync-base-cmd "rsync -azm"
      rsync-exclude-list '("data" ".git" "container-dev" "container" "__pycache__" "*.pyc" "renv/library" "renv/local" "renv/python" "renv/staging"))

(defun rsync--build-exclude-list (exclude-list)
  (mapconcat (lambda (s) (concat " --exclude=" s " ")) exclude-list " "))

(defun rsync--cmd (&optional display)
  (if display
      (concat rsync-base-cmd " --progress " (rsync--build-exclude-list rsync-exclude-list))
    (concat rsync-base-cmd (rsync--build-exclude-list rsync-exclude-list))))

(defun dorsync (src dest is_hidden)
  "Launch an asynchronuous rsync command"
  (interactive)
  (let ((async-value async-shell-command-display-buffer))
    (if is_hidden
        (progn
            (setq async-shell-command-display-buffer nil)
            (setq rsync-cmd (rsync--cmd)))
      (setq rsync-cmd (rsync--cmd t)))
    (async-shell-command (concat rsync-cmd " " src " " dest))
    (setq async-shell-command-display-buffer async-value)))

;; Bind a local key to launch rsync
(bind-evil-normal-key "SPC l ;" (lambda () (interactive) (dorsync rsync-source rsync-destination 1)))
(bind-evil-normal-key "SPC l ," (lambda () (interactive) (dorsync rsync-source rsync-destination nil)))

(defun conda-activate-once (env-name)
  " Set the conda environment if it hasn't been set yet "
  (interactive)
  (let ((current-env (locate-file "python" exec-path)))
    (unless (string-match-p (regexp-quote env-name) current-env)
      (conda-env-activate env-name))))

(when (file-exists-p "/usr/local/share/emacs/site-lisp/mu4e/mu4e.el")
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e/")
  ;; define some custom keybindings
  (require 'mu4e)
  (define-key mu4e-compose-mode-map (kbd "C-c C-a") 'mail-add-attachment)
  (define-key mu4e-view-mode-map (kbd "C-c C-s") 'org-store-link)
  ;; load the configuration details
  (let ((mu4e-config (concat user-emacs-directory "mu4e-init.el")))
    (when (file-exists-p mu4e-config)
      (load mu4e-config)
      (add-hook 'mu4e-main-mode-hook '(lambda () (interactive) (linum-mode -1))))))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-linum-mode)
;; (linum-relative-on)
(setq display-line-numbers 'visual)

(use-package modus-themes
 :bind (("<f8>" . modus-themes-toggle))
 :init
 (setq modus-operandi-themes-org-blocks 'greyscale
       modus-operandi-themes-mode-line 'moody))

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'modus-operandi t)

;; define the font face and size
(set-face-attribute 'fixed-pitch nil :family "Jetbrains mono" :height 110)
(setq default-frame-alist '((font . "Jetbrains Mono-11")))

(global-auto-revert-mode t)
(setq completion-auto-help t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'image-mode-hook (lambda () (linum-mode -1)))

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

(setq create-lockfiles nil)
(setq auto-save-default nil)
(setq backup-directory-alist '(("" . "~/.Trash")))
(put 'dired-find-alternate-file 'disabled nil)
(setq confirm-kill-processes nil)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq revert-without-query 1)

;; make dired easier to read
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

(use-package exwm
  :init
  (require 'exwm)
  ;; send keys chords directly to emacs instead of underlying window
  (setq exwm-input-prefix-keys
        '(?\C-x
          ?\C-u
          ?\C-h
          ?\C-c
          ?\C-w
          ?\C-\s
          ?\M-x
          ?\M-`
          ?\M-&
          ?\M-:))

  ;; but if prefixed with C-q then send the next keystroke to window
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)


  (defun launch-program (cmd)
    (interactive (list (read-shell-command "$ ")))
    (start-process-shell-command cmd nil cmd))

  (defun exwm-logout ()
    (interactive)
    (recentf-save-list)
    (save-some-buffers)
    (start-process-shell-command "logout" nil "lxsession-logout"))

  ;; Make buffer name more meaningful
  (add-hook 'exwm-update-class-hook
            (lambda ()
            (exwm-workspace-rename-buffer exwm-class-name)))
  ;; remove modeline for floating windows
  (add-hook 'exwm-floating-setup-hook 'exwm-layout-hide-mode-line)

  ;; start up applications
  ;; (setq my/exwm-startup-applications '("nextcloud"))
  ;; (defun my/launch-startup (apps)
  ;;   (let ((apps my/exwm-startup-applications))
  ;;     (map #'launch-program apps)))
  ;; (add-hook 'exwm-mode-hook 'my/launch-startup)

  ;; define keys to manage EXWM environment
  (setq exwm-input-global-keys
        `(([?\s-r]    . exwm-reset)
         ([s-left]     . windmove-left)
         ([s-right]    . windmove-right)
         ([s-up]       . windmove-up)
         ([s-down]     . windmove-down)
         ([?\s-&]      . launch-program)
         ([?\s-w]      . exwm-workspace-switch)
         ([?\s-b]      . exwm-layout-toggle-mode-line)
         ([?\s-i]      . (lambda () (interactive) (launch-program "firefox")))
          ;; swap to workspace with s-N
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

  (use-package xbacklight
    :ensure nil  ;; so quelpa-use-package works
    :quelpa (xbacklight :fetcher github :repo "dakra/xbacklight")
    :bind (("<XF86MonBrightnessUp>" . xbacklight-increase)
           ("<XF86MonBrightnessDown>" . xbacklight-decrease)
           :map exwm-mode-map
           ("<XF86MonBrightnessUp>" . xbacklight-increase)
           ("<XF86MonBrightnessDown>" . xbacklight-decrease))
    :init
    (setq xbacklight-step 5))

  (use-package pulseaudio-control
    :bind (("<XF86AudioRaiseVolume>" . pulseaudio-control-increase-volume)
           ("<XF86AudioLowerVolume>" . pulseaudio-control-decrease-volume)
           ("<XF86AudioMute>" . pulseaudio-control-toggle-current-sink-mute)
           :map exwm-mode-map
           ("<XF86AudioRaiseVolume>" . pulseaudio-control-increase-volume)
           ("<XF86AudioLowerVolume>" . pulseaudio-control-decrease-volume)
           ("<XF86AudioMute>" . pulseaudio-control-toggle-current-sink-mute)))

  ;; display time and battery
  (setq display-time-format " %H:%M:%S %a,%d %b ")
  (display-time-mode)
  (use-package fancy-battery :init (fancy-battery-mode))

  ;; start in workspace 1
  (setq exwm-workspace-current-index 1
        exwm-workspace-number        4)

  (exwm-enable))

(appt-activate 1)
(setq diary-file diary-loc
      calendar-date-style "iso"
      appt-display-mode-line t
      org-agenda-diary-file diary-file
      org-agenda-include-diary t)
(define-key calendar-mode-map (kbd "C-x i") 'diary-insert-entry)
(add-hook 'diary-list-entries-hook 'diary-sort-entries t)
