(defun add-to-exec-path (path)
  (let ((fp (expand-file-name path)))
    (unless (member fp exec-path)
      (setq exec-path (cons fp exec-path)))))

(add-to-exec-path "~/.bin")
(add-to-exec-path "~/.bin/julia")

(setq package-enable-at-startup nil
      straight-check-for-modifications 'live)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t
      use-package-always-defer t)

(straight-override-recipe
 '(org :type git :host github :repo "emacsmirror/org" :no-build t))

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      auto-save-default nil
      backup-inhibited t
      create-lockfiles nil
      custom-file (concat user-emacs-directory "custom.el")
      revert-without-query '(".*")
      require-final-newline t
      indent-tabs-mode nil
      dired-listing-switches "-alhgo --group-directories-first"
      dired-auto-revert-buffer t
      ring-bell-function 'ignore
      dired-dwim-target t
      home-path "~/"
      confirm-kill-processes nil
      confirm-kill-emacs nil)

(defun from-home (path)
  (concat home-path path))

;; load the customize file to keep this init clean
(when (file-exists-p custom-file)
  (load-file custom-file))

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'isearch-forward 'isearch-forward-regexp)
(defalias 'isearch-backward 'isearch-backward-regexp)

(recentf-mode t)
(setq recentf-max-menu-items 100
      recentf-max-saved-items 100)

(defun my/recentf (file)
  "Select from recentf files using the minibuffer"
  (interactive
   (list (completing-read "Recent file: " (recentf-menu-elements recentf-max-menu-items))))
  (find-file file))

(global-auto-revert-mode)
(delete-selection-mode)  ;; delete whats highlighted if user types/pastes something
(add-hook 'write-file-hooks 'delete-trailing-whitespace nil t)

;; (use-package vertico
;;   :init (vertico-mode t))

(use-package marginalia
  :init (marginalia-mode))

(use-package which-key
  :init
  ;; only show which-key if C-h is trigged during keystroke
  (setq which-key-show-early-on-C-h t
        which-key-idle-delay 10000
        which-key-idle-secondary-delay 0.01)
  (which-key-mode))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	completion-category-overrides '((file (styles basic partial-completion)))))

(icomplete-mode t)
(setq icomplete-prospects-height 1)

(use-package avy)

(setq my/window-config (current-window-configuration))
(defun my/fullscreen-toggle ()
  (interactive)
  (if (= (length (window-list)) 1)
      (set-window-configuration my/window-config)
    (progn
      (setq my/window-config (current-window-configuration))
      (delete-other-windows))))

(use-package expand-region
  :defer nil
  :commands (er/expand-region)
  :bind ("C-=" . er/expand-region))

(use-package multiple-cursors
  :defer nil
  :bind (("C-M-<" . mc/mark-previous-like-this)
	 ("C-M->" . mc/mark-next-like-this)))

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

(defun find-forward ()
  "Move cursor after character ahead of current position"
  (interactive)
  (let ((searchc (byte-to-string (read-char))))
    (search-forward searchc)))

(defun find-backward ()
  "Move cursor after character behind current position"
  (interactive)
  (let ((searchc (byte-to-string (read-char))))
    (search-backward searchc)
    (right-char)))

(defun copy-whole-line ()
  "Copy the whole line"
  (interactive)
  (let ((org (point))
	(beg (line-beginning-position))
	(end (progn (next-line)
		    (line-beginning-position))))
    (kill-ring-save beg end)
    (goto-char org)))

;; don't recenter the cursor in the vertical plane when the cursor
;; moves to the end of the page. Instead, increment the page
;; (i.e. move the page up or down) therefore preserving the context of
;; the cursor.
(setq scroll-margin 3
      scroll-conservatively 101
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01
      scroll-preserve-screen-position t
      auto-window-vscroll nil)

(use-package ace-window)

(use-package perspective
  :bind (("C-x k" . persp-kill-buffer*))
  :init (persp-mode)
  :custom (persp-mode-prefix-key (kbd "C-x x")))

(use-package company
  :bind ("M-/" . company-complete)
  :hook (after-init . global-company-mode)
  :config
  (setq company-minimum-prefix-length 2
	company-idle-delay 0.2))

(use-package company-quickhelp
  :after company
  :config
  (company-quickhelp-mode t))

(use-package magit)

(use-package vterm
  :config
  (setq vterm-kill-buffer-on-exit t))

(defun vterm-below ()
  "Open a vterm window below"
  (interactive)
  (split-window-below -20)
  (other-window 1)
  (vterm t)
  (activate-projectile-project-in-terminal))

(defun activate-projectile-project-in-terminal ()
  (interactive)
  (let ((project-name pyvenv-virtual-env-name))
    (vterm-send-string (format "conda activate %s" project-name))
    (vterm-send-return)
    (vterm-clear)))

(use-package projectile
  :defer nil
  :bind-keymap ("M-p" . projectile-command-map)
  :bind (:map projectile-mode-map
	      ("C-c p t p" . run-python-projectile))
  :init
  (projectile-mode t)
  (setq projectile-project-search-path (list (from-home "workspace/"))))

(defun run-repl-projectile (cmd)
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (funcall cmd)))

(defun run-python-projectile ()
  (interactive)
  (run-repl-projectile #'run-python))

(use-package ripgrep :after projectile)

(use-package undo-tree
  :init
  (global-undo-tree-mode)
  :config
  (setq undo-tree-visualizer-diff t
	undo-tree-visualizer-timestamps t
	undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(use-package c-mode
  :straight nil
  :hook ((c++-mode . electric-pair-mode)
	 (c-mode . electric-pair-mode))
  :init
  (setq c-default-style "linux"
	c-basic-offset 4))

(require 'flymake)

(use-package python-mode
  :hook (python-mode . prettify-symbols-mode)
  :bind (:map python-mode-map
	      ("C-c C-c" . python-shell-send-buffer)
	      ("C-c C-r" . python-shell-send-region))
  :init
  (setq python-indent-offset 4
	python-shell-interpreter "ipython"
	python-shell-interpreter-args "--pprint --autoindent --simple-prompt -i --matplotlib"
	py-default-interpreter "ipython"))

(use-package eglot; :hook (python-mode . eglot-mode)
  )

(use-package blacken)

(use-package pyvenv
  :defer nil
  :hook ((python-mode . pyvenv-mode)
	 (projectile-mode . pyvenv-mode))
  :init
  (setenv "WORKON_HOME" (expand-file-name "~/.bin/miniconda3/envs"))
  (pyvenv-mode))

(defun highlight-and-send ()
  "Highlight a code send and send it via isend"
  (interactive)
  (cond ((eq major-mode 'org-mode) (org-babel-mark-block))
	((eq major-mode 'python-mode) (code-cells-mark-cell))
	(t (error (format "Unknown major mode: %s" major-mode))))
  (isend-send))

(use-package code-cells
  :hook (python-mode . code-cells-mode-maybe)
  :bind (:map code-cells-mode
	      ("C-c <return>" . highlight-and-send)
	      ("C-<left>" . code-cells-backward-cell)
	      ("C-<right>" . code-cells-forward-cell)))

(defun string-replace (fromstring tostring instring)
  (replace-regexp-in-string (regexp-quote fromstring) tostring instring nil 'literal))

(defun conda-activate-once (name)
  "Activate a conda environment only if it is not already set"
  (interactive)
  (unless (string= pyvenv-virtual-env-name name)
    (pyvenv-workon name)))

(use-package highlight-indent-guides
  :hook ((prog-mode . highlight-indent-guides-mode))
  :config (setq highlight-indent-guides-method 'character))

(use-package numpydoc
  :config
  (setq numpydoc-insert-parameter-types t
	numpydoc-insert-return-without-typehint t))

(use-package zeal-at-point)
(use-package csv-mode)
(use-package yaml-mode)
(use-package markdown-mode)

(use-package isend-mode
  :config
  (setq isend-send-region-function 'isend--ipython-cpaste))

(use-package ess
  :config
  (setq ess-indent-level 2))

(use-package paredit
  :hook ((lisp-mode . paredit-mode)
	 (emacs-lisp-mode . paredit-mode)))

;; racket
(use-package racket-mode
  :hook (racket-mode . paredit-mode)
  :init
  (use-package quack)
  (use-package geiser-racket))

(use-package lisp-mode
  :straight nil
  :hook ((lisp-mode . show-paren-mode)
	 (lisp-mode . prettify-symbols-mode)))

(use-package emacs-lisp-mode
  :straight nil
  :hook ((emacs-lisp-mode . show-paren-mode)))

(use-package sly
  :init
  (setq inferior-lisp-program "sbcl"))

(use-package slurp-mode
  :straight (slurp-mode :type git :host github :repo "jaypmorgan/slurp-mode")
  :init
  (setq slurp-repl-location (from-home "workspace/slurp/slurp")))

(use-package slurp-repl-mode
  :straight (slurp-repl-mode :type git :host github :repo "jaypmorgan/slurp-mode")
  :bind (:map slurp-mode-map
	      ("C-c C-c" . slurp-repl-send-line)
	      ("C-c C-z" . run-slurp-other-window)))

(use-package plantuml-mode
  :mode ("\\.plantuml\\'" . plantum-mode)
  :init
  (let ((filepath (expand-file-name "~/.bin/plantuml.jar")))
    (unless (file-exists-p filepath)
      (switch-to-buffer (make-temp-name "plantuml"))
      (ignore-errors (plantuml-mode))
      (plantuml-download-jar))
    (setq plantuml-jar-path filepath
          plantuml-default-exec-mode 'jar
          org-plantuml-jar-path plantuml-jar-path)))

(setq language-mode->functions
      '((python-mode . ((:format . lsp-format-buffer)
			(:refacor . lsp-rename)
			(:goto-definition . xref-find-definitions)))
	(emacs-lisp-mode . ((:goto-definition . xref-find-definitions)))))

(defun get-language-function (language fun-type)
  "Get a function associated with language"
  (cdr (assoc fun-type (assoc language language-mode->functions))))

(defun get-registered-languages ()
  "Get a list of languages defined in programming system"
  (mapcar 'car language-mode->functions))

(defmacro register-source-code-fun (fun-name fun-type)
  `(defun ,fun-name ()
     (interactive)
     (cond
      ,@(append (cl-loop for lang in (get-registered-languages) collect
			 `((eq major-mode ',lang)
			   (get-language-function ',lang ,fun-type)))
		'((t (message "Unknown instructions for %s" major-mode)))))))

;; Generate some functions
(register-source-code-fun source-code-format :format)
(register-source-code-fun source-code-refactor :refactor)
(register-source-code-fun source-code-goto-definition :goto-definition)

;; Projectile level syncing between local and remote hosts
;; set the initial variables to nil
;; .dir-local.el should set these at a project level
(setq rsync-source nil
      rsync-destination nil
      rsync-base-cmd "rsync -am"
      rsync-exclude-list '("data" ".git" "container-dev" "container"
			   "__pycache__" "*.pyc" "renv/library" "renv/local"
			   "renv/python" "renv/staging" "build" "dist"))

(defun rsync--build-exclude-list (exclude-list)
  (mapconcat
   (lambda (s) (concat " --exclude=" s " "))
   exclude-list " "))

(defun rsync--cmd (&optional display)
  (let ((exclude-list (rsync--build-exclude-list rsync-exclude-list)))
    (if display
	(concat rsync-base-cmd " --progress " exclude-list)
      (concat rsync-base-cmd exclude-list))))

(defun select-rsync-destination (dest)
  (interactive (list (completing-read "Destination: " *available-destinations*)))
  (setq rsync-destination dest))

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

(use-package org
  :hook (org-mode . mixed-pitch-mode)
  :straight (:type built-in)
  ;;:ensure org-plus-contrib
  :config
  (require 'org-ref)
  ;;(require 'citar)
  (require 'pdf-view)
  (require 'ox-latex)
  (use-package gnuplot)
  (use-package ox-rst)
  (use-package ob-async)
  (pdf-loader-install)
  (require 'ox-rst)

  (use-package org-fragtog
    :hook (org-mode . org-fragtog-mode))

  ;; Slide show setup. First we use org-tree slide to provide the
  ;; basic and critical functionality of the slide show and only show
  ;; one heading at one time.
  (use-package org-tree-slide
    :bind (:map org-mode-map ("<f8>" . org-tree-slide-mode)
		("<f9>" . org-tree-slide-move-next-tree)
		("<f7>" . org-tree-slide-move-previous-tree))
    :config
    (setq org-tree-slide-modeline-display nil
	  org-tree-slide-header t))

  ;; It's nice to have a mixed pitch (variable-pitch for body text,
  ;; and fixed-pitch for source code) when viewing the slide shows.
  (use-package mixed-pitch
    :hook ((org-tree-slide-mode . mixed-pitch-mode)
	   ;(org-mode . mixed-pitch-mode)
	   ))

  (setq	org-hide-emphasis-markers t
	org-edit-src-content-indentation 0
	org-footnote-auto-adjust t
	org-confirm-babel-evaluate nil
	org-latex-prefer-user-labels t
	org-src-window-setup 'current-window
	org-latex-listings 'minted
	org-latex-packages-alist '(("" "minted"))
	org-latex-pdf-process '("latexmk -shell-escape -bibtex -f -pdf %f")
	org-highlight-latex-and-related '(latex script entities)
	org-src-fontify-natively t)

  (add-hook 'org-mode-hook #'(lambda ()
			       (set-fill-column 85)
			       (visual-line-mode 1)
			       (auto-fill-mode 1)))

  ;; re-display any inline images after a source code block is executed.
  (define-key org-mode-map (kbd "C-c C-c")
    (lambda ()
      (interactive)
      (org-ctrl-c-ctrl-c)
      (org-display-inline-images)))

  (add-to-list 'org-latex-classes
	       '("book-no-parts"
		 "\\documentclass{book}"
		 ("\\chapter{%s}" . "\\chapter*{%s}")
		 ("\\section{%s}" . "\\section*{%s}")
		 ("\\subsection{%s}" . "\\subsection*{%s}")
		 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
		 ("\\paragraph{%s}" . "\\paragraph*{%s}")))

  (org-babel-do-load-languages 'org-babel-load-languages '((lisp . t)
							   (shell . t)
							   (python . t)
							   (R . t)
							   (gnuplot . t)
							   (plantuml . t)
							   (C . t)))

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

  (define-key pdf-view-mode-map (kbd "<f4>") #'my/swap-to-org)
  (define-key org-mode-map (kbd "<f4>") #'my/swap-to-pdf)
  (define-key org-mode-map (kbd "<f5>") #'org-latex-export-to-pdf)
  (define-key org-mode-map (kbd "<f3>") #'my/open-to-odf-other-window)
  (define-key org-mode-map (kbd "C-<right>") #'org-babel-next-src-block)
  (define-key org-mode-map (kbd "C-<left>") #'org-babel-previous-src-block))

  ;; (require 'color)
  ;; (set-face-attribute 'org-block nil :background (color-darken-name (face-attribute 'default :background) 2))
  ;; (set-face-attribute 'org-block-begin-line nil :background (color-darken-name (face-attribute 'default :background) 4))
  ;; (set-face-attribute 'org-block-end-line nil :background (color-darken-name (face-attribute 'default :background) 4)))

(use-package flyspell
  :hook ((prog-mode . flyspell-prog-mode)
	 (text-mode . flyspell-mode))
  :init
  (setq flyspell-default-dictionary "british"))

(use-package org-roam
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert))
  :custom
  (org-roam-directory (from-home "Nextcloud/Notes/BIOSOFT"))
  (org-roam-capture-templates
   `(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
      :unnarrowed t)
     ("m" "meeting" plain
      (file ,(from-home "Nextcloud/Notes/BIOSOFT/Templates/meeting-template.org"))
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+date: %U\n")
      :unnarrowed t)
     ("p" "paper" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n#+FILETAGS: bibliography\n")
      :unnarrowed t)))
  :init (setq org-roam-v2-ack t)
  :config (org-roam-setup))

(use-package deft
  :bind ("C-c n d" . deft)
  :config
  (setq deft-directory (from-home "Nextcloud/Notes/BIOSOFT")
	deft-recursive t
	deft-strip-summary-regexp ":PROPERTIES:\n\\(.+\n\\)+:END:\n"
	deft-use-filename-as-title t))

(setq org-capture-templates
      `(("f" "Fleeting Note" entry (file ,(from-home "Nextcloud/Notes/fleeting.org"))
	 "* %U\n\n%?" :unnarrowed nil)
	("t" "Todo Entry" entry (file ,(from-home "Nextcloud/Notes/tasks.org"))
	 "* TODO %?\n:PROPERTIES:\n:CREATED: %T\n:END:" :unnarrowed nil)
	("b" "Bug Log" entry (file ,(from-home "Nextcloud/Notes/bugs.org"))
	 "* %T\n\n- Type: %?\n- Severity:\n- What happened:\n" :unnarrowed nil)))
(global-set-key (kbd "C-c C-/") 'org-capture)

(use-package org-roam-ui
  :straight (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
	org-roam-ui-follow t
	org-roam-ui-update-on-save t
	org-roam-open-on-start t))

(setq global-bib-file (from-home "Nextcloud/Notes/references.bib")
      global-bib-pdf (from-home "Nextcloud/Notes/PDFs"))

;; Centre the screen when entering the slide show, and put a fancy
;; border around it!
(use-package olivetti
  :hook (org-tree-slide-mode . olivetti-mode)
  :init
  (setq olivetti-body-width 90
	olivetti-style 'fancy))

(use-package pdf-tools
  :config
  (pdf-loader-install)
  (setq auto-revert-interval 0.5
	pdf-annot-activate-created-annotations t
	pdf-view-display-size 'fit-page))

(use-package org-ref
  :commands (org-ref)
  :config
  (setq reftex-default-bibliography global-bib-file
	bibtex-completion-bibliography (list global-bib-file (from-home "Nextcloud/Notes/zotero.bib"))
	org-ref-default-bibliography (list global-bib-file (from-home "Nextcloud/Notes/zotero.bib"))))

(use-package org-roam-bibtex
  :init
  (org-roam-bibtex-mode t)
  :config
  (require 'org-ref))

(use-package citar
  :bind (("C-c o b f" . citar-open-library-file)
	 ("C-c o b i" . citar-insert-citation)
	 ("C-c o b a" . citar-add-citation)
	 ("C-c o b n" . citar-open-notes))
  :custom
  (citar-bibliography (list
		       (from-home "Nextcloud/Notes/zotero.bib")
		       (from-home "Nextcloud/Notes/references.bib")))
  (citar-library-paths (list (from-home "Nextcloud/Notes/PDFs")))
  :config
  (use-package all-the-icons)

  (defun citar-add-citation (citation)
    "Add a new key to the bibliography file"
    (interactive (list (read-from-minibuffer "Bibtex citation: ")))
    (write-region (concat "\n" citation "\n") nil citar-bibliography 'append)
    (citar-refresh))

  (defun citar-add-and-insert-citation (citation)
    "Add a new key to the bibliography and insert citation into buffer"
    (interactive (list (read-from-minibuffer "Bibtex citation: ")))
    (citar-add-citation citation)
    (and (string-match "@.*?{\\(.*\\)?," citation)
	 (citar-insert-citation (list (match-string 1 citation)))))

  (defun citar-add-pdf-for-citation (citation)
    (interactive (list (completing-read "Citation key: " (citar--extract-keys (citar--get-candidates)))))
    (let* ((citation (car (last (split-string citation " "))))
	   (pdf-link-loc (read-from-minibuffer "PDF location: " ))
	   (new-loc (concat (car citar-library-paths) "/" citation ".pdf")))
      (url-copy-file pdf-link-loc new-loc)
      (citar-refresh)
      new-loc))

  (defun citar-add-pdf-for-citation-and-open (citation)
    (interactive (list (completing-read "Citation key: " (citar--extract-keys (citar--get-candidates)))))
    (let ((loc (citar-add-pdf-for-citation citation)))
      (find-file loc)))

  (setq citar-open-note-function 'orb-citar-edit-note))

(when (file-exists-p "/usr/share/emacs/site-lisp/mu4e/mu4e.el")
  (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
  (require 'org-mu4e)
  (setq mail-user-agent 'mu4e-user-agent)
  (setq org-mu4e-convert-to-html t)
  (let ((mu4e-config (concat user-emacs-directory "mu4e-init.el")))
    (when (file-exists-p mu4e-config)
      (load mu4e-config))))

(use-package calendar
  :hook (diary-list-entries . diary-sort-entries)
  :bind (:map calendar-mode-map ("C-x i" . diary-insert-entry))
  :config
  (setq diary-file (from-home "Nextcloud/Notes/diary")
	calendar-date-style "iso"
	appt-display-mode-line t
	org-agenda-diary-file (from-home "Nextcloud/Notes/diary")
	org-agenda-include-diary t))

(use-package org-gcal
  :config
  (setq org-agenda-include-diary t)
  (let ((gcal-config (concat user-emacs-directory "gcal.el")))
    (when (file-exists-p gcal-config)
      (load gcal-config))))

(use-package elfeed
  :init
  ;; https://www.theinsaneapp.com/2021/04/top-machine-learning-blogs-to-follow-in-2021.html
  (setq elfeed-db-directory "~/.cache/elfeed/"  ;; keep the home directory clean
	elfeed-feeds
        '(("https://ruder.io/rss/index.rss" machine-learning)
          ("https://karpathy.github.io/feed.xml" machine-learning)
          ("https://lilianweng.github.io/lil-log/feed.xml" machine-learning)
          ("https://machinelearningmastery.com/feed/" machine-learning)
          ("http://blog.shakirm.com/feed/" machine-learning)
	  ("http://planet.lisp.org/rss20.xml" lisp programming)
	  ("https://protesilaos.com/books.xml" misc))))

(global-set-key (kbd "C-]") #'join-line)
(global-set-key (kbd "C-x x g") #'revert-buffer)
(global-set-key (kbd "C-;") #'comment-line)
(global-set-key (kbd "C-<tab>") #'expand-abbrev)
(global-set-key (kbd "M-n") #'avy-goto-char-2)
(global-set-key (kbd "M-j") #'avy-goto-line)
(global-set-key (kbd "M-k") 'avy-move-line)
(global-set-key (kbd "s-f") #'forward-sexp)
(global-set-key (kbd "s-b") #'backward-sexp)
(global-set-key (kbd "C-o") #'insert-line-below)
(global-set-key (kbd "C-S-o") #'insert-line-above)
(global-set-key (kbd "C-c y") #'copy-whole-line)
(global-set-key (kbd "C-z") #'repeat)
(global-set-key (kbd "C-f") #'find-forward)
(global-set-key (kbd "C-b") #'find-backward)
(global-set-key (kbd "C-c C-j") #'imenu)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-'") 'my/fullscreen-toggle)
(global-set-key (kbd "C-c <Return>") 'highlight-and-send)

(use-package general)
(general-define-key
 :prefix "C-c"
 ;; buffer/window management
 "a" #'org-agenda
 "q" #'avy-goto-char-timer
 "p" #'projectile-command-map
 "w" #'ace-window
 "e" #'eww
 ;; code actions
 "c f" #'source-code-format
 "c r" #'source-code-refactor
 "c g d" #'source-code-goto-definition
 ;; remote hosts
 "r l" #'(lambda () (interactive) (find-file "/ssh:lis.me:"))
 "l ;" #'(lambda () (interactive) (dorsync rsync-source rsync-destination t))
 "l ," #'(lambda () (interactive) (dorsync rsync-source rsync-destination nil))
 ;; open maps
 "o t" #'(lambda () (interactive) (find-file (from-home "Nextcloud/Notes/tasks.org")))
 "o f" #'(lambda () (interactive) (find-file (from-home "Nextcloud/Notes/fleeting.org")))
 "o s" #'vterm-below
 "o S" #'(lambda () (interactive) (vterm t))
 "o c" #'(lambda () (interactive) (find-file (concat user-emacs-directory "config.org")))
 "o r" 'my/recentf
 "o g" #'(lambda () (interactive) (find-file (from-home "Nextcloud/Notes/google-calendar.org")))
 ;; modify buffer
 "m o" #'olivetti-mode
 "m b" #'ibuffer
 ;; EMMS
 "v v" #'emms
 "v p" #'emms-pause
 "v >" #'emms-seek-forward
 "v <" #'emms-seek-backward
 "v ," #'emms-previous
 "v ." #'emms-next
 ;; organisation
 "o C" #'calendar
 "o m s" #'slack-im-select
 "o m m" #'mu4e
 "o e" #'elfeed
 "o u" #'undo-tree-visualize)

(add-hook 'dired-mode-hook 'hl-line-mode)

(add-to-list 'default-frame-alist '(font . "IBM Plex Mono-10"))

(use-package modus-themes
  ;; this is some text
  :defer nil
  :init
  (setq modus-themes-italic-constructs t
	modus-themes-subtle-line-numbers t
	modus-themes-syntax '(yellow-comments)
	modus-themes-region '(accented bg-only no-extend)
	modus-themes-mode-line '(borderless padding)
	modus-themes-org-blocks 'gray-background)
  (load-theme 'modus-operandi t))

(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

(add-hook 'prog-mode-hook 'linum-mode)

(use-package cern-root-mode
  :straight (cern-root-mode :repo "jaypmorgan/cern-root-mode" :fetcher git :host github)
  :bind (:map c++-mode-map
	      (("C-c C-c" . cern-root-eval-defun-maybe)
	       ("C-c C-b" . cern-root-eval-buffer)
	       ("C-c C-l" . cern-root-eval-file)
	       ("C-c C-r" . cern-root-eval-region)
	       ("C-c C-z" . run-cern-root-other-window)))
  :config
  (setq cern-root-filepath "~/Téléchargements/root-6.26.00/root_install/bin/root"
	cern-root-terminal-backend 'inferior))

(use-package exwm
  :init
  (require 'exwm)
  (require 'exwm-randr)
  
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
          ?\M-:
          ?\s-\ ))

  ;; but if prefixed with C-q then send the next keystroke to window
  (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

  (defun launch-program-with-completion ()
    "Launch a program inside EXWM reading from PATH"
    (interactive)
    (let* ((cmds (split-string (shell-command-to-string "compgen -c") "\n"))
           (cmd  (completing-read "Program: " cmds)))
      (start-process-shell-command cmd nil cmd)))

  (defun launch-program (cmd)
    "Launch a program inside EXWM"
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
  (setq my/exwm-startup-applications '("nextcloud" "nm-applet" "blueman-applet"))
  (defun my/launch-startup ()
    (interactive)
    (mapc #'launch-program my/exwm-startup-applications))
  (add-hook 'exwm-init-hook #'my/launch-startup)

  (setq window-size-delta 10)

  ;; define keys to manage EXWM environment
  (setq exwm-input-global-keys
        `(([?\s-r] . exwm-reset)
         ([?\s-&]  . launch-program-with-completion)
         ([?\s-g]  . launch-program-with-completion)
         ([?\s-w]  . exwm-workspace-switch)
         ([?\s-b]  . exwm-layout-toggle-mode-line)
         ([?\s-i]  . (lambda () (interactive) (launch-program "firefox")))
         ;; window management
         ([?\s-h]    . windmove-left)
         ([?\s-l]    . windmove-right)
         ([?\s-k]    . windmove-up)
         ([?\s-j]    . windmove-down)
         (,(kbd "S-H") . #'(lambda () (exwm-layout-enlarge-window-horizontally window-size-delta)))
         (,(kbd "S-L") . #'(lambda () (exwm-layout-shrink-window-horizontally window-size-delta)))
         (,(kbd "S-J") . #'(lambda () (exwm-layout-shrink-window window-size-delta)))
         (,(kbd "S-K") . #'(lambda () (exwm-layout-enlarge-window window-size-delta)))
         ;; worskspace management
         ;; swap to workspace with s-N
         ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" i)) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 9))))

  (use-package xbacklight
    :straight (xbacklight :fetcher git :host github :repo "dakra/xbacklight")
    :bind (("<XF86MonBrightnessUp>" . xbacklight-increase)
           ("<XF86MonBrightnessDown>" . xbacklight-decrease)
           :map exwm-mode-map
           ("<XF86MonBrightnessUp>" . xbacklight-increase)
           ("<XF86MonBrightnessDown>" . xbacklight-decrease))
    :init (setq xbacklight-step 1))

  (use-package pulseaudio-control
    :bind (("<XF86AudioRaiseVolume>" . pulseaudio-control-increase-volume)
           ("<XF86AudioLowerVolume>" . pulseaudio-control-decrease-volume)
           ("<XF86AudioMute>" . pulseaudio-control-toggle-current-sink-mute)
           :map exwm-mode-map
           ("<XF86AudioRaiseVolume>" . pulseaudio-control-increase-volume)
           ("<XF86AudioLowerVolume>" . pulseaudio-control-decrease-volume)
           ("<XF86AudioMute>" . pulseaudio-control-toggle-current-sink-mute))
    :init (setq pulseaudio-control-volume-step "5%"))

  ;; display time and battery
  (setq display-time-format " %H:%M:%S %a,%d %b ")
  (display-time-mode)
  (use-package fancy-battery :init (fancy-battery-mode))

  ;; TODO: move window to workspace with super+shift+N where N is the
  ;; workspace number to move it to
  ;; TODO: show workspace number in modeline
  ;; TODO: improve battery and time format
  ;; TODO: exwm doesn't start on workspace one
  ;; TODO: enlarge and skrink windows with super+[jklh]

  (require 'exwm-systemtray)
  (exwm-systemtray-enable)

  ;; start in workspace 1
  (setq exwm-workspace-number 4)
  (add-hook 'exwm-init-hook #'(lambda () (exwm-workspace-switch 1)))

  (exwm-enable)

  (exwm-randr-enable)
  (call-process "/bin/bash" "/home/jaymorgan/Applications/startup.sh")
  (exwm-randr--init))

(use-package doom-modeline
  :init (setq doom-modeline-height 20)
  (doom-modeline-mode))
