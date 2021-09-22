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

(setq gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024)
      auto-save-default nil
      backup-inhibited t
      create-lockfiles nil
      custom-file (concat user-emacs-directory "custom.el")
      revert-without-query '(".*")
      require-final-newline t
      indent-tabs-mode nil
      dired-listing-switches "-alhgo --group-directories-first"
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
(global-auto-revert-mode)

(use-package vertico
  :init (vertico-mode t))

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

(use-package avy)

(use-package expand-region
  :defer nil
  :commands (er/expand-region)
  :bind ("C-=" . er/expand-region))

(use-package multiple-cursors
  :defer nil
  :bind (("C-<" . mc/mark-previous-like-this)
	 ("C->" . mc/mark-next-like-this)))

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

(global-set-key (kbd "C-o") #'insert-line-below)
(global-set-key (kbd "C-S-o") #'insert-line-above)
(global-set-key (kbd "C-c y") #'copy-whole-line)
(global-set-key (kbd "C-z") #'repeat)

(use-package ace-window)
(use-package perspective
  :bind (("C-x k" . persp-kill-buffer*))
  :init (persp-mode))

(use-package popper
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :config
  (setq popper-display-control nil)
  (popper-mode t))

(use-package company
  :hook (after-init . global-company-mode))

(use-package magit)

(use-package vterm
  :config
  (setq vterm-kill-buffer-on-exit t))

(defun vterm-below ()
  "Open a vterm window below"
  (interactive)
  (split-window-below -20)
  (other-window 1)
  (vterm t))

(use-package projectile
  :defer nil
  :bind-keymap ("M-p" . projectile-command-map)
  :init
  (projectile-mode t)
  (setq projectile-project-search-path (list (from-home "workspace/"))))

(use-package undo-tree
  :init
  (global-undo-tree-mode)
  :config
  (setq undo-tree-visualizer-diff t
	undo-tree-visualizer-timestamps t))

(use-package python-mode
  :init
  (setq python-indent-offset 4))

(use-package elpy
  :hook (python-mode . elpy-enable)
  :init
  (setq elpy-rpc-python-command (expand-file-name "~/.bin/miniconda3/bin/python3")))

(use-package pyvenv
  :defer nil
  :hook ((elpy-mode . pyvenv-mode)
	 (projectile-mode . pyvenv-mode))
  :init
  (setenv "WORKON_HOME" (expand-file-name "~/.bin/miniconda3/envs"))
  (pyvenv-mode))

(use-package isend-mode
  :config
  (setq isend-send-region-function 'isend--ipython-cpaste))

(use-package csv-mode)
(use-package yaml-mode)
(use-package markdown-mode)

(use-package ess
  :config
  (setq ess-indent-level 2))

(use-package paredit
  :hook ((lisp-mode . paredit-mode)
	 (emacs-lisp-mode . paredit-mode)))

(use-package lisp-mode
  :straight nil
  :hook ((lisp-mode . show-paren-mode)))

(use-package emacs-lisp-mode
  :straight nil
  :hook ((emacs-lisp-mode . show-paren-mode)))

(use-package auctex
  :ensure auctex)

(use-package slime
  :config
  (setq inferior-lisp-program "sbcl")
  (define-key slime-mode-map (kbd "<f5>") #'slime-selector))

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

(defun conda-activate-once (name)
  "Activate a conda environment only if it is not already set"
  (interactive)
  (unless (string= pyvenv-virtual-env-name name)
    (pyvenv-workon name)))

(setq language-mode->functions
      '((python-mode . ((:format . elpy-black-fix-code)
			(:refacor . elpy-refactor-rename)
			(:goto-definition . elpy-goto-definition)))
	(emacs-lisp-mode . ((:format . nil)))))

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
      rsync-base-cmd "rsync -azm"
      rsync-exclude-list '("data" ".git" "container-dev" "container"
			   "__pycache__" "*.pyc" "renv/library" "renv/local"
			   "renv/python" "renv/staging"))

(defun rsync--build-exclude-list (exclude-list)
  (mapconcat
   (lambda (s) (concat " --exclude=" s " "))
   exclude-list " "))

(defun rsync--cmd (&optional display)
  (let ((exclude-list (rsync--build-exclude-list rsync-exclude-list)))
    (if display
	(concat rsync-base-cmd " --progress " exclude-list)
      (concat rsync-base-cmd exclude-list))))

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
      :unnarrowed t)))
  :init (setq org-roam-v2-ack t)
  :config (org-roam-setup))

(setq org-capture-templates
      `(("f" "Fleeting Note" entry (file ,(from-home "Nextcloud/Notes/fleeting.org"))
	 "* %U\n\n%?" :unnarrowed nil)
	("t" "Todo Entry" entry (file ,(from-home "Nextcloud/Notes/tasks.org"))
	 "* TODO %?\n:PROPERTIES:\n:CREATED: %T\n:END:" :unnarrowed nil)))
(global-set-key (kbd "C-c C-/") 'org-capture)

(use-package org-roam-ui
  :straight (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
	org-roam-ui-follow t
	org-roam-ui-update-on-save t
	org-roam-open-on-start t))

(use-package pdf-tools
  :config
  (pdf-loader-install)
  (setq auto-revert-interval 0.5))

(use-package org-ref
  :commands (org-ref)
  :config
  (setq reftex-default-bibliography (from-home "Nextcloud/Notes/references.bib")
	org-ref-default-bibliography (list (from-home "Nextcloud/Notes/references.bib"))))

(use-package bibtex-actions
  :custom
  (bibtex-completion-bibliography (from-home "Nextcloud/Notes/references.bib"))
  :config
  (use-package all-the-icons)

  (defun bibtex-actions-add-citation (citation)
    "Add a new key to the bibliography file"
    (interactive (list (read-from-minibuffer "Bibtex citation: ")))
    (write-region (concat "\n" citation "\n") nil bibtex-completion-bibliography 'append)
    (bibtex-actions-refresh))

  (defun bibtex-actions-open-library ()
    (interactive)
    (split-window-sensibly)
    (find-file bibtex-completion-bibliography))

  (defun bibtex-actions-add-and-insert-citation (citation)
    "Add a new key to the bibliography and insert citation into buffer"
    (interactive (list (read-from-minibuffer "Bibtex citation: ")))
    (bibtex-actions-add-citation citation)
    (and (string-match "@.*?{\\(.*\\)?," citation)
	 (bibtex-actions-insert-citation (list (match-string 1 citation)))))

  ;; enable font icons -- taken directly from bibtex-actions README
  (setq bibtex-actions-symbols
	`((pdf  . (,(all-the-icons-icon-for-file "foo.pdf" :face 'all-the-icons-dred) .
		   ,(all-the-icons-icon-for-file "foo.pdf" :face 'bibtex-actions-icon-dim)))
	  (note . (,(all-the-icons-icon-for-file "foo.txt") .
		   ,(all-the-icons-icon-for-file "foo.txt" :face 'bibtex-actions-icon-dim)))
	  (link . (,(all-the-icons-faicon "external-link-square" :v-adjust 0.02 :face 'all-the-icons-dpurple) .
		   ,(all-the-icons-faicon "external-link-square" :v-adjust 0.02 :face 'bibtex-actions-icon-dim)))))

  (defface bibtex-actions-icon-dim
    '((((background dark)) :foreground "#282c34")
      (((background light)) :foreground "#fafafa"))
    "Face for obscuring/dimming icons"
    :group 'all-the-icons-faces))

(straight-override-recipe
 '(org :type git :host github :repo "emacsmirror/org" :no-build t))

(use-package org
  :ensure org-plus-contrib
  :config
  (require 'org-ref)
  (require 'bibtex-actions)  
  (require 'pdf-view)
  (require 'ox-latex)
  (use-package gnuplot)
  (pdf-loader-install)

  ;; Slide show setup. First we use org-tree slide to provide the
  ;; basic and critical functionality of the slide show and only show
  ;; one heading at one time.
  (use-package org-tree-slide
    :bind (:map org-mode-map ("<f8>" . org-tree-slide-mode)))

  ;; It's nice to have a mixed pitch (variable-pitch for body text,
  ;; and fixed-pitch for source code) when viewing the slide shows.
  (use-package mixed-pitch
    :hook ((org-tree-slide-mode . mixed-pitch-mode)
	   (org-mode . mixed-pitch-mode)))

  ;; Centre the screen when entering the slide show, and put a fancy
  ;; border around it!
  (use-package olivetti
    :hook (org-tree-slide-mode . olivetti-mode)
    :init
    (setq olivetti-body-width 130
	  olivetti-style 'fancy))
  
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
							   (plantuml . t)))

  ;; darken code blocks to easily distinguish body text from source code
  (require 'color)
  (set-face-attribute 'org-block nil :background (color-darken-name (face-attribute 'default :background) 2))
  (set-face-attribute 'org-block-begin-line nil :background (color-darken-name (face-attribute 'default :background) 4))
  (set-face-attribute 'org-block-end-line nil :background (color-darken-name (face-attribute 'default :background) 4))

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

(use-package flyspell
  :hook ((prog-mode . flyspell-prog-mode)
	 (text-mode . flyspell-mode))
  :init
  (setq flyspell-default-dictionary "british"))

(use-package mu4e
  :commands (mu4e)
  :load-path "/usr/local/share/emacs/site-lisp/mu4e/"
  :bind (:map mu4e-compose-mode-map ("C-c C-a" . mail-add-attachment)
	 :map mu4e-view-mode-map ("C-c C-s" . org-store-link))
  :config
  (let ((mu4e-config (concat user-emacs-directory "mu4e-init.el")))
    (when (file-exists-p mu4e-config)
      (load mu4e-config))))

(use-package alert
  :config
  (setq alert-default-style 'libnotify
	alert-libnotify-command "/usr/bin/notify-send"))

(defmacro my/slack-team (host user)
  "Connect to a team with a token and cookie"
  `(slack-register-team
    :full-and-display-names t
    :name ,(car (split-string host ".slack.com"))
    :token (auth-source-pick-first-password :host ,host :user ,user)
    :cookie (auth-source-pick-first-password :host ,host :user ,(concat user "^cookie"))))

(defmacro load-if-exists (file)
  (let ((load-file (gensym)))
    `(let ((load-file ,(expand-file-name (eval file))))
       (when (file-exists-p load-file)
	 (load load-file)))))

(use-package slack
  :commands (slack-start)
  :config
  (setq slack-buffer-emojify t
	slack-thread-also-send-to-room nil
	lui-time-stamp-format "[%Y-%m-%d %H:%M]"
	lui-time-stamp-only-when-changed-p t)
  (load-if-exists (concat user-emacs-directory "slack-init.el")))

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
	  ("https://protesilaos.com/books.xml" misc)))

  ;; mark entries with a media enclosure (i.e. contains an mp3) as a podcast
  ;; this is taken from https://www.reddit.com/r/emacs/comments/g1o36p/emacs_way_to_listen_podcasts/
  ;; as an easy way to configure elfeed to handle podcast lists
  (defun elfeed-podcast-tagger (entry)
    (when (elfeed-entry-enclosures entry)
      (elfeed-tag entry 'podcast)))

  (add-hook 'elfeed-new-entry-hook #'elfeed-podcast-tagger)

  ;; A list of podcasts to include the elfeed list
  (setq elfeed-feeds
	(append elfeed-feeds
		'("http://feeds.soundcloud.com/users/soundcloud:users:27399956/sounds.rss" ;; cox-n-crendor
		  "https://audioboom.com/channels/5031584.rss" ;; chilluminati
		  ))))

(use-package emms
  :defer nil
  :init
  (require 'emms-setup)
  (emms-all)
  (emms-default-players))

(use-package nov
  :mode ("\\.epub\\'" . nov-mode)
  :straight (nov :type git
		 :repo "https://depp.brause.cc/nov.el.git"
		 :local-repo "nov")
  :config
  (setq nov-text-width 80))

(global-set-key (kbd "C-]") #'join-line)
(global-set-key (kbd "C-x x g") #'revert-buffer)
(global-set-key (kbd "C-;") #'comment-line)
(global-set-key (kbd "C-<tab>") #'expand-abbrev)

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

(set-frame-font "Hack 11" nil t)
(set-face-font 'variable-pitch "Noto Sans")
(setq default-frame-alist '((font . "Hack-11")))

(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)
