(setq auto-save-default nil
      backup-inhibited t
      create-lockfiles nil
      custom-file (concat user-emacs-directory "custom.el")
      revert-without-query t
      require-final-newline t
      indent-tabs-mode nil
      dired-listing-switches "-alhgo --group-directories-first"
      ring-bell-function 'ignore)

;; setup straight.el instead of package.el
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

(use-package company
  :hook (after-init . global-company-mode))

(use-package projectile
  :defer nil
  :bind-keymap ("M-p" . projectile-command-map)
  :init
  (projectile-mode t)
  (setq projectile-project-search-path '("/media/hdd/workspace/")))

(use-package vertico
  :init
  (use-package marginalia
    :init
    (marginalia-mode))
  (vertico-mode t))

(use-package orderless
  :init
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	completion-category-overrides '((file (styles basic partial-completion)))))

(defun insert-line-above ()
  "Insert and indent to the next line"
  (interactive)
  (beginning-of-visual-line)
  (newline-and-indent)
  (previous-line)
  (indent-according-to-mode))

(defun insert-line-below ()
  "Insert and indent to the previous line"
  (interactive)
  (end-of-visual-line)
  (newline-and-indent))

(global-set-key (kbd "C-o") #'insert-line-below)
(global-set-key (kbd "C-S-o") #'insert-line-above)

(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'isearch-forward 'isearch-forward-regexp)
(defalias 'isearch-backward 'isearch-backward-regexp)

(use-package python-mode
  :init
  (setq python-indent-offset 2))

(use-package elpy
  :hook (python-mode . elpy-enable))

(use-package pyvenv
  :hook (elpy-mode . pyvenv-mode)
  :init
  (setenv "WORKON_HOME" (expand-file-name "~/miniconda3/envs")))

(use-package ess)
(use-package yaml-mode)
(use-package markdown-mode)

(use-package slurp-mode
  :straight (slurp-mode :type git :host github :repo "jaypmorgan/slurp-mode")
  :init
  (setq slurp-repl-location "~/workspace/slurp/slurp"))

(use-package slurp-repl-mode
  :straight (slurp-repl-mode :type git :host github :repo "jaypmorgan/slurp-mode")
  :bind (:map slurp-mode-map
	      ("C-c C-c" . slurp-repl-send-line)
	      ("C-c C-z" . run-slurp-other-window)))

(use-package plantuml-mode
  :mode ("\\.plantuml\\'" . plantum-mode)
  :init
  (unless (file-exists-p (expand-file-name "~/plantuml.jar"))
    (switch-to-buffer (make-temp-name "plantuml"))
    (ignore-errors (plantuml-mode))
    (plantuml-download-jar))
  (setq plantuml-jar-path (expand-file-name "~/plantuml.jar")
        plantuml-default-exec-mode 'jar
        org-plantuml-jar-path plantuml-jar-path))

(defun conda-activate-once (name)
  (interactive))

;;;;;;;;;;;;;;;
;; Org mode  ;;
;;;;;;;;;;;;;;;

(use-package pdf-tools
  :config
  (pdf-loader-install)
  (setq auto-revert-interval 0.5))

(use-package org-ref
  :defer nil
  :commands (org-ref)
  :config
  (setq reftex-default-bibliography "~/Nextcloud/Notes/Wiki/library.bib"
        org-ref-pdf-directory "~/Nextcloud/Notes/Wiki/Papers/"
        org-ref-default-bibliography '("~/Nextcloud/Notes/Wiki/library.bib")))

(use-package org
  :ensure org-plus-contrib
  :config
  (setq	org-hide-emphasis-markers t
	org-edit-src-content-indentation 0
	org-footnote-auto-adjust t
	org-confirm-babel-evaluate nil
        org-latex-prefer-user-labels t
        org-latex-pdf-process '("latexmk -shell-escape -bibtex -f -pdf %f"))
  (add-hook 'org-mode-hook #'(lambda ()
                              (set-fill-column 85)
                              (visual-line-mode 1)
                              (auto-fill-mode 1)))
  
  (require 'pdf-view)
  (require 'ox-latex)

  (add-to-list 'org-latex-classes
            '("book-no-parts"
                "\\documentclass{book}"
                ("\\chapter{%s}" . "\\chapter*{%s}")
                ("\\section{%s}" . "\\section*{%s}")
                ("\\subsection{%s}" . "\\subsection*{%s}")
                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                ("\\paragraph{%s}" . "\\paragraph*{%s}")))

  (org-babel-do-load-languages 'org-babel-load-languages '((shell . t)
							   (python . t)
							   (R . t)
							   (plantuml . t)))

  (require 'color)
  (set-face-attribute 'org-block nil :background
		      (color-darken-name (face-attribute 'default :background) 3))
  
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
  (define-key org-mode-map (kbd "<f5>") #'org-latex-export-to-pdf)
  (define-key org-mode-map (kbd "<f3>") #'my/open-to-odf-other-window)
  (define-key org-mode-map (kbd "C-<right>") #'org-babel-next-src-block)
  (define-key org-mode-map (kbd "C-<left>") #'org-babel-previous-src-block))

(use-package bibtex-actions
  :custom
  (bibtex-completion-bibliography bib-file-loc)
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

  (setq test-string "@inproceeding{thisisatesrkjerkj,")
  (and (string-match "{\\(.*\\)?," test-string)
       (match-string 1 test-string))

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

  ;; Here we define a face to dim non 'active' icons, but preserve alignment
  (defface bibtex-actions-icon-dim
      '((((background dark)) :foreground "#282c34")
      (((background light)) :foreground "#fafafa"))
      "Face for obscuring/dimming icons"
      :group 'all-the-icons-faces))

(use-package flyspell
  :init
  (setq flyspell-default-dictionary "british"))

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

(use-package mu4e
  :commands (mu4e)
  :load-path  "/usr/local/share/emacs/site-lisp/mu4e/"
  :bind (:map mu4e-compose-mode-map
	      ("C-c C-a" . mail-add-attachment)
	 :map mu4e-view-mode-map
	      ("C-c C-s" . org-store-link))
  :config
  ;;(define-key mu4e-compose-mode-map (kbd "C-c C-a") #'mail-add-attachment)
  ;;(define-key mu4e-view-mode-map (kbd "C-c C-s") #'org-store-link)
  ;; load the configuration details
  (let ((mu4e-config (concat user-emacs-directory "mu4e-init.el")))
    (when (file-exists-p mu4e-config)
      (load mu4e-config))))

(use-package calendar
  :hook (diary-list-entries . diary-sort-entries)
  :bind (:map calendar-mode-map
	      ("C-x i" . diary-insert-entry))
  :config
  (setq diary-file "~/Nextcloud/Notes/diary"
	calendar-date-style "iso"
	appt-display-mode-line t
	org-agenda-diary-file "~/Nextcloud/Notes/diary"
	org-agenda-include-diary t))

(use-package elfeed
  :init
  ;; https://www.theinsaneapp.com/2021/04/top-machine-learning-blogs-to-follow-in-2021.html
  (setq elfeed-feeds
        '("https://ruder.io/rss/index.rss"
          "https://karpathy.github.io/feed.xml"
          "https://lilianweng.github.io/lil-log/feed.xml"
          "https://machinelearningmastery.com/feed/"
          "http://blog.shakirm.com/feed/")))

(use-package general)
(use-package avy)
(use-package magit)
(use-package vterm)

(defmacro dofn (func)
  "macro to make lambda shorter"
  `(lambda ()
     (interactive)
     ,func))

(general-define-key
 :prefix "C-c"
 "a" #'org-agenda
 "n" #'avy-goto-char-2
 "p" #'projectile-command-map
 ;; remote hosts
 "r l" #'(lambda () (interactive) (find-file "/ssh:lis.me:"))
 ;; open maps
 "o t" #'(lambda () (interactive) (find-file "/media/hdd/Nextcloud/Notes/tasks.org"))
 "o s" #'(lambda () (interactive) (vterm t))
 "o c" #'(lambda () (interactive) (find-file (concat user-emacs-directory "init.el")))
 "o C" #'calendar
 "o m" #'mu4e
 "o e" #'elfeed)
