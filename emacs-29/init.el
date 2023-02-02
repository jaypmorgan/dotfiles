(setq custom-file (concat user-emacs-directory "custom.el"))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)

(defalias 'yes-or-no-p 'y-or-n-p)	; easier to type a single letter
(set-face-attribute 'default nil :height 90) ; make font slightly smaller

;; need to add:
;; - rsync directory to host
;; - vterm (trying out term/eshell to see if it works as a replacement)

(use-package treesit
  :hook ((python-mode . python-ts-mode)))

(use-package eglot
  :hook ((python-mode . eglot)))

(use-package conda
  :ensure t
  :init
  (defun conda-activate-once (env)
    "Activate an environment if not already activated"
    (interactive)
    (unless (string= env conda-env-current-name)
      (conda-env-activate env))))

(use-package sly
  :ensure t)

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper)))

(use-package corfu
  :ensure t
  :init
  (setq corfu-auto t
	tab-always-indent 'complete)
  (global-corfu-mode))

(use-package vertico
  :ensure t
  :init
  (vertico-mode t))

(use-package paredit
  :ensure t
  :hook ((lisp-mode . paredit-mode)
	 (emacs-lisp-mode . paredit-mode)))

(when (file-exists-p "/usr/local/share/emacs/site-lisp/mu4e/mu4e.el")
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
  (require 'org-mu4e)
  (use-package async :ensure t)
  (setq mail-user-agent 'mu4e-user-agent)
  (setq org-mu4e-convert-to-html t)
  (let ((mu4e-config (concat user-emacs-directory "mu4e-init.el")))
    (when (file-exists-p mu4e-config)
      (load mu4e-config))))
