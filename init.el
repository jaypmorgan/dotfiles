;;--------------------------
;; EMACS Configuration File
;; Author: Jay Morgan
;;--------------------------

(load (concat user-emacs-directory "hiddens.el") 'noerror)

;; Config settings are available in an org-mode
;; file. This function call loads them.
(require 'org)
(org-babel-load-file
 (expand-file-name "config.org" user-emacs-directory))

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; Remove the GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
