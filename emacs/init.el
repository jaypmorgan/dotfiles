;;--------------------------
;; EMACS Configuration File
;; Author: Jay Morgan
;;--------------------------

(setq warning-minimum-level :error)
(load (concat user-emacs-directory "hiddens.el") 'noerror)

;; Load the configuration settings
;; These settings are tangled from config.org
;; literature config
(load (concat user-emacs-directory "config.el"))

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;; Remove the GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
