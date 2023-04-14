;; Remove the GUI elements
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; (use-package modus-themes
;;   :ensure t
;;   :init
;;   ;; by default -- let's use the light theme
;;   (setq modus-themes-mixed-fonts t
;;         modus-themes-org-blocks 'gray-background
;;         modus-themes-headings '((1 . (variable-pitch 1.5))
;;                                 (2 . (1.3))
;;                                 (agenda-date . (1.3))
;;                                 (agenda-structure . (variable-pitch light 1.8))
;;                                 (t . (1.1)))
;;         modus-themes-common-palette-overrides '((comment yellow-cooler)
;;                                                 (string green-cooler)))
;;   (load-theme 'modus-operandi t))

;; set the font -- only if the font is available though.
(defun morg/configure-font-hook ()
  (let ((font-name "Lilex Nerd Font Mono")
	(font-size 8))
    ;; (when (find-font (font-spec :name font-name)))
    (add-to-list 'default-frame-alist `(font . ,(format "%s-%s" font-name font-size)))
    (set-face-attribute 'default nil :font font-name)
    (set-frame-font (format "%s %d" font-name font-size) nil t)
    (custom-set-faces `(default ((t (:inherit nil :height ,(* font-size 10) :family font-name)))))
    (message "Set font to: %s" font-name)))

;; apply the font face after each frame is created
;; this hook is necessary for when daemon mode is used.
(add-hook 'after-make-frame-functions (lambda (frame) (morg/configure-font-hook)))
