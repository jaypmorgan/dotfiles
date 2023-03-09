; (load-theme 'leuven t)

;; set the font -- only if the font is available though.
(defun morg/configure-font ()
  (let ((font-name "IBM Plex Mono")
	(font-size 9))
    (when (find-font (font-spec :name font-name))
      (add-to-list 'default-frame-alist `(font . (format "%s-%s" font-name font-size)))
      (set-face-attribute 'default nil :font font-name)
      (set-frame-font (format "%s %d" font-name font-size) nil t)
      (custom-set-faces `(default ((t (:inherit nil :height ,(* font-size 10) :family font-name)))))
      (message "Set font to: %s" font-name))))

(morg/configure-font)

