;; set the font -- only if the font is available though.
(defun morg/configure-font-hook ()
  (let ((font-name "IBM Plex Mono")
        (font-size 12))
    (when (find-font (font-spec :name font-name))
      (add-to-list 'default-frame-alist `(font . ,(format "%s-%s" font-name font-size)))
      (set-face-attribute 'default nil :font font-name)
      (set-frame-font (format "%s %d" font-name font-size) nil t)
      (custom-set-faces `(default ((t (:inherit nil :height ,(* font-size 10) :family font-name)))))
      (message "Set font to: %s" font-name))))

;; apply the font face after each frame is created
;; this hook is necessary for when daemon mode is used.
;(add-hook 'after-make-frame-functions (lambda (frame) (morg/configure-font-hook)))
(morg/configure-font-hook)

