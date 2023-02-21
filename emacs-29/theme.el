(load-theme 'leuven-dark t)

;; set the font -- only if the font is available though.
(let ((font-name "Berkeley Mono Trial"))
  (if (find-font (font-spec :name font-name))
      (set-frame-font (format "%s 9" font-name) nil t)))
