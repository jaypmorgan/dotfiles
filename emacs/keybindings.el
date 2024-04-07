(cl-defmacro morg-keysbinding (&body body)
  (dolist (k body)
    (eval `(global-set-key (kbd ,(car k)) ',(cadr k)))))

(morg-keysbinding
 ("C-]" join-line)
 ("C-o" insert-line-below)
 ("C-S-o" insert-line-above)
 ("M-o" other-window)
 ("C-c l ;" rsync-upload-current-project))

(if (eq system-type 'darwin)
    (global-set-key (kbd "M-3") '(lambda () (interactive) (insert "#"))))
