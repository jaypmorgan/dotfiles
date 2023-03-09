;; Perhaps the set of plugins that most uses external/complex
;; packages.

(let ((notes-dir (file-truename "~/Nextcloud/Notes/")))

  (defun note-path (name)
    (concat notes-dir "/" name))

  (use-package citar
    :ensure t
    :bind (("C-c b" . citar-open))
    :init
    (setq citar-bibliography (list (note-path "references.bib"))
	  citar-notes-paths (list (note-path "references"))))

  (use-package denote
    :ensure t
    :bind (("C-c n c" . denote))
    :init
    (setq denote-directory (note-path "Denote")))

  (use-package citar-denote
    :ensure t
    :bind (("C-c n o" . citar-denote-open-note)
	   ("C-c n n" . citar-create-note))
    :after citar denote
    :init
    (citar-denote-mode)
    (setq citar-open-always-creates-note t)))
