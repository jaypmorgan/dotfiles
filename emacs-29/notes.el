;; Perhaps the set of plugins that most uses external/complex
;; packages.

(let ((notes-dir (file-truename "~/Nextcloud/Notes/")))

  (defun note-path (name)
    (concat notes-dir "/" name))

  (use-package org-roam
    :ensure t
    :init
    (setq org-roam-directory (note-path "BIOSOFT"))
    (org-roam-db-autosync-mode))

  (use-package consult-org-roam
    :ensure t
    :defer t
    :init
    (require 'consult-org-roam)
    (consult-org-roam-mode t)
    :bind (("C-c f" . consult-org-roam-file-find)
	   ("C-c s" . consult-org-roam-search)))

  (use-package org-roam-ui
    :ensure t
    :after org-roam
    :config
    (setq org-roam-ui-sync-theme t
	  org-roam-ui-follow t
	  org-roam-ui-update-on-save nil))

  (use-package citar
    :ensure t
    :bind (("C-c b" . citar-open))
    :init
    (setq citar-bibliography (list (note-path "references.bib"))
	  citar-notes-paths (list (note-path "references"))))

 (use-package citar-org-roam
   :ensure t
   :after citar
   :init (citar-org-roam-mode t)))
