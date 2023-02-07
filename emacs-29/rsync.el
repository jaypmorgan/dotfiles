;; Projectile level syncing between local and remote hosts
;; set the initial variables to nil
;; .dir-local.el should set these at a project level
(setq rsync-source nil
      rsync-destination nil
      rsync-base-cmd "rsync -am"
      rsync-exclude-list '("data" "container-dev" "container"
			   "__pycache__" "*.pyc" "renv/library" "renv/local"
			   "renv/python" "renv/staging" "build" "dist"))

(defun rsync--build-exclude-list (exclude-list)
  (mapconcat
   (lambda (s) (concat " --exclude=" s " "))
   exclude-list " "))

(defun rsync--cmd (&optional display)
  (let ((exclude-list (rsync--build-exclude-list rsync-exclude-list)))
    (if display
	(concat rsync-base-cmd " --progress " exclude-list)
      (concat rsync-base-cmd exclude-list))))

(defun select-rsync-destination (dest)
  (interactive (list (completing-read "Destination: " *available-destinations*)))
  (setq rsync-destination dest))

(defun dorsync (src dest is_hidden)
  "Launch an asynchronuous rsync command"
  (interactive)
  (let ((async-value async-shell-command-display-buffer))
    (if is_hidden
        (progn
            (setq async-shell-command-display-buffer nil)
            (setq rsync-cmd (rsync--cmd)))
      (setq rsync-cmd (rsync--cmd t)))
    (async-shell-command (concat rsync-cmd " " src " " dest) (format "*Rsync log: %s*" dest))
    (setq async-shell-command-display-buffer async-value)))

(defun dorsync-all-destinations (src is_hidden)
  "Rsync to all available destinations"
  (interactive)
  (dolist (dest *available-destinations*)
    (dorsync src dest is_hidden)))
