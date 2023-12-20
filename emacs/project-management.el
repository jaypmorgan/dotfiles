;;; project-management.el --- Functions relating to the management of projects, tasks, and time-keeping  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  MORGAN Jay

;; Author: MORGAN Jay <jay.morgan@lis-lab.fr>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(defun morg/open-todo ()
  "Open the TODO file containing the list of tasks and projects"
  (interactive)
  (find-file "~/Nextcloud/Notes/tasks.org"))

(setq org-capture-templates
      `(("f" "Fleeting Note" entry (file "~/Nextcloud/Notes/fleeting.org")
	 "* %U\n\n%?" :unnarrowed nil)
	("t" "Todo Entry" entry (file "~/Nextcloud/Notes/tasks.org")
	 "* TODO %?\n:PROPERTIES:\n:CREATED: %T\n:END:" :unnarrowed nil)
	("b" "Bug Log" entry (file "~/Nextcloud/Notes/bugs.org")
	 "* %T\n\n- Type: %?\n- Severity:\n- What happened:\n" :unnarrowed nil)))
(global-set-key (kbd "C-c C-/") 'org-capture)


;; (provide 'project-management)

;;; project-management.el ends here
