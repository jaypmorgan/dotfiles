;;; morg-packager.el --- A small wrapper for system package managers  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Jay Morgan

;; Author: Jay Morgan <jay.morgan@lis-lab.fr>
;; Keywords: extensions

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

(defgroup morg-packager ()
  "Manage the system's package manager through emacs"
  :group 'extensions)

(defcustom morg-packager-app-name "apt"
  "The name of the system's package manager"
  :group 'morg-packager)

(defcustom morg-packager-run-as-root t
  "Whether to invoke the package manager as root (sudo)."
  :type 'boolean
  :group 'morg-packager)

(defcustom morg-packager-history-file-enable t
  "Boolean flag to enable the use of history file"
  :type 'boolean
  :group 'morg-packager)

(defcustom morg-packager-history-file-loc (locate-user-emacs-file ".morg-packager.history")
  "Location of the history file that lists past searches or installations"
  :type 'string
  :group 'morg-packager)

(require 'eieio)

(defclass morg-packager-app ()
  ((name :initarg :name :documentation "Name of the package manager")))

(defclass morg-packager-apt (morg-packager-app)
  ((name :initform "apt")))

(cl-defun morg-packager--make-app (&optional (app-name morg-packager-app-name))
  (cond ((or (string= app-name "apt")
	     (string= app-name "apt-get"))
	 (morg-packager-apt))
	(t (error (format "Unknown packager manager %s" app-name)))))

(cl-defmethod morg-packager--make-search ((app morg-packager-apt) search-terms)
  (format "apt-cache search %s" search-terms))

(cl-defmethod morg-packager--make-install ((app morg-packager-apt) search-terms)
  (format "apt-get install %s" search-terms))

(defun morg-packager--get-app (name)
  (cond ((string= name "apt") morg-packager-apt)
	(t (error (format "No package manager setup for %s." name)))))

(defun morg-packager--format-command (cmd root-p)
  (format "%s%s" (or (and root-p "sudo ") "") cmd))

(defun morg-packager--execute-command (cmd)
  (async-shell-command cmd "*Packager search*"))

(cl-defun morg-packager-search (search-terms &optional (root-p nil))
  "Search for SEARCH-TERMS"
  (interactive (list (completing-read "Search term(s): "
				 (morg-packager--read-from-history
				  morg-packager-history-file-loc)
				 nil nil)))
  (morg-packager--add-to-history-file search-terms)
  (morg-packager--execute-command
   (morg-packager--format-command
    (morg-packager--make-search (morg-packager--make-app) search-terms)
    root-p)))

(defun morg-packager-install (search-terms)
  "Install SEARCH-TERMS"
  (interactive (list (completing-read "Install package(s): "
				 (morg-packager--read-from-history
				  morg-packager-history-file-loc)
				 nil nil)))
  (morg-packager--add-to-history-file search-terms)
  (morg-packager--execute-command
   (morg-packager--format-command
    (morg-packager--make-install (morg-packager--make-app) search-terms)
    morg-packager-run-as-root)))

(defun morg-packager--make-history-file (history-file)
  "Make an empty history file at HISTORY-FILE"
  (make-empty-file history-file t))

(defun morg-packager--read-from-history (history-file)
  (split-string
   (with-temp-buffer
     (insert-file-contents history-file)
     (buffer-string))
   "\n" t))

(cl-defun morg-packager--add-to-history-file
    (search-terms &optional (history-file morg-packager-history-file-loc))
  "Add the search term to the end of the history file"
  (unless (file-exists-p history-file)
    (morg-packager--make-history-file history-file))
  (dolist (search-term (split-string search-terms " " t))
    (write-region (string-remove-suffix  ;; remove any leading newlines
		   "\n"
		   (or (and (string-prefix-p "\n" search-term) search-term) ;; contains newline
		       (format "\n%s" search-term)))  ;; newline must be added
		  nil history-file 'append)))

(provide 'morg-packager)
;;; morg-packager.el ends here
