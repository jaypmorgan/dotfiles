;;; morg-term.el --- Interface for interacting with terminals  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  MORGAN Jay

;; Author: MORGAN Jay <jay@morganwastaken.com>
;; Keywords: extensions, terminals

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

(require 'cl-lib)
(require 'use-package)

(use-package vterm
  :config
  (setq vterm-kill-buffer-on-exit t))

(defgroup morg-term ()
  "Interface with terminals"
  :group 'extensions)

(defcustom morg-term-start-locations nil
  "List of possible locations to start the terminal"
  :type 'list
  :group 'morg-term)

(cl-defun morg-term-send-commands (commands &optional (clear nil))
  (cl-loop for command in commands do
	   (vterm-send-string command)
	   (vterm-send-return))
  (when clear
    (vterm-clear)))

(defun morg-term-vterm-below ()
  "Open a vterm window below"
  (interactive)
  (split-window-below -20)
  (other-window 1)
  (vterm t)
  (activate-projectile-project-in-terminal))

(defun morg-term-activate-projectile-project-in-terminal ()
  (interactive)
  (morg-term-send-commands `(,(format "conda activate %s" pyvenv-virtual-env-name)) t))

(defun morg-term-start-at-location (location)
  (interactive (list (completing-read "Start terminal at: " morg-term-start-locations)))
  (morg-term-vterm-below)
  (morg-term-send-commands `(,(format "ssh %s" location)) t))

(provide 'morg-term)
;;; morg-term.el ends here
