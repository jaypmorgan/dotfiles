;;; morg-pomodoro.el --- Simple pomodoro timer using org-timer  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  MORGAN Jay

;; Author: MORGAN Jay <jay@morganwastaken.com>
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

(require 'org)

(defgroup morg-pomodoro ()
  "Simple pomodoro timer using org-timer"
  :group 'extensions)

(defcustom morg-pomodoro-time-work "00:25:00"
  "Amount of time to work in format (hh:mm:ss)."
  :type 'string)

(defcustom morg-pomodoro-time-rest "00:05:00"
  "Amount of time to rest in format (hh:mm:ss)."
  :type 'string)

(setq morg-pomodoro--status 'rest) ;; begin at rest

;;;###autoload
(defun morg-pomodoro-start ()
  (interactive)
  (if (eq morg-pomodoro--status 'rest)
      (and (org-timer-set-timer morg-pomodoro-time-work)
	   (setq morg-pomodoro--status 'work))
    (and (org-timer-set-timer morg-pomodoro-time-rest)
	 (setq morg-pomodoro--status 'rest))))

;;;###autoload
(defun morg-pomodoro-pause-unpause ()
  (interactive)
  (org-timer-pause-or-continue))

;;;###autoload
(defun morg-pomodoro-stop ()
  (interactive)
  (setq morg-pomodoro--status 'rest)
  (org-timer-stop))

(provide 'morg-pomodoro)
;;; morg-pomodoro.el ends here
