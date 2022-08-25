;;; morg-monitor.el --- Control monitor brightness from emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Jay Morgan

;; Author: Jay Morgan <jay@morganwastaken.com>
;; Keywords: extensions
;; Version: 0.1
;; Homepage: https://github.com/jaypmorgan
;; Package-Requires: ((emacs "26.1"))

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

;;; Code:

(require 'cl-lib)
(require 'notifications)

(defgroup morg-monitor ()
  "Control monitor brightness"
  :group 'extensions)

(defun morg-monitor--parse-brightness-file (filename)
  (with-temp-buffer
    (insert-file-contents-literally (format "%s/%s" morg-monitor-brightness-root filename))
    (cl-parse-integer (buffer-substring-no-properties (point-min) (point-max)))))

(defcustom morg-monitor-brightness-root "/sys/class/backlight/intel_backlight"
  "The root path to the brightness files."
  :type 'string
  :group 'morg-monitor)

(defcustom morg-monitor-max-brightness (morg-monitor--parse-brightness-file "max_brightness")
  "The maximum brightness the monitor is capable of. This can be
found in the max_brightness file of the system."
  :type 'integer
  :group 'morg-monitor)

(defcustom morg-monitor-step-size 10
  "The amount of change in brightness (absolute) to make."
  :type 'integer
  :group 'morg-monitor)

(defcustom morg-monitor-message-fn #'morg-monitor--alert-message
  "The function to call after the monitor brightness has been
changed. This function should alert the user about the currently
set brightness."
  :type 'function
  :group 'morg-monitor)

(defun morg-monitor--get-current-brightness ()
  (morg-monitor--parse-brightness-file "brightness"))

(defun morg-monitor--set-brightness (val)
  (cl-flet ((set-val (val)
		     (call-process-shell-command
		      (format "echo %s > %s/brightness" val morg-monitor-brightness-root)
		      nil 0)))
    (cond ((>= val morg-monitor-max-brightness) (set-val morg-monitor-max-brightness))
	  ((<= val 0) (set-val 0))
	  (t (set-val val)))))

(cl-defun morg-monitor--change-brightness (fun amt)
  (if (null amt)
      (morg-monitor--set-brightness (funcall fun (morg-monitor--get-current-brightness) morg-monitor-step-size))
    (morg-monitor--set-brightness (funcall fun (morg-monitor--get-current-brightness) amt)))
  (funcall morg-monitor-message-fn))

(defvar morg-monitor--notification-id nil)

(defun morg-monitor--alert-message ()
  (setq morg-monitor--notification-id
	(notifications-notify
	 :title "Monitor brightness"
	 :replaces-id morg-monitor--notification-id
	 :body (format "Brightness set to: %.2f%% (%s/%s)"
		       (* (/ (float (morg-monitor--get-current-brightness)) morg-monitor-max-brightness) 100)
		       (morg-monitor--get-current-brightness)
		       morg-monitor-max-brightness))))

(defun morg-monitor--print-message ()
  (message
   "Brightness: %.2f%% (%s/%s)"
   (* (/ (float (morg-monitor--get-current-brightness)) morg-monitor-max-brightness) 100)
   (morg-monitor--get-current-brightness)
   morg-monitor-max-brightness))

;;;###autoload
(defun morg-monitor-increase-brightness ()
  (interactive)
  (morg-monitor--change-brightness #'+ morg-monitor-step-size))

;;;###autoload
(defun morg-monitor-decrease-brightness ()
  (interactive)
  (morg-monitor--change-brightness #'- morg-monitor-step-size))


(provide 'morg-monitor)

;;; morg-monitor.el ends here
