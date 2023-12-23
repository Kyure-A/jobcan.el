;;; jobcan.el --- Managing jobcan in Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Kyure_A

;; Author: Kyure_A <twitter.com/kyureq>
;; Keywords: tools

;; Version: 0.0.0
;; Package-Requires: ((emacs "24.1"))
;; URL: https://github.com/Kyure-A/jobcan.el

;; SPDX-License-Identifier:  GPL-3.0-or-later

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

;; Managing jobcan in Emacs

;;; Code:

(require 'auth-source)
(require 'request)
(require 'elquery)

(defgroup jobcan ()
  "Managing jobcan in Emacs."
  :group 'tools
  :prefix "jobcan-"
  :link '(url-link "https://github.com/Kyure-A/jobcan.el"))

(defcustom night-shift-p nil
  "Whether the user works the night shift or not."
  :type 'boolean)

;; (jobcan--extract-content-by-name :: (function (string string) string))
(defun jobcan--extract-content-by-name (html-str name)
  ""
  (nth 3
       (elquery-props
	(car (elquery-$ (format "[name=%s]" name)
			(elquery-read-string html-str))))))

;; (jobcan--get-csrf-token :: (function () string))
(defun jobcan--get-csrf-token ()
  ""
  (let ((request-response
	 (request
	   "https://id.jobcan.jp/users/sign_in"
	   :sync t)))
    (jobcan--extract-content-by-name (request-response-data request-response) "csrf-token")))

(defun jobcan-credential ()
  "Credential."
  )

;; (jobcan-login :: (function () string))
(defun jobcan-login ()
  "Punch in to jobcan."
  (let ((csrf-token (jobcan--get-csrf-token)))
    (request "https://id.jobcan.jp/users/sign_in"
      :sync t
      :type "POST"
      :data `(("authenticity_token" . ,csrf-token)
	      ("user[email]" . "") ;; credential
	      ("user[client_code]" . "")
	      ("user[password]" . "") ;; credential
	      ("save_sign_in_information" . "true")
	      ("app_key" . "atd")
	      ("commit" .  "ログイン"))
      :complete (cl-function
		 (lambda (&key resp &allow-other-keys))))))

;; "<span>Total: 12:59<span class=\"d-inline-block ml-4\">Break: 17:19</span><span class=\"d-inline-block ml-4\">Overtime Work: 0:00</span><span class=\"d-inline-block ml-4\">Night Shifts: 0:00</span></span>" -> ("Total: 12:59" "Break: 17:19" "Overtime Work: 0:00" "Night Shifts: 0:00")
;; (jobcan--parse-top-informations :: (function (string) (list string)))
(defun jobcan--parse-top-informations (load-top-info)
  "Parse the html that can be obtained from LOAD-TOP-INFO (the monthly in load-top-informations)."
  (mapcar #'elquery-text
	  (elquery-$ "span" (elquery-read-string load-top-info))))

;; (jobcan-get-top-informations :: (function () (list string)))
(defun jobcan-get-top-informations ()
  ""
  (let ((request-response nil))
    (request "https://ssl.jobcan.jp/employee/index/adit"
      :type "POST"
      :headers '(("User-Agent" . "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/51.0.2704.103 Safari/537.36"))
      :data '(("is_yakin" . (if night-shift-p 1 0))
	      )
      :encoding 'utf-8
      :parser 'json-read
      :complete (cl-function
		 (lambda (&key resp &allow-other-keys)
		   (setf request-response resp))))
    (jobcan--parse-top-informations (request-response-data request-response))))

(defun jobcan-status ()
  "Retrieve status from jobcan.")

;; (jobcan-working-p :: (function () bool))
(defun jobcan-working-p ()
  "Working?")

(defalias 'jobcan-working? 'jobcan-working-p)

;; (jobcan-working-p :: (function () bool))
(defun jobcan-resting-p ()
  "Resting?")

(defalias 'jobcan-resting? 'jobcan-resting-p)

(provide 'jobcan)
;;; jobcan.el ends here
