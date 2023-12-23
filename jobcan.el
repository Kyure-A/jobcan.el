;;; jobcan.el --- Managing jobcan in Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Kyure_A

;; Author: Kyure_A <twitter.com/kyureq>
;; Keywords: tools

;; Version: 0.0.0
;; Package-Requires: ((emacs "25.1") (request "0.3.3") (elquery "1.1.0"))
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

(defvaralias 'night-shift? 'night-shift-p)

;; (jobcan--extract-content-by-name :: (function (string string) string))
(defun jobcan--extract-content-by-name (html-str name)
  "Specify the NAME of an HTML-STR element and extracts the corresponding content."
  (nth 3
       (elquery-props
	(car (elquery-$ (format "[name=%s]" name)
			(elquery-read-string html-str))))))

;; (jobcan--get-csrf-token :: (function () string))
(defun jobcan--get-csrf-token ()
  "Get the CSRF Token required for signing in from jobcan.jp."
  (let ((request-response
	 (request
	   "https://id.jobcan.jp/users/sign_in"
	   :sync t)))
    (jobcan--extract-content-by-name (request-response-data request-response) "csrf-token")))

;; (jobcan--get-value-from-cookie :: (function (string) string))
(defun jobcan--get-value-from-cookie (key)
  "Get value by KEY from cookie."
  (cdr (assoc key (request-cookie-alist "id.jobcan.jp" "/" t))))

;; (jobcan--get-jbcid-session :: (function () string))
(defun jobcan--get-jbcid-session ()
  "Get _jbcid_session from cookie."
  (jobcan--get-value-from-cookie "_jbcid_session"))

;; (jobcan--get-logged-in-users :: (function () string))
(defun jobcan--get-logged-in-users ()
  "Get logged_in_users from cookie."
  (jobcan--get-value-from-cookie "logged_in_users"))

;; (jobcan--get-locale :: (function () string))
(defun jobcan--get-locale ()
  "Get locale from cookie."
  (jobcan--get-value-from-cookie "locale"))

;; (jobcan--get-cookie-string ())
(defun jobcan--get-cookie-string ()
  "Get cookie string."
  (request-cookie-string "id.jobcan.jp" "/" t))

;; (jobcan-login :: (function () string))
(defun jobcan-login ()
  "Login to jobcan."
  (let ((csrf-token (jobcan--get-csrf-token)))
    (request "https://id.jobcan.jp/users/sign_in"
      :sync t
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

;; incomplete
(defun jobcan-status ()
  "Displays information (name and affiliation) of the currently linked user."
  (jobcan-login)
  (let ((request-response
	 (request "https://id.jobcan.jp/account/profile"
	   :type "GET"
	   :sync t
	   :headers `(("Cookie" . ,(jobcan--get-cookie-string))))))
    (request-response-data request-response)))

;; (jobcan-working-p :: (function () bool))
(defun jobcan-working-p ()
  "Return a boolean value if the user is \"working\".")

(defalias 'jobcan-working? 'jobcan-working-p)

;; (jobcan-working-p :: (function () bool))
(defun jobcan-resting-p ()
  "Return a boolean value if the user is \"resting\".")

(defalias 'jobcan-resting? 'jobcan-resting-p)

(provide 'jobcan)
;;; jobcan.el ends here
