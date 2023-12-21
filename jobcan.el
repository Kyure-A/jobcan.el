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

(defun jobcan--extract-content-by-name (html-str name) ;; html-str: string, name: string -> string
  (nth 3
       (elquery-props
	(car (elquery-$ (format "[name=%s]" name)
			(elquery-read-string html-str))))))

(defun jobcan--get-csrf-token () ;; void -> string
  (let ((request-response nil))
    (request "https://id.jobcan.jp/users/sign_in"
      :complete (cl-function
		 (lambda (&key resp &allow-other-keys)
		   (setf request-response resp)
		   (message "%s" (request-response-data request-response)))))
    (jobcan--extract-content-by-name (request-response-data request-response))))

(defun jobcan-credential ()
  "Credential."
  )

(defun jobcan-login () ;; void -> string
  "Punch in to jobcan."
  (let ((request-response nil))
    (request "https://id.jobcan.jp/users/sign_in"
      :type "POST"
      :data '(("authenticity_token" . (jobcan--extract-content-by-name (request-response-data resp) "csrf-token"))
	      ("user[email]" . "") ;; credential
	      ("user[client_code]" . "")
	      ("user[password]" . "") ;; credential
	      ("save_sign_in_information" . "true")
	      ("redirect_uri" . "https://ssl.jobcan.jp/jbcoauth/callback")
	      ("app_key" . "atd")
	      ("commit" .  "ログイン"))
      :parser 'json-read
      :complete (cl-function
		 (lambda (&key resp &allow-other-keys)
		   (setf request-response resp))))))

(defun jobcan--parse-top-informations (load-top-info) ;; load-top-info: string -> list<string>
  (mapcar #'elquery-text
	  (elquery-$ "span" (elquery-read-string load-top-info))))

;; "<span>Total: 12:59<span class=\"d-inline-block ml-4\">Break: 17:19</span><span class=\"d-inline-block ml-4\">Overtime Work: 0:00</span><span class=\"d-inline-block ml-4\">Night Shifts: 0:00</span></span>" -> ("Total: 12:59" "Break: 17:19" "Overtime Work: 0:00" "Night Shifts: 0:00")
(defun jobcan-get-top-informations ()
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

(defun jobcan-working-p ()
  "Working?")

(defalias 'jobcan-working? 'jobcan-working-p)

(defun jobcan-resting-p ()
  "Resting?")

(defalias 'jobcan-resting? 'jobcan-resting-p)

(provide 'jobcan)
;;; jobcan.el ends here
