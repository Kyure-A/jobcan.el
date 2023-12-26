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
(require 'elquery)
(require 'request)
(require 's)


(defgroup jobcan ()
  "Managing jobcan in Emacs."
  :group 'tools
  :prefix "jobcan-"
  :link '(url-link "https://github.com/Kyure-A/jobcan.el"))

(defcustom night-shift-p nil
  "Whether the user works the night shift or not."
  :type 'boolean)

(defcustom jobcan-default-adit-group-id 6
  "Adit group id.  1: ?, 2: ?, 3: ? 4: ? 5: ? 6: 業務委託契約."
  :type 'number)

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

;; (jobcan--get-adit-token :: (function () string))
(defun jobcan--get-adit-token ()
  "Get the Adit token required for aditting."
  (let ((request-response
	 (request "https://ssl.jobcan.jp/employee"
	   :sync t
	   :headers `(("Cookie" . ,(jobcan--get-ssl-cookie-string))))))
    (nth 5 (nth 5 (car (elquery-$ "[name=token]" (elquery-read-string (request-response-data request-response))))))))

;; (jobcan--get-top-informations :: (function () (list string)))
(defun jobcan--get-top-informations ()
  "Get working time."
  (jobcan-login)
  (let ((request-response
	 (request "https://ssl.jobcan.jp/employee/index/load-top-informations"
	   :sync t
	   :headers `(("Cookie" . ,(jobcan--get-ssl-cookie-string))))))
    (request-response-data request-response)))

;; (jobcan--get-linked :: (function () (cons string string)))
(defun jobcan--get-linked ()
  "Get information (name and affiliation) of the currently linked user."
  (jobcan-login)
  (let ((request-response
	 (request "https://id.jobcan.jp/account/profile"
	   :type "GET"
	   :sync t
	   :headers `(("Cookie" . ,(jobcan--get-id-cookie-string))))))
    (cons
     (elquery-text (nth 6 (elquery-$ "td" (elquery-read-string (request-response-data request-response)))))
     (elquery-text (nth 1 (elquery-$ "td" (elquery-read-string (request-response-data request-response))))))))

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

;; (jobcan--get-id-cookie-string :: (function () string))
(defun jobcan--get-id-cookie-string ()
  "Get cookie string of id.jobcan.jp."
  (request-cookie-string "id.jobcan.jp" "/" t))

;; (jobcan--get-ssl-cookie-string :: (function () string))
(defun jobcan--get-ssl-cookie-string ()
  "Get cookie string of ssl.jobcan.jp."
  (request-cookie-string "ssl.jobcan.jp" "/" t))

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
	      ("commit" .  "ログイン")))))

;; incomplete
(defun jobcan-touch (&rest notice)
  "Enter NOTICE as a comment (blanks allowed) and imprint."
  (jobcan-login)
  (request "https://ssl.jobcan.jp/employee/index/adit"
    :sync t
    :headers `(("Cookie" . ,(jobcan--get-ssl-cookie-string)))
    :data `(("is_yakin" . 0)
	    ("adit-item" . "DEF")
	    ("notice" . ,(unless notice ""))
	    ("token" . ,(jobcan--get-adit-token))
	    ("adit_group_id" . jobcan-default-adit-group-id)
	    ("_" . ""))))

;; (jobcan-top-informations :: (function (string) (list string)))
(defun jobcan-top-informations ()
  "Parse the html that can be obtained from LOAD-TOP-INFO (the monthly in load-top-informations)."
  (reverse (mapcar #'elquery-text
		   (elquery-$ "span" (elquery-read-string (jobcan--get-top-informations))))))

;; (jobcan--linked :: (function () ()))
(defun jobcan-linked ()
  "Displays information (name and affiliation) of the currently linked user."
  (interactive)
  (let ((status (jobcan--get-linked)))
    (if (string= (jobcan--get-locale) "ja")
	(message "%s 所属の %s さんと連携しています" (cdr status) (car status))
      (message "Linked to %s's account (they are member of %s)" (car status) (cdr status)))))

;; (jobcan--eval-js :: (function (string) (string)))
(defun jobcan--eval-js (script objective)
  "Create a js file to be passed to node.js with SCRIPT to retrieve current_status."
  (let ((temp-js (make-temp-file "jobcan" nil ".js")))
    (with-temp-file temp-js
      (insert (concat script ";" (s-lex-format "console.log(${objective})"))))
    (let ((response
	   (s-chomp (with-temp-buffer (call-process-shell-command "deno" nil t nil "run" temp-js)
				      (buffer-string)))))
      (delete-file temp-js)
      response)))

;; (jobcan--get-current-status :: (function () (string)))
(defun jobcan--get-current-status ()
  "Get current_status."
  (jobcan-login)
  (let ((request-response
	 (request "https://ssl.jobcan.jp/employee"
	   :sync t
	   :headers `(("Cookie" . ,(jobcan--get-ssl-cookie-string))))))
    (nth 0
	 (s-split "function"
		  (elquery-text
		   (nth 9 (elquery-$ "script" (elquery-read-string (request-response-data request-response)))))))))

;; incomplete
(defun jobcan-current-status ()
  "Displays current_status."
  (interactive)
  (if (executable-find "deno")
      (progn
	(let ((current-status (jobcan--eval-js (jobcan--get-current-status) "current_status")))
	  (message "%s" current-status)
	  current-status))
    (message "deno is not found. Please install it.")
    nil))

;; (jobcan-working-p :: (function () bool))
(defun jobcan-working-p ()
  "Return a boolean value if the user is \"working\"."
  (string= (jobcan-current-status) "working"))

(defalias 'jobcan-working? 'jobcan-working-p)

;; (jobcan-resting-p :: (function () bool))
(defun jobcan-resting-p ()
  "Return a boolean value if the user is \"resting\"."
  (string= (jobcan-current-status) "resting"))

(defalias 'jobcan-resting? 'jobcan-resting-p)

;; (jobcan-having-breakfast-p :: (function () bool))
(defun jobcan-having-breakfast-p ()
  "Return a boolean value if the user is \"having_breakfast\" (not yet at work)."
  (string= (jobcan-current-status) "having_breakfast"))

(defalias 'jobcan-having-breakfast? 'jobcan-having-breakfast-p)

(provide 'jobcan)
;;; jobcan.el ends here
