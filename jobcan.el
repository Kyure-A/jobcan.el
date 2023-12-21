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

(defgroup jobcan ()
  "Managing jobcan in Emacs."
  :group 'tools
  :prefix "jobcan-"
  :link '(url-link "https://github.com/Kyure-A/jobcan.el"))

(defun jobcan-credential ()
  "Credential."
  )

(defun jobcan-touch ()
  "Punch in to jobcan."
  (let (())
    ))

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
