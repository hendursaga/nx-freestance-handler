;;;; freestance-handler, a redirector from mainstream websites to their
;;;; privacy-supporting mirrors for the Nyxt browser
;;;; Copyright (C) 2021 Hendursaga

;;;; This program is free software: you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation, either version 3 of the License, or
;;;; (at your option) any later version.

;;;; This program is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.

;;;; You should have received a copy of the GNU General Public License
;;;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;;; common.lisp

(in-package :nx-freestance-handler)

(define-class freestance-instance ()
  ((url (error "Slot `url' must be set")
         :type quri:uri
         :documentation "The URL of the instance.")
   (health nil
           :type (or null float)
           :documentation "The instance uptime as a percentage, between 0 and 100."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod prompter:object-attributes ((instance freestance-instance))
  `(("URL" ,(render-url (url instance)))
    ("Health" ,(format nil "~:[N/A~;~:*~,2f%~]" (health instance)))))
