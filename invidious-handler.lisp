;;;; freestance-handler, a redirector from mainstream websites to their
;;;; privacy-supporting mirrors for the Nyxt browser
;;;; Copyright (C) 2020 kssytsrk
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

;;;; invidious-handler.lisp

(in-package :nx-freestance-handler)

(define-class invidious-instance ()
  ((url (error "Slot `url' must be set")
         :type quri:uri
         :documentation "The URL of the instance.")
   (health nil
           :type (or null float)
           :documentation "The instance uptime as a percentage."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (class*:make-name-transformer name)))

(defmethod prompter:object-attributes ((instance invidious-instance))
  `(("URL" ,(render-url (url instance)))
    ("Health" ,(format nil "~:[N/A~;~:*~,2f%~]" (health instance)))))

(defun get-invidious-instances ()
  (mapcar #'(lambda (json)
              (let ((json (first (rest json))))
                (make-instance 'invidious-instance
                               :url (~>> json
                                         (assoc ':uri) rest
                                         nyxt:url)
                               :health (~>> json
                                            (assoc ':monitor) rest
                                            (assoc ':90-d-ratio) rest
                                            (assoc ':ratio) rest))))
          (cl-json:with-decoder-simple-list-semantics
            (cl-json:decode-json-from-string
             (dex:get "https://api.invidious.io/instances.json?sort_by=health")))))

(defvar *preferred-invidious-instance* nil)

(defun invidious-handler (request-data)
  (let ((url (url request-data)))
    (setf (url request-data)
          (if (or (search "youtube.com" (quri:uri-host url))
                  (search "youtu.be"    (quri:uri-host url)))
              (let ((instance (or *preferred-invidious-instance*
                                  (url (first (get-invidious-instances))))))
                (setf (quri:uri-host url) (quri:uri-host instance)
                      (quri:uri-scheme url) (quri:uri-scheme instance))
                (log:info "Switching to Invidious: ~s" (render-url url))
                url)
              url)))
  request-data)

(in-package :nyxt)

(define-class instance-source (prompter:source)
  ((prompter:name "URL")
   (prompter:constructor (nx-freestance-handler::get-invidious-instances))))

(define-command set-preferred-invidious-instance ()
  "Set the preferred Invidious instance."
  (let ((instance (first (prompt
                          :prompt "Instance"
                          :sources (make-instance 'instance-source)))))
    (setf nx-freestance-handler:*preferred-invidious-instance* (url instance))))
