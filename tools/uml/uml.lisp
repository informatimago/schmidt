;;;; uml.lisp

;; This file is part of UML library.

;; UML is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; UML is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with UML.  If not, see <http://www.gnu.org/licenses/>.

(in-package #:uml)

;;; "uml" goes here. Hacks and glory await!

(defstruct (use-case
            (:conc-name uc-))
  name
  primary-actor
  goal-in-context
  scope
  level
  brief-description
  pre-condition
  post-condition
  basic-flow
  alternate-flow
  exceptional-flow)


(defparameter *use-cases* (make-hash-table :test #'equal))

;;
;; Macros.
;;

;; actor classifier extend extension-point include

(defmacro define-use-case (name &rest parameters &key &allow-other-keys)
  "Only the keys for make-use-case are actually allowed."
  (let ((sname (string name)))
    `(setf (gethash ,sname *use-cases*) (make-use-case :name ,sname
                                                       ,@(loop :for (k v) :on  parameters :by (function cddr)
                                                               :collect k
                                                               :collect `(quote ,v))))))

;;
;; Functions.
;;

(defparameter *width* 80)

(defun write-paragraph (&key (title "empty") (text nil) (width *width*) ((stream *standard-output*) *standard-output*) )
  (check-type *standard-output* stream)
  (check-type title string)
  (check-type width integer)
  (assert (>= width 0))
  (check-type text (or null string))
  (when text
    (format t "~a~%" title)
    (format t "~a~%~%" (make-string width :initial-element #\-))
    (format t "~a~%~%" text)))


(defun text (item)
  item)

(defun and-list (items)
  (when items
    (format nil "~{~a~#[. ~;;~%~% ~:;;~%~% ~]~^, AND ~}" items)))

(defun ordinal-list (items)
  (when items
    (format nil "~{#. ~a~#[. ~;;~%~% ~:;;~%~% ~]~}" items)))

(defun create-use-case-file (&key file-pathname use-case-name (*width* *width*))
  (check-type file-pathname pathname)
  (check-type use-case-name string)
  (check-type *width* integer)
  (assert (> *width* 0))
  (with-open-file (use-case-stream file-pathname :direction :output :if-exists :supersede)
    (let ((uc (gethash use-case-name *use-cases*))
          (*standard-output* use-case-stream))
      (unless uc
        (error "Undefined use case named ~S" use-case-name))
      (format t ".. comment: -*- mode:rst; coding:utf-8; electric-indent-mode:nil; tab-always-indent:t -*-~%~%")
      (format t "Use Case: ~a~%" (uc-name uc))
      (format t "~a~%~%" (make-string *width* :initial-element #\=))
      (loop :for (title reader formatter)
              :in '(("Primary Actor"          uc-primary-actor          text)
                    ("Goal in Context"        uc-goal-in-context        text)
                    ("Scope"                  uc-scope                  text)
                    ("Level"                  uc-level                  text)
                    ("Brief Description"      uc-brief-description      text)
                    ("Precondition"           uc-pre-condition          and-list)
                    ("Postcondition"          uc-post-condition         and-list)
                    ("Basic Flow"             uc-basic-flow             ordinal-list)
                    ("Alternate Flow"         uc-alternate-flow         ordinal-list)
                    ("Exceptional Flow"       uc-exceptional-flow       ordinal-list))
            :do (write-paragraph :title title :text (funcall formatter (funcall reader uc)))))))


#|
(uml:define-use-case "Administrator Logs In"
  :primary-actor "`Administrator`_"
  :basic-flow ("`Administrator`_ enters password or uses a cryptographic authentication")
  :alternate-flow ("`Administrator`_ enters a wrong password/cryptographic authentication")
  :exceptional-flow ("N/A")
  :brief-description "The `Administrator`_ enters the password or uses a cryptographic authentication methods to log in the system."
  :pre-condition ("Access to the `User Interface`_" "`Administrator`_ is not logged in")
  :post-condition ("`Administrator`_ is logged in")
  :goal-in-context "`Administrator`_ is Logged In"
  :level "N/A."
  :scope "Administrative scope.")

(uml:create-use-case-file :file-pathname #p"u-test.txt"
                          :use-case-name "Administrator Logs In")
|#
