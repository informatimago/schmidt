;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               librarian.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    A librarian to manage Single Bank SysEx files of the Schmidt Synthesizer.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2018-01-14 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2018 - 2018
;;;;
;;;;    This program is free software: you can redistribute it and/or modify
;;;;    it under the terms of the GNU Affero General Public License as published by
;;;;    the Free Software Foundation, either version 3 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    This program is distributed in the hope that it will be useful,
;;;;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;;    GNU Affero General Public License for more details.
;;;;
;;;;    You should have received a copy of the GNU Affero General Public License
;;;;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;;**************************************************************************

(in-package "COM.INFORMATIMAGO.SYNTHESIZER.SCHMIDT.LIBRARIAN")

(defconstant +schmidt-single-bank-sysex-file-length+ 65548)
(defparameter *sysex-directory* #P"~/Documents/SysEx Librarian/*.syx")



(defun menu-select (items &key (cancel t))
  "
An item in the menu ITEMS list can be either an atom,
or a cons containing an atom in the car, and further data.
The atoms must be unique amongst the ITEMS.
Only the atoms are displayed.
The whole selected item is returned.
"
  (with-standard-io-syntax
    (loop
      :do (loop :for index :from 1
                :for item :in items
                :do (format *query-io* "~3D) ~A~%" index (if (consp item) (car item) item))
                :finally (when cancel (format *query-io* "~3D) ~A~%" 0 'cancel)))
          (let ((selection (let ((*read-eval* nil))
                             (read *query-io*))))
            (cond
              ((not (integerp selection))
               (format *query-io* "Invalid entry.~%"))
              ((and cancel (zerop selection))
               (return nil))
              ((<= 1 selection (length items))
               (return (values (elt items (1- selection)) (1- selection))))
              (t
               (format *query-io* "Invalid entry.~%")))))))





(defparameter *banks*             '() "A list of (bank bank-namestring bank-number).")
(defparameter *programs*          '() "A list of (program bank-namestring bank-number program-number)")
(defparameter *program-selection* '() "A list of (program bank-namestring bank-number program-number)")

(defun sort-bank-entries (banks)
  (sort banks
        (lambda (a b)
          (destructuring-bind (a.bank a.bank-namestring a.bank-number) a
            (declare (ignore a.bank))
            (destructuring-bind (b.bank b.bank-namestring b.bank-number) b
              (declare (ignore b.bank))
              (or (string< a.bank-namestring b.bank-namestring)
                  (and (string= a.bank-namestring b.bank-namestring)
                       (< a.bank-number b.bank-number))))))))

(defun sort-program-entries (programs)
  (sort programs
        (lambda (a b)
          (destructuring-bind (a.program a.bank-namestring a.bank-number a.program-number) a
            (declare (ignore a.program))
            (destructuring-bind (b.program b.bank-namestring b.bank-number b.program-number) b
              (declare (ignore b.program))
              (or (< a.bank-number b.bank-number)
                  (and (= a.bank-number b.bank-number)
                       (or (< a.program-number b.program-number)
                           (and (= a.program-number b.program-number)
                                (string< a.bank-namestring b.bank-namestring))))))))))

(defun display-program-list (programs)
  (terpri)
  (format t "------------------------------------------------------------------------~%")
  (loop :for (program bank-namestring bank-number program-number)
          :in programs
        :do (format t "~3D:~3D ~18A  ~S~%" (1+ bank-number) (1+ program-number) (program-name program) bank-namestring))
  (format t "------------------------------------------------------------------------~%")
  (values))

(defun programs-from-bank (bank bank-namestring bank-number)
  (loop
    :for program-number :below (bank-program-count bank)
    :collect (list (program bank program-number) bank-namestring bank-number program-number)))

(defun list-programs-in-bank ()
  (let ((selection (menu-select (mapcar (lambda (entry)
                                          (destructuring-bind (bank bank-namestring bank-number) entry
                                            (declare (ignore bank))
                                            (cons (format nil "~3D  ~S" (1+ bank-number) bank-namestring)
                                                  entry)))
                                        (sort-bank-entries (copy-list *banks*))))))
    (when selection
      (destructuring-bind (bank bank-namestring bank-number) (rest selection)
        (display-program-list (programs-from-bank bank bank-namestring bank-number)))))
  (values))

(defun load-programs-from-bank (bank bank-namestring bank-number)
  (setf *programs* (programs-from-bank bank bank-namestring bank-number)))



(defun sysex-file-menu ()
  (menu-select (remove +schmidt-single-bank-sysex-file-length+
                       (directory *sysex-directory*)
                       :test (function /=)
                       :key (lambda (path)
                              (or (ignore-errors
                                   (with-open-file (stream path :direction :input
                                                                :element-type '(unsigned-byte 8))
                                     (file-length stream)))
                                  0)))))

(defun load-a-bank ()
  (let ((sysex-path (sysex-file-menu)))
    (when sysex-path
      (let ((sysex (binary-file-contents sysex-path)))
        (if (and (= (length sysex)
                    +schmidt-single-bank-sysex-file-length+)
                 (= #XF0 (aref sysex 0))
                 (= #x7D (aref sysex 1))
                 (= #x77 (aref sysex 2))
                 (= #x33 (aref sysex 3))
                 (= #xF7 (aref sysex (1- (length sysex)))))
            (let ((bank            (bank-from-sysex sysex))
                  (bank-number     (aref sysex 4))
                  (bank-namestring (namestring sysex-path)))
              (push (list bank bank-namestring bank-number) *banks*)
              (load-programs-from-bank bank bank-namestring bank-number))
            (error "Invalid Sysex file.")))))
  (values))


(defun select-programs-by-name ()
  (let ((pattern (progn (write-string "Pattern: " *query-io*)
                        (finish-output *query-io*)
                        (read-line *query-io*))))
    (display-program-list
     (setf *program-selection*
           (sort-program-entries
            (remove pattern *programs*
                    :test-not (lambda (pattern string)
                                (search pattern string :test (function char-equal)))
                    :key (lambda (entry)
                           (program-name (first entry)))))))))

(defun program-menu (programs)
  (mapcar (lambda (entry)
            (destructuring-bind (program bank-namestring bank-number program-number) entry
              (cons (format nil "~3D:~3D ~18A  ~S" (1+ bank-number) (1+ program-number) (program-name program) bank-namestring)
                    entry)))
          programs))

(defun select-a-program ()
  (if (or *program-selection* *programs*)
      (let ((selection (menu-select (program-menu (or *program-selection* *programs*)))))
        (when selection
          (display-program-list
           (setf *program-selection* (list (cdr selection))))))
      (error "No program loaded; try load-a-bank first.")))

(defun display-program-selection ()
  (display-program-list *program-selection*))

;; single program
;; single multi
;; single bank = 128 single programs
;; multi bank  = 128 multi programs
;; set  = 8 single banks + 4 multi banks

(defun quit () (throw 'quit (values)))

(defun run ()
  (format t "Hello!~%")
  (catch 'quit
    (loop
      (funcall (menu-select '(create-a-new-bank
                              load-a-bank
                              save-bank
                              save-bank-as
                              change-bank-number
                              list-programs-in-bank
                              display-program-selection
                              select-programs-by-name
                              select-a-program
                              select-a-program-range

                              quit)
                            :cancel nil)))))
