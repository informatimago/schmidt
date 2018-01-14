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



(defun menu-select (items)
  (with-standard-io-syntax
    (loop
      :do (loop :for index :from 1
                :for item :in items
                :do (format *query-io* "~3D) ~A~%" index item)
                :finally (format *query-io* "~3D) ~A~%" 0 'cancel))
          (let ((selection (let ((*read-eval* nil))
                             (read *query-io*))))
            (cond
              ((not (integerp selection))
               (format *query-io* "Invalid entry.~%"))
              ((zerop selection)
               (return nil))
              ((<= 1 selection (length items))
               (return (values (elt items (1- selection)) (1- selection))))
              (t
                (format *query-io* "Invalid entry.~%")))))))



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
            (bank-from-sysex sysex)
            (error "Invalid Sysex file."))))))


;; single program
;; single multi
;; single bank = 128 single programs
;; multi bank  = 128 multi programs
;; set  = 8 single banks + 4 multi banks


(defun run ()
  (format t "Hello!~%")
  '(create-a-new-bank
    load-a-bank
    save-bank
    save-bank-as
    change-bank-number
    list-programs
    select-a-program
    select-a-program-range
    
    
    )
  (values))
