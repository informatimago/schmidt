;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               schmidt-synthesizer.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    Concrete class for the schmidt-synthezier.
;;;;    
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2021-04-29 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;    
;;;;    Copyright Pascal J. Bourguignon 2021 - 2021
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

(defpackage "COM.INFORMATIMAGO.SYNTHESIZER.SCHMIDT-SYNTHESIZER"
  (:use "COMMON-LISP"
        "MIDI"
        "COM.INFORMATIMAGO.MIDI.ABSTRACT-SYNTHESIZER")
  (:export "MIDI-DATA"
           "CHANNEL"
           "+SYSEX+"
           "+EOX+"
           "+DEVICE-ID+"
           "DEVICE-ID-REQUEST"
           "SYSEX"
           "WRITE-DATA-BYTE-TO-SYSEX"
           "WRITE-STRING-TO-SYSEX"
           "WRITE-INTEGER-TO-SYSEX"
           "WRITE-PCM-SAMPLE-TO-SYSEX"
           "SYSEX-BUFFER"))
(in-package "COM.INFORMATIMAGO.SYNTHESIZER.SCHMIDT-SYNTHESIZER")

(deftype midi-data        () '(integer 0 127))
(deftype channel          () '(integer 0 15))


(defconstant +sysex+                             #xf0)
(defconstant +eox+                               #xf7)


;; (defmacro sysex (&body expressions)
;;   (let ((i -1)
;;         (vvar (gensym)))
;;     `(let ((,vvar (make-array ,(+ 2 (length expressions)) :element-type '(unsigned-byte 8))))
;;        (labels ((write-data-byte-to-sysex (data-byte)
;;                   (check-type data-byte midi-data)
;;                   (setf (aref ,vvar ,(incf i)) data-byte))
;;                 (write-string-to-sysex (string length)
;;                   (loop :for ch :across (subseq (format nil "~VA" length string) 0 length)
;;                         :for code := (korg-char-code ch)
;;                         :do (write-data-byte-to-sysex code)))
;;                 (write-integer-to-sysex (value length)
;;                   (loop
;;                     :repeat length
;;                     :for p :from 0 :by 7
;;                     :do (write-data-byte-to-sysex (ldb (byte 7 p) value))))
;;                 (write-pcm-sample-to-sysex (value)
;;                   (write-data-byte-to-sysex (ash (ldb (byte 5 0) value) 2))
;;                   (write-data-byte-to-sysex (ldb (byte 7 5) value)))
;;                 (sysex-buffer () ,vvar))
;;          (declare (inline write-data-byte-to-sysex
;;                           write-string-to-sysex
;;                           write-integer-to-sysex
;;                           write-pcm-sample-to-sysex))
;;          (setf (aref ,vvar ,(incf i)) +sysex+)
;;          ,@(loop
;;              :while expressions
;;              :do (let ((e (pop expressions)))
;;                    (if (eq e :eval)
;;                        (progn
;;                          (unless expressions
;;                            (error "Missing lisp expressions after :eval in SYSEX form."))
;;                          (pop expressions))
;;                        `(setf (aref ,vvar ,(incf i)) ,e))))
;;          (setf (aref ,vvar ,(incf i)) +eox+)
;;          ,vvar))))

;; (defun device-id-request (channel)
;;   (check-type channel channel)
;;   (sysex
;;     +korg-id+
;;     (logior +device-id-request+ channel)))


(defclass schmidt-synthesizer (synthesizer)
  ())

(defmethod get-current-program ((synthesizer schmidt-synthesizer))
  ;; from midi!
  )

(defmethod program-parameters ((program program))
  )

(defmethod update-parameter ((parameter parameter) value)
  )


;;;; THE END ;;;;
