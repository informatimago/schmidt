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
           "RECEIVE-SYSEX-MESSAGE"
           "SCHMIDT-SYNTHESIZER"
           "SYSEX-BUFFER"))
(in-package "COM.INFORMATIMAGO.SYNTHESIZER.SCHMIDT-SYNTHESIZER")

(deftype midi-data        () '(integer 0 127))
(deftype channel          () '(integer 0 15))
(deftype bank-number      () '(integer 0 7))
(deftype program-number   () '(integer 0 127))
;; (deftype parameter-offset () '(integer 0 63))
;; (deftype parameter-value  () '(integer 0 63))


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




(defun device-id (channel device-id)
  (print `(device-id ,channel ,device-id)))
(defun received-data-dump (channel device-id parameters)
  (print `(data-dump ,channel ,device-id ,parameters)))
(defun write-completed-status (channel device-id)
  (print `(write-completed-status ,channel ,device-id)))
(defun write-error-status     (channel device-id)
  (print `(write-error-status ,channel ,device-id)))

(defun parse-system-exclusive-message (bytes)
  (let ((s 0)
        channel device-id)
    (flet ((eat (code)
             (if (= code (aref bytes s))
                 (incf s)
                 (progn
                   (cerror "Skip until expected ~2*~2,'0X byte is found."
                           "Unexpected byte in sysex at position ~D, got ~2,'0X, expected ~2,'0X."
                           s (aref bytes s) code)
                   (loop :while (and (< s (length bytes))
                                     (/= code (aref bytes s)))
                         :do (incf s))))))
      (warn "~S not implemented yet" parse-system-exclusive-message)
      ;; (eat +sysex+)
      ;; (unless (= +korg-id+ (aref bytes s))
      ;;   (return-from parse-system-exclusive-message nil))
      ;; (eat +korg-id+)
      ;; (setf channel (ldb (byte 4 0) (aref bytes s)))
      ;; (let ((format (ldb (byte 4 4) (aref bytes s))))
      ;;   (case format
      ;;     ((#.+device-id+)
      ;;      (incf s)
      ;;      (setf device-id (aref bytes s))
      ;;      (unless (= +korg-dw-8000+ device-id)
      ;;        (return-from parse-system-exclusive-message nil))
      ;;      (incf s)
      ;;      (if (= +eox+ (aref bytes s))
      ;;          (progn
      ;;            (eat +eox+)
      ;;            (device-id channel device-id))
      ;;          (case (aref bytes s)
      ;;            ((#.+program-parameter-dump+)
      ;;             (incf s)
      ;;             (let ((parameters (loop
      ;;                                 :with parameters := '()
      ;;                                 :while (and (< s (- (length bytes) 2))
      ;;                                             (< (aref bytes s) 128)
      ;;                                             (< (aref bytes (1+ s)) 128))
      ;;                                 :for p := (aref bytes s)
      ;;                                 :for v := (aref bytes (1+ s))
      ;;                                 :for parameter := (find-parameter p)
      ;;                                 ;; :do (if parameter
      ;;                                 ;;         (print (list p '/ (parameter-name parameter) (parameter-min parameter) '<= v '<= (parameter-max parameter)))
      ;;                                 ;;         (print `(unknown parameter ,p value ,v)))
      ;;                                 :do (if parameter
      ;;                                         (if (<= (parameter-min parameter) v (parameter-max parameter))
      ;;                                             (push (list (parameter-offset parameter) v) parameters)
      ;;                                             (progn
      ;;                                               (cerror "Set parameter ~@1*~A to minimum value ~D"
      ;;                                                       "Value ~D of parameter ~A is out of expected range [~D,~D] in data dump."
      ;;                                                       v
      ;;                                                       (parameter-name parameter)
      ;;                                                       (parameter-min parameter)
      ;;                                                       (parameter-max parameter))
      ;;                                               (push (list (parameter-offset parameter) (parameter-min parameter)) parameters)))
      ;;                                         (cerror "Ignore unknown parameter ~D"
      ;;                                                 "Unknown parameter offset ~D in data dump." p))
      ;;                                     (incf s 2)
      ;;                                 :finally (return parameters))))
      ;;               (eat +eox+)
      ;;               (received-data-dump channel device-id parameters)))
      ;;            ((#.+write-completed-status+)
      ;;             (write-completed-status channel device-id))
      ;;            ((#.+write-error-status+)
      ;;             (write-error-status     channel device-id))
      ;;            (otherwise
      ;;             (error "Unexpected sysex from DW-8000/EX-8000.")))))
      ;; 
      ;;     (otherwise
      ;;      (error "Unexpected format code in sysex at position ~D, got ~1,'0X."
      ;;             s format))))
      )))


(defmacro sysex (&body expressions)
  (let ((i -1)
        (vvar (gensym)))
    `(let ((,vvar (make-array ,(+ 2 (length expressions)) :element-type '(unsigned-byte 8))))
       (setf (aref ,vvar ,(incf i)) +sysex+)
       (setf ,@(mapcan (lambda (e) `((aref ,vvar ,(incf i)) ,e))
                       expressions))
       (setf (aref ,vvar ,(incf i)) +eox+)
       ,vvar)))


(defmethod receive-sysex-message ((synthesizer schmidt-synthesizer) message)
  ;; (cancel-timeout synthesizer)
  (let ((parsed (handler-bind ((error (lambda (err)
                                        (let ((restart (find-restart 'continue err)))
                                          (when restart
                                            (format *error-output* "~&RS: ~A -- continued.~%" err)
                                            (invoke-restart restart))))))
                  (parse-system-exclusive-message (message-data message)))))
    (print parsed)
    ;; let's ignore the channel.
    (case (first parsed)
      ((device-id)
       (let ((device-id (third parsed)))
         (unless (eql device-id +KORG-DW-8000+)
           (bad-device-id synthesizer device-id)))
       (case (synthesizer-state synthesizer)
         ((nil :expecting-device-id) (enter-idle-state synthesizer))))
      ((data-dump)
       ;;- fill the current-program with the received parameters.
       (assert (eql 3 (third parsed)))
       (setf (program-values (synthesizer-current-program synthesizer)) (fourth parsed))
       (case (synthesizer-state synthesizer)
         ((nil :expecting-data-dump) (enter-idle-state synthesizer))))
      ((write-error-status)
       (write-error synthesizer)
       (case (synthesizer-state synthesizer)
         ((nil :expecting-write-status) (enter-idle-state synthesizer))))
      ((write-completed-status)
       (case (synthesizer-state synthesizer)
         ((nil :expecting-write-status) (enter-idle-state synthesizer))))
      (otherwise
       (case (synthesizer-state synthesizer)
         ((nil)  (enter-idle-state synthesizer)))))))

;;;; THE END ;;;;
