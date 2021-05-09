;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               librarian-application.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;    
;;;;    A Schmidt librarian MIDI application
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

(defpackage "COM.INFORMATIMAGO.SYNTHESIZER.SCHMIDT.LIBRARIAN.APPLICATION"
  (:use "COMMON-LISP"
        "MIDI"
        "TRIVIAL-MAIN-THREAD"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.FILE"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.INTERACTIVE.INTERACTIVE"
        "COM.INFORMATIMAGO.MACOSX.COREMIDI"
        "COM.INFORMATIMAGO.MACOSX.COREMIDI.MIDI"
        "COM.INFORMATIMAGO.MIDI.ABSTRACT-MIDI-APPLICATION"
        "COM.INFORMATIMAGO.MIDI.ABSTRACT-SYNTHESIZER"
        "COM.INFORMATIMAGO.SYNTHESIZER.SCHMIDT"
        "COM.INFORMATIMAGO.SYNTHESIZER.SCHMIDT-SYNTHESIZER")
  (:shadowing-import-from "COM.INFORMATIMAGO.MACOSX.COREMIDI"
                          "DEVICE-ID")
  (:shadowing-import-from "COM.INFORMATIMAGO.MIDI.ABSTRACT-SYNTHESIZER"
                          "PARAMETER-NAME"
                          "PARAMETER-VALUES"
                          "PROGRAM"
                          "PROGRAM-NAME")
  (:shadow "INITIALIZE")
  (:export "INITIALIZE" "RUN" "PRINT-MIDI-DEVICES" "MAIN"))
(in-package "COM.INFORMATIMAGO.SYNTHESIZER.SCHMIDT.LIBRARIAN.APPLICATION")


(defvar *rc-filename* ".schmidt-librarian.lisp")
(defvar *version* "1.1.0")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defparameter *allow-print-backtrace* t)

(defun print-backtrace (&optional (output *error-output*))
  (when *allow-print-backtrace*
   #+ccl (format output "~&~80,,,'-<~>~&~{~A~%~}~80,,,'-<~>~&"
                 (ccl::backtrace-as-list))))


(defvar *effects* '())
(defun call-effects (&rest arguments)
  (mapc (lambda (effect)
          (block effect
            (handler-bind
                ((error (lambda (err)
                          (terpri *error-output*)
                          (print-backtrace *error-output*)
                          (format *error-output* "~&EE: effect ~A error: ~A~%" effect err)
                          (setf *effects* (delete effect *effects*))
                          (return-from effect))))
              (apply effect arguments))))
        (copy-list *effects*))
  *effects*)


(defvar *midi-log*         *error-output*)
(defvar *midi-application* nil)
(defvar *midi-verbose*     nil)
(defvar *verbose*          t)



(defvar *output-list* '())

(defun list-forward (message)
  (push message *output-list*))

(defun change-program (channel bank-number program-number
                       &optional (forward (function list-forward)))
  (funcall forward
           (make-instance 'midi:control-change-message
                          :time 0
                          :channel channel
                          :controller 0
                          :value 0))
  (funcall forward
           (make-instance 'midi:control-change-message
                          :time 0
                          :channel channel
                          :controller 32
                          :value (logand #x7f bank-number)))
  (funcall forward
           (make-instance 'midi:program-change-message
                          :time 0
                          :channel channel
                          :program (logand #x7f program-number)))
  (when (eql forward (function list-forward))
    (prog1 (nreverse *output-list*)
      (setf *output-list* '()))))

(defun flush-output-list ()
  (when *output-list*
    (let ((output-list '()))
      (rotatef *output-list* output-list)
      (setf output-list (nreverse output-list))
      (when *midi-verbose*
        (format t "~&RO: output-list ~S~%" output-list))
      (send (midi-output-port *midi-application*)
            (synth-destination *midi-application*)
            (packet-list-from-messages output-list))
      (flush-output (synth-destination *midi-application*))))
  (when *midi-verbose*
    (force-output)))

(defun client-notify (message)
  (format *midi-log* "CN: ~A~%" message)
  (force-output *midi-log*))

(defvar *bad-source-connection-refcon* '())


(defun midi-port-read (packet-list source-connection-refcon)
  (handler-bind
      ((error (lambda (err)
                (terpri *error-output*)
                (print-backtrace)
                (format *error-output* "~&EE: ~A: ~A~%"
                        (cffi:pointer-address source-connection-refcon)
                        err)
                (force-output *error-output*)
                (return-from midi-port-read))))

    (let ((*standard-output* *midi-log*)
          ;; (source-connection-refcon (cffi:pointer-address source-connection-refcon))
          )

      ;; (format *error-output* "PR: source-connection-refcon = ~S ~A ~A~%"
      ;;         source-connection-refcon
      ;;         (dw-8000-refcon-p *midi-application* source-connection-refcon)
      ;;         (controller-refcon-p *midi-application* source-connection-refcon))
      ;; (finish-output *error-output*)

      (cond
        ((null *midi-application*) #| not initialized yet|#)

        ((synth-refcon-p *midi-application* source-connection-refcon)
        
         (let ((message-list (packet-list-to-messages packet-list)))
           (call-effects :start-packet-list message-list source-connection-refcon)
           (dolist (message message-list)
             (when *midi-verbose*
               (unless (typep message '(or midi:timing-clock-message midi:active-sensing-message))
                 (format t "~&RD: ~A: ~A~%" source-connection-refcon message)))
             (call-effects :message message source-connection-refcon))
           (force-output)
           (call-effects :end-packet-list message-list source-connection-refcon)))

        ;; ((controller-refcon-p *midi-application* source-connection-refcon)
        ;; 
        ;;  (let ((message-list (packet-list-to-messages packet-list)))
        ;;    (dolist (message message-list)
        ;;      (when (and (typep message 'channel-message)
        ;;                 (= (message-channel message) (controller-channel *midi-application*)))
        ;;        (typecase message
        ;;          (program-change-message
        ;;           (let ((program  (message-program message)))
        ;;             (format t "~&RC: ~S~%" message)
        ;;             (format t "~&RC: ~A: PC ~A~%" source-connection-refcon program)
        ;;             (unless (= (message-channel message)
        ;;                        (dw-8000-channel *midi-application*))
        ;;               (setf (message-channel message) (dw-8000-channel *midi-application*)))
        ;;             (push message output-list)))
        ;;          (control-change-message
        ;;           (let ((controller (message-controller message))
        ;;                 (value      (message-value      message)))
        ;;             (when *midi-verbose*
        ;;               (format t "~&RC: ~S~%" message)
        ;;               (format t "~&RC: ~A: CC ~A ~A~%" source-connection-refcon controller value))
        ;;             (if (configuringp *midi-application*)
        ;;                 (configure *midi-application* controller value)
        ;;                 (map-controller-to-sysex-request *midi-application* controller value))))
        ;;          (t
        ;;           (when *midi-verbose*
        ;;             (unless (typep message '(or midi:timing-clock-message midi:active-sensing-message))
        ;;               (format t "~&RC: ~A: ~A~%" source-connection-refcon message)))
        ;;           (unless (= (message-channel message)
        ;;                      (dw-8000-channel *midi-application*))
        ;;             (setf (message-channel message) (dw-8000-channel *midi-application*)))
        ;;           (push message output-list)))))))

        (t
         (unless (member source-connection-refcon *bad-source-connection-refcon*)
           (push source-connection-refcon *bad-source-connection-refcon*)
           (format *error-output* "~&RR: ~A: unexpected refcon.~%" source-connection-refcon))))
      (flush-output-list))))



(defun test/send (output-port destination &key (channel 0))
  (let ((ti (current-host-time))
        (1s 1000000000))
    (flet ((in (n)
             (+ ti (* n 1s))))
      (send output-port destination
            (packet-list-from-messages
             (list  (make-instance 'midi::note-on-message :time (in 1) :status #x90 :channel channel :key 80 :velocity 70)
                    (make-instance 'midi::note-on-message :time (in 1) :status #x90 :channel channel :key 64 :velocity 70)
                    (make-instance 'midi::note-on-message :time (in 2) :status #x90 :channel channel :key 68 :velocity 40)
                    (make-instance 'midi::note-on-message :time (in 3) :status #x90 :channel channel :key 87 :velocity 80)
                    (make-instance 'midi::note-on-message :time (in 3) :status #x90 :channel channel :key 80 :velocity 80)
                    (make-instance 'midi::all-notes-off-message :time (in 5) :status #xb0 :channel channel)))))))


(defun print-midi-devices ()
  (let ((*print-circle* nil))
    (flet ((endpoint-and-connected-device (endpoint)
             (list (name endpoint)
                   (mapcar (function name) (connected-devices endpoint)))))
      (dolist (device (append (devices)
                              (external-devices)))
        (let ((entities      (device-entities device)))
          (format t "~30A ~%"
                  (name device))
          (dolist (entity entities)
            (format t "          - ~A~@[ <- ~{~S~^, ~}~]~@[ -> ~{~S~^, ~}~]~%"
                    (name entity)
                    (mapcar (function endpoint-and-connected-device)
                            (entity-sources entity))
                    (mapcar (function endpoint-and-connected-device)
                            (entity-destinations entity))))
          (terpri))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defun find-endpoint-for-external-device (external-device &key (direction :source))
  "Finds a source or destination endpoint belonging to a device, that
is linked to some endpoint of this EXTERNAL-DEVICE."
  (check-type external-device device)
  (check-type direction (member :source :input :destination :output))
  (flet ((not-connected (device)
           (error "Device named ~S doesn't seem to be connected" (name device))))
    (let* ((inp (case direction
                  ((:source :input) t)
                  (otherwise        nil)))
           (entity-endpoints (if inp
                                 (function entity-sources)
                                 (function entity-destinations))))
      (with-functions (entity-endpoints)
        (first (mapcan (lambda (device)
                         (mapcan (lambda (entity)
                                   (let ((endpoints (find external-device (entity-endpoints entity)
                                                          :key (function connected-devices)
                                                          :test (function member))))
                                     (when endpoints (list endpoints))))
                                 (device-entities device)))
                       (delete-duplicates
                        (mapcan (lambda (x)
                                  (mapcan (function connected-devices)
                                          (append (entity-sources x)
                                                  (entity-destinations x))))
                                (device-entities external-device)))))))))

(defun find-endpoint-for-device (device &key (direction :source))
  "Finds a source or destination endpoint belonging to a device, that
is linked to some endpoint of this EXTERNAL-DEVICE."
  (check-type device device)
  (check-type direction (member :source :input :destination :output))
  (flet ((not-connected (device)
           (error "Device named ~S doesn't seem to be connected" (name device))))
    (let* ((inp (case direction
                  ((:source :input) t)
                  (otherwise        nil)))
           (entity-endpoints (if inp
                                 (function entity-sources)
                                 (function entity-destinations))))
      (with-functions (entity-endpoints)
        (first (entity-endpoints
                (find-if (lambda (entity)
                           (and (entity-sources      entity)
                                (entity-destinations entity)))
                         (device-entities device))))))))

(defun find-source-endpoint-for-device-named (name)
  (let ((device (find-external-device-named name)))
    (if device
        (find-endpoint-for-external-device device :direction :source)
        (let ((device (find-device-named name)))
          (when device
            (find-endpoint-for-device device :direction :source))))))

(defun find-destination-endpoint-for-device-named (name)
  (let ((device (find-external-device-named name)))
    (if device
        (find-endpoint-for-external-device device :direction :destination)
        (let ((device (find-device-named name)))
          (when device
            (find-endpoint-for-device device :direction :destination))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;

(defclass librarian-application (midi-application)
  ((synthesizer            :reader synthesizer       :initarg  :synthesizer)

   (synth-device-name      :reader synth-device-name :initarg  :synth-device-name)
   (synth-channel          :reader synth-channel     :initarg  :synth-channel)
   (synth-destination      :reader synth-destination)
   (synth-source           :reader synth-source)
   (synth-refcon           :reader synth-refcon      :initform (generate-refcon))
   
   (configuring-controller :reader configuringp      :initform nil
                           :accessor configure-controller)))


(defmethod midi-initialize ((application librarian-application))
  (call-next-method)
  (let ((synthesizer (synthesizer application)))
    (with-slots (input-port output-port
                 synth-destination synth-source
                 synth-device-name synth-refcon) application

      (setf synth-destination (find-destination-endpoint-for-device-named synth-device-name)
            synth-source      (find-source-endpoint-for-device-named      synth-device-name)
            (synthesizer-destination synthesizer)     synth-destination
            (synthesizer-source      synthesizer)     synth-source)

      (port-connect-source input-port synth-source synth-refcon)
      (when *verbose*
        (format *midi-log* "synthesizer source       = ~60A (~A)~%" synth-source      synth-device-name)
        (format *midi-log* "synthesizer destination  = ~60A (~A)~%" synth-destination synth-device-name))
      application)))

(defgeneric (setf synth-state) (new-state application controller)
  (:method (new-state (application librarian-application) controller)
    (check-type controller (integer 0 127))
    (send (midi-output-port application)
          (synth-destination application)
          (packet-list-from-messages
           (list (make-instance 'control-change-message :time (current-host-time)
                                                        :channel (synth-channel application)
                                                        :controller controller
                                                        :value (if new-state 127 0)))))))

(defgeneric synth-refcon-p (application refcon)
  (:method ((self librarian-application) refcon)
    (eql refcon (synth-refcon self))))

(defgeneric map-synth-to-sysex-request (application controller value)
  (:method ((self librarian-application) controller value)
    ;; (let ((map (cc-map self))
    ;;       (*midi-application* self))
    ;;   (dispatch map controller value))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;


(defun configure (midi-application controller value)
  (declare (ignore midi-application controller value))
  (warn "~S not implemented yet." 'configure))


;; Two threads:

;; ========================================================================
;; 1- a REPL thread, using the terminal.
;; ========================================================================

(define-symbol-macro quit          (repl-exit))
(define-symbol-macro cl-user::quit (repl-exit))

(defun simple-repl ()
  (#-ccl progn #+ccl objc:with-autorelease-pool
   #+(and) (progn
             (format t "~&Use quit to exit.~%")
             (repl))
   #-(and) (loop
             :for command := (string-trim " "
                                          (progn (format t "> ")
                                                 (finish-output)
                                                 (read-line)))
             :do (cond
                   ((string-equal command "quit"))
                   ((string-equal command "help")
                    (format t "~&Help: ~:{~%  ~8A ~A~}~%"
                            '(("help" "Displays this help.")
                              ("quit" "Stops this midi application.")
                              ("" "otherwise, evaluate lisp expressions."))))
                   (t
                    (rep :line command)))
             :until (string-equal command "quit"))))

(defvar *repl-done* nil)
(defun run-repl-thread ()
  (bt:make-thread (lambda ()
                    (setf *repl-done* nil)
                    (unwind-protect
                         (simple-repl)
                      (setf *repl-done* t)))
                  :name "repl"
                  :initial-bindings `((*standard-input*  . ,*standard-input*)
                                      (*standard-output* . ,*standard-output*)
                                      (*terminal-io*     . ,*terminal-io*))))


;; ========================================================================
;; 2- the main thread, running an event loop to process midi events.
;; ========================================================================

(defun run-loop-process-midi-events ()
  #+ccl (objc:with-autorelease-pool
          (#_CFRunLoopRunInMode #$kCFRunLoopDefaultMode 0.1d0 1)
          #-(and) (list #$kCFRunLoopRunFinished
                        #$kCFRunLoopRunStopped
                        #$kCFRunLoopRunTimedOut
                        #$kCFRunLoopRunHandledSource))
  #-ccl (error "Not implemented yet for ~A" (lisp-implementation-type)))

(defun run-main-event-loop ()
  (loop
    :do #+swank (with-body-in-main-thread (:blocking t)
                  (run-loop-process-midi-events))
        #-swank (run-loop-process-midi-events)
    :until *repl-done*))


;; ========================================================================
;; The main function.
;; ========================================================================

(defmethod print-configuration ((application librarian-application))
  (format t "~2%")
  (format t "Application ~A:~%" (class-name (class-of application)))
  (format t "~A ~A channel ~A:~%    src: ~A~%    dst: ~A~%    refcon ~A~%"
          "Schmidt Synthesizer"
          (synth-device-name application)
          (synth-channel application)
          (name (synth-source application))
          (name (synth-destination application))
          (synth-refcon application))
  (format t "~2%")
  (values))


(defun run (&key
              (synth-device-name "MIDI SCHMIDT SYNTH")
              (synth-channel 8))
  (initialize)
  (let* ((synthesizer (make-instance 'schmidt-synthesizer
                                     :name synth-device-name
                                     :channel synth-channel))
         (application (create-midi-application
                       'librarian-application
                       "Schmidt MIDI Librarian"
                       'client-notify 'midi-port-read
                       :synth-device-name synth-device-name
                       :synth-channel synth-channel
                       :synthesizer synthesizer)))
    (push (lambda (selector message source-connection-refcon)
            (declare (ignorable source-connection-refcon))
            (block effect
              (when (and (eq :message selector)
                         (typep message 'midi:system-exclusive-message))
                (handler-bind
                    ((error (lambda (err)
                              (finish-output)
                              (terpri *error-output*)
                              (print-backtrace)
                              (format *error-output* "~&EE: ~A~%" err)
                              (finish-output *error-output*)
                              (return-from effect))))
                  (receive-sysex-message synthesizer message)))))
          *effects*)
    (unwind-protect
         (progn
           (setf *midi-application* application)
           (run-repl-thread)
           (run-main-event-loop))
      (terminate *midi-application*)
      (setf *midi-application* nil))))

(defun load-rc-file ()
  (with-open-file (rc (merge-pathnames *rc-filename* (user-homedir-pathname))
                      :if-does-not-exist nil)
    (when rc
      (handler-case (load rc)
        (error (err)
          (finish-output)
          (terpri *error-output*)
          (print-backtrace)
          (format *error-output* "~%ERROR: ~A~%" err)
          (finish-output *error-output*))))))

(defvar *initialized* nil)
(defun initialize ()
  #-(and) (trace midi-initialize
                 midi-port-read
                 client-create
                 output-port-create
                 input-port-create
                 port-connect-source
                 find-destination-endpoint-for-device-named
                 find-source-endpoint-for-device-named)
  (unless *initialized*
    (#-ccl progn #+ccl objc:with-autorelease-pool
     (com.informatimago.common-lisp.interactive.interactive:initialize)
     (coreaudio-framework)
     (coremidi-framework)
     (coremidi:restart)
     (load-rc-file))
    (setf *initialized* t)))


;;----------------------------------------------------------------------
;; main
;;----------------------------------------------------------------------

(defun check-bounds (val min max title)
  (assert (<= min val max)
          (val) "~A should be between ~A and ~A"
          title min max)
  val)

(defun parse-arguments (arguments)
  (loop
    :with result := '()
    :while arguments
    :for arg := (pop arguments)
    :do (cond
          ((member arg '("-h" "--help") :test (function string=))
           (setf result (list* :help t result)))
          ((member arg '("-V" "--version") :test (function string=))
           (setf result (list* :version t result)))
          ((member arg '("-v" "--verbose") :test (function string=))
           (setf result (list* :verbose t result)))
          ((member arg '("-l" "--list-devices") :test (function string=))
           (setf result (list* :list-devices t result)))
          ((member arg '("-sd" "--synth-device-name") :test (function string=))
           (let ((value (if arguments
                            (pop arguments)
                            (error "Missing the device name of the Schmidt Synthesizer after ~S" arg))))
             (setf result (list* :synth-device-name value result))))
          ((member arg '("-sc" "--synth-channel") :test (function string=))
           (let ((value (if arguments
                            (1- (check-bounds (parse-integer (pop arguments)) 1 16 "MIDI Channel"))
                            (error "Missing then channel of the Schmidt Synthesizer after ~S" arg))))
             (setf result (list* :synth-channel value result))))
          ((member arg '("--print-backtrace-on-error") :test (function string=))
           (setf result (list* :print-backtrace-on-error t result))))
    :finally (return result)))

(defun print-help (pname)
  (format t "~2%~A usage:" pname)
  (format t "~2%    ~A [-h|--help] [-V|--version] [-l|--list-devices]" pname)
  (format t " \\~%    ~VA [-sd|--synth-device-name  name]"             (length pname) "")
  (format t " \\~%    ~VA [-sc|--synth-channel  midi-channel]"         (length pname) "")
  (format t " \\~%    ~VA [-v|--verbose] [--print-backtrace-on-error]" (length pname) "")
  (format t "~2%  names can be found with --list-devices,")
  (format t "~%  midi-channel go from 1 to 16.")
  (format t "~%  Defaults are: -sd \"MIDI SCHMIDT SYNTH\" -sc 8")
  (format t "~2%")
  (finish-output))

(defun print-version (pname)
  (format t "~A version: ~A~%" pname *version*))

(defun main (program-path arguments)
  (let ((print-backtrace-on-error nil))
    (handler-bind
        ((error (lambda (condition)
                  (finish-output *standard-output*)
                  (when print-backtrace-on-error
                    (terpri *error-output*)
                    (print-backtrace *error-output*))
                  (format *error-output* "~%ERROR: ~A~%" condition)
                  (finish-output *error-output*)
                  (ccl:quit 1))))
      (let ((options (parse-arguments arguments))
            (pname   (file-namestring program-path)))
        (setf print-backtrace-on-error (getf options :print-backtrace-on-error))
        (setf *midi-verbose*           (getf options :verbose))
        (setf *verbose*                (getf options :verbose))
        (cond
          ((getf options :help)
           (print-help pname))
          ((getf options :version)
           (print-version pname))
          ((getf options :list-devices)
           (initialize)
           (print-midi-devices))
          (t
           (initialize)
           (run :synth-device-name (getf options :synth-device-name "MIDI SCHMIDT SYNTH")
                :synth-channel     (getf options :synth-channel     8)))))))
  0)

