(defpackage "COM.INFORMATIMAGO.SYNTHESIZER.SCHMIDT.EXPLORATION"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.SYNTHESIZER.SCHMIDT"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.FILE"))
(in-package "COM.INFORMATIMAGO.SYNTHESIZER.SCHMIDT.EXPLORATION")

#|

(loop for x across (histogram-bins (histogram (com.informatimago.common-lisp.cesarum.file:binary-file-contents
                                               (first (directory "*.syx"))) 257 :min-value 0 :max-value 255))
      for i from -1
      unless (zerop x)
        do (format t "~3D: ~8D ~@[~C~]~%" i x (when (> i 32) (code-char i))))






(mapcar 'com.informatimago.common-lisp.cesarum.file:binary-file-contents (directory #P"/Users/pjb/Documents/SysEx Librarian/Schmidt-Single-bank-*.syx"))

(defparameter *syx* #P"/Users/pjb/Documents/SysEx Librarian/Schmidt-Single-bank-1.syx")
(defparameter *sysex* (com.informatimago.common-lisp.cesarum.file:binary-file-contents *syx*))

(map 'string 'code-char
  (loop
    :with sysex := *sysex*
    :for i :from 6 :below (1- (length sysex)) :by 2
    :collect
    (dpb (aref sysex (1+ i))
         (byte 4 4)
         (aref sysex i))))


(mapcar (lambda (*sysex*)
          (list (position-if (lambda (x) (> x 15)) *sysex* :start 6)
                (aref *sysex* (position-if (lambda (x) (> x 15)) *sysex* :start 6))))
 (mapcar 'com.informatimago.common-lisp.cesarum.file:binary-file-contents
         (directory #P"/Users/pjb/Documents/SysEx Librarian/Schmidt-Single-bank-*.syx")))
((65547 247) (65547 247) (65547 247) (65547 247) (65547 247) (65547 247) (65547 247) (65547 247))
(65547 65547 65547 65547 65547 65547 65547 65547)

|#


(defparameter *syx* #P"/Users/pjb/Documents/SysEx Librarian/Schmidt-Single-bank-1.syx")
(defparameter *sysex* (com.informatimago.common-lisp.cesarum.file:binary-file-contents *syx*))

(defparameter *syx* #P"~/Documents/SysEx Librarian/Schmidt-Single-PJB-8-20171228.syx")
(defparameter *sysex* (com.informatimago.common-lisp.cesarum.file:binary-file-contents *syx*))

(defparameter *syx* #P"~/Documents/SysEx Librarian/Schmidt-Single-8-9-color.syx")
(defparameter *sysex* (com.informatimago.common-lisp.cesarum.file:binary-file-contents *syx*))
(defparameter *bank*  (bank-from-sysex *sysex*))


(defun swap-quad (q)
  (rotatef (ldb (byte 1 0) q) (ldb (byte 1 3) q))
  (rotatef (ldb (byte 1 1) q) (ldb (byte 1 2) q))
  q)

(defun little-quad (a b)
  (check-type a (integer 0 15))
  (check-type b (integer 0 15))
  (dpb b (byte 4 4) a))

(defun big-quad (a b)
  (check-type a (integer 0 15))
  (check-type b (integer 0 15))
  (dpb a (byte 4 4) b))



(defun print-bank (b)
  (let ((*print-base* 16.))
   (loop :for i :from 0 :by 256 :below (length b)
         :do (print (subseq b i (min (length b) (+ i 256)))))))


(defun sysex-data (sysex)
  (coerce (loop
            :for i :from COM.INFORMATIMAGO.SYNTHESIZER.SCHMIDT::*data-offset* :below (- (length sysex) 5) :by 2
            :collect (little-quad (aref sysex i) (aref sysex (+ i 1))))
          '(vector (unsigned-byte 8))))

(defun dump-block (seq)
  (loop
    :for i :from 0
    :for byte :across seq
    :do (when (zerop (mod i 32))
          (terpri))
        (format t "~2,'0X " byte)
    :finally (terpri)))

(defun dump-sysex (sysex)
  (dump-block (subseq sysex 0 COM.INFORMATIMAGO.SYNTHESIZER.SCHMIDT::*data-offset*))
  (let ((end (position #xf7 sysex)))
    (dump-block (subseq sysex COM.INFORMATIMAGO.SYNTHESIZER.SCHMIDT::*data-offset* end))
    (dump-block (subseq sysex end))))


(defmacro with-output-to-file (path-and-option &body body)
  (let ((path   (if (atom path-and-option)
                    path-and-option
                    (first path-and-option)))
        (append (if (atom path-and-option)
                    nil
                    (member :append (rest path-and-option)))))
    `(with-open-file (*standard-output* ,path
                                        :direction :output
                                        :if-does-not-exist :create
                                        :if-exists ,(if append
                                                        :append
                                                        :supersede))
       ,@body)))


(defun dump-syxes ()
  (loop
    :with destination-directory := (append (pathname-directory (user-homedir-pathname))
                                           '("works" "synth" "schmidt" "tests"))
    :for path  :in '(
                     ;; #P"~/Documents/SysEx Librarian/Schmidt-Single-PJB-8-20171228.syx"
                     ;; #P"~/Documents/SysEx Librarian/Schmidt-Single-PJB-8-20180101T014000.syx"
                     ;; #P"~/Documents/SysEx Librarian/Schmidt-Single-PJB-8-20180101T025500.syx"
                     #P"~/Documents/SysEx Librarian/Schmidt-Single-8-9-color.syx"
                     #P"~/Documents/SysEx Librarian/Schmidt-Single-8-9-VCF1=0.syx"
                     #P"~/Documents/SysEx Librarian/Schmidt-Single-8-9-VCF1=255.syx"
                     #P"~/Documents/SysEx Librarian/Schmidt-Single-8-9-VCF1=511.syx"
                     #P"~/Documents/SysEx Librarian/Schmidt-Single-8-1-Vol=165.syx"
                     #P"~/Documents/SysEx Librarian/Schmidt-Single-8-1-Vol=222.syx"
                     ;; #P"~/works/synth/schmidt/V1.22_panel.syx"
                     ;; #P"~/works/synth/schmidt/V1.22_system.syx"
                     ;; #P"~/works/synth/schmidt/V1.22_voice.syx"
                     )
    :for dump  := (make-pathname :type "dump"
                                 :directory destination-directory
                                 :defaults path)
    :for bytes := (make-pathname :type "bytes"
                                 :directory destination-directory
                                 :defaults path)
    :for sysex := (com.informatimago.common-lisp.cesarum.file:binary-file-contents path)
    :do (with-open-file (*standard-output* dump
                                           :direction :output
                                           :if-does-not-exist :create
                                           :if-exists :supersede)
          (dump-sysex sysex))
        (with-open-file (*standard-output* bytes
                                           :direction :output
                                           :if-does-not-exist :create
                                           :if-exists :supersede)
          (dump-block (sysex-data sysex)))))




#|

(dump-syxes)

(print-bank (sysex-data *sysex*))

(progn
  (with-output-to-file "a"  (dump-block (program *bank* 8)) (terpri))
  (with-output-to-file "b"  (dump-block (program *bank* 9)) (terpri))
  (with-output-to-file "c"  (dump-block (program *bank* 10)) (terpri)))

(with-open-file (*standard-output* "~/works/synth/schmidt/schmidt.out"
                                   :direction :output
                                   :if-does-not-exist :create
                                   :if-exists :supersede)
  (loop :for encoding :in '(code-char offset-ascii-code-char schmidt-code-char)
        :do (loop :for quadsex :in '(little-quad big-quad)
                  :do (loop
                        :for permutation :in (com.informatimago.clext.association::permutations '(0 1 2 3))
                        :for quadswap := (coerce `(lambda (q)
                                                    (dpb (ldb (byte 1 ,(first permutation)) q)
                                                         (byte 1 0)
                                                         (dpb (ldb (byte 1 ,(second permutation)) q)
                                                              (byte 1 1)
                                                              (dpb (ldb (byte 1 ,(third permutation)) q)
                                                                   (byte 1 2)
                                                                   (ldb (byte 1 ,(fourth permutation)) q)))))
                                                 'function)
                        :do (loop
                              :for start :in '(6 7)
                              :for packed := (map 'string encoding
                                               (loop
                                                 :with sysex := *sysex*
                                                 :for i :from start :below (- (length sysex) 2) :by 2
                                                 :collect (funcall quadsex
                                                                   (funcall quadswap (aref sysex i))
                                                                   (funcall quadswap (aref sysex (1+ i))))))
                              :do (print (list encoding quadsex permutation start (length packed)))
                                  (print-bank packed)
                              :when (search "Digitizer" packed)
                                :do (print packed))))))




(defparameter *sysbytes* (let ((start COM.INFORMATIMAGO.SYNTHESIZER.SCHMIDT::*data-offset*)
                               (quadsex 'big-quad)
                               (quadswap 'identity))
                           (loop
                             :with sysex := *sysex*
                             :for i :from start :below (- (length sysex) 2) :by 2
                             :collect (funcall quadsex
                                               (funcall quadswap (aref sysex i))
                                               (funcall quadswap (aref sysex (1+ i)))))))


(map nil (lambda (x) (print (list (cdr x) (car x))))
  (com.informatimago.common-lisp.cesarum.histogram:histogram-bins-and-labels
   *sysbytes* 256 :min-value 0 :max-value  256))

(length *sysbytes*)
(pprint (com.informatimago.common-lisp.cesarum.histogram:histogram-bins
         (com.informatimago.common-lisp.cesarum.histogram:histogram  *sysbytes* 256 :min-value 0 :max-value  256)))


#(0 11902 128 1 0 0 1 256 0 1664 380 1 127 127 126 0 0 259 3 0 1 127 127 128 1 253 1 0 0 384 0 0 0 1407 1 2 130 379 0
  129 0 513 1 0 2 0 0 0 1 641 1 639 0 1 0 0 0 126 129 0 382 2 1 0 0 612 511 254 0 0 0 127 0 0 3 2 1 0 1 0 127 128 0 127
  0 129 1 1 1 2 0 0 1 0 128 1 0 128 0 128 127 126 128 0 126 129 0 1 127 0 1 2 0 126 1 128 1 0 0 0 127 0 1 0 1 0 0 0 0
  130 0 253 0 127 0 128 4 128 0 3 380 255 128 0 128 128 126 2 0 0 0 1 128 0 0 1 0 0 126 2 0 3 0 2 0 256 1 1 2 0 1 1 1 1
  1 0 1 1 0 0 0 1 0 0 1 254 0 127 0 0 128 0 0 0 1 0 0 0 0 0 128 1 507 253 1 0 254 0 126 0 1 1 0 0 1 0 253 1 0 0 0 0 511
  1 0 1 3 128 128 0 0 1 509 255 255 0 0 0 1 0 253 255 129 0 1 0 1 2 127 1 2 1 512 0 0 1 1154 0)


(loop :for byte :in *sysbytes*
      :for i :from 0 :to 127
      :when (zerop (mod i 16))
        :do (terpri)
      :do (format t "~8,'0B" byte))


10001111101011111001111000000000110010110000000011111111111011110000000000000000000000000000000110100000001000000000000000110001
00110010011100101010001001100010111100010000100000111001100100101111101010001010111000010000000001111011011100110000000010001010
11100000000000000100100111111001010110001001011111111111010111011110110110100111010011010000000010111101000000000000000010000000
00000000100001110010011000010110000010000000100000000000000000000000000000000000010000101010000000000000110011010010001100000000
00000000100011010000000000000000110001110000000001101101000000000000100000000000110110000000000000000000100101110010101100000000
00110000111111110000000000000000000000001001001000000000001110111101011100111101100001110000000000000000000000000000000000000000
00000000101110100000000000011100010110000000100000111100011010110101010000000000110101010000000000000001000100000000000000000000
00000000000000000000000000000000111100110111000100000110010010011111010110001010111010011100100100000000000000000000000000000000


(loop :for byte :in (append (map 'list (lambda (c) (position c #-(and) com.informatimago.common-lisp.cesarum.ascii:*ascii-characters*
                                                      *schmidt-code*))
                       "PJB") '(-6 -8))
      :for i :from 0 :to 127
      :when (zerop (mod i 16))
        :do (terpri)
      :do (format t "~8,'0B "
                  (if (minusp byte)
                      (+ 256 byte)
                      byte)))

00010000 00001010 00000010 111010 111000

|#
