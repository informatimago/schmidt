;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               loader.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    This file loads the schmidt-librarian program and its dependencies.
;;;;
;;;;AUTHORS
;;;;    <PJB> Pascal J. Bourguignon <pjb@informatimago.com>
;;;;MODIFICATIONS
;;;;    2021-04-29 <PJB> Adapted for schmidt-librarian.
;;;;    2017-08-15 <PJB> Created.
;;;;BUGS
;;;;LEGAL
;;;;    AGPL3
;;;;
;;;;    Copyright Pascal J. Bourguignon 2017 - 2021
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

(in-package "COMMON-LISP-USER")

(eval-when (:compile-toplevel :load-toplevel)
  (error "This file should be loaded as source, not compiled"))

;;;----------------------------------------
(defmacro with-directory (directory-expression &body body)
  (let ((vsaved (gensym)))
    `(let ((,vsaved (uiop:getcwd)))
       (unwind-protect (progn
                         (uiop:chdir ,directory-expression)
                         (locally ,@body))
         (uiop:chdir (namestring ,vsaved))))))


(defvar *source-directory*
  (make-pathname :defaults *load-truename*
                 :name nil :type nil :version nil)
  "This source directory.")

(defvar *base-directory*
  (merge-pathnames (make-pathname :directory '(:relative :up "dependencies"))
                   *source-directory*
                   nil)

  "Base directory, where this source directory is, and where we store
the dependencies.")

;;;----------------------------------------

(defun clone-repository (repository test-file)
  (unless (probe-file (merge-pathnames test-file *base-directory* nil))
    (with-directory (namestring *base-directory*)
      (uiop:run-program (list "git" "clone" repository)))))

(defun setup-dependency (repository test-file)
  (clone-repository repository test-file)
  (setf asdf:*central-registry*
        (append (delete-duplicates
                 (mapcar (lambda (path)
                           (truename (make-pathname :name nil :type nil :version nil
                                                    :defaults path)))
                         (directory (merge-pathnames
                                     (make-pathname :directory '(:relative :wild-inferiors)
                                                    :name :wild :type "asd" :version nil)
                                     test-file)))
                 :test (function equalp))
                asdf:*central-registry*)))

(setup-dependency "git@framagit.org:patchwork/CoreMIDI.git"                  "../dependencies/CoreMIDI/coremidi.lisp")
(setup-dependency "git@framagit.org:abnotation/midi.git"                     "../dependencies/midi/midi.lisp")
(setup-dependency "git@framagit.org:com-informatimago/com-informatimago.git" "../dependencies/com-informatimago/com.informatimago.asd")
(push #P"./" asdf:*central-registry*)

(require "OBJC-SUPPORT")
(ql:quickload :com.informatimago.synthesizer.schmidt.librarian)

(load "cffi-utils.lisp")

(defun foreign-raw-string-to-lisp (pointer)
  "Copy at most COUNT bytes from POINTER plus OFFSET encoded in
ENCODING into a Lisp string and return it.  If POINTER is a null
pointer, NIL is returned."
  (unless (cffi:null-pointer-p pointer)
    (com.informatimago.cffi-utils:foreign-null-terminated-vector pointer :uchar 'character #'code-char)))



;; (cd #P"~/works/synth/schmidt/sources/librarian/")
;;
;; (pushnew #P"~/works/synth/CoreMIDI/"                  asdf:*central-registry* :test (function equalp))
;; (pushnew #P"~/works/synth/midi/"                      asdf:*central-registry* :test (function equalp))
;; (pushnew #P"~/works/synth/schmidt/sources/librarian/" asdf:*central-registry* :test (function equalp))
;;
;; (ql:quickload :com.informatimago.synthesizer.schmidt.librarian)
;;
;; (in-package :com.informatimago.synthesizer.schmidt)
;; (in-package :com.informatimago.synthesizer.schmidt.librarian)
