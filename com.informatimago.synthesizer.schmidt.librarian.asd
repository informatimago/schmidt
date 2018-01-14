;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               com.informatimago.synthesizer.schmidt.librarian.asd
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

(setf *readtable* (copy-readtable nil))

(asdf:defsystem "com.informatimago.synthesizer.schmidt.librarian"
  :description "Librarian for the Schmidt Synthesizer."
  :author "Pascal J. Bourguignon"
  :version "1.0.0"
  :license "AGPL3"
  :depends-on ("cffi"
               "midi"
               "trivial-timers"
               "com.informatimago.macosx.coremidi"
               "com.informatimago.macosx.coremidi.midi"
               "com.informatimago.common-lisp.cesarum"
               "com.informatimago.common-lisp.interactive")
  :components ((:file "packages")
               (:file "schmidt"   :depends-on ("packages"))
               (:file "librarian" :depends-on ("packages" "schmidt")))
  #+asdf-unicode :encoding #+asdf-unicode :utf-8)

;;;; THE END ;;;;
