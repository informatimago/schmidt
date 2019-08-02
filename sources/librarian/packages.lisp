;;;; -*- mode:lisp;coding:utf-8 -*-
;;;;**************************************************************************
;;;;FILE:               packages.lisp
;;;;LANGUAGE:           Common-Lisp
;;;;SYSTEM:             Common-Lisp
;;;;USER-INTERFACE:     NONE
;;;;DESCRIPTION
;;;;
;;;;    Defines the packages.
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

(defpackage "COM.INFORMATIMAGO.SYNTHESIZER.SCHMIDT"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.UTILITY"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.STRING")
  (:export
   "BANK-FROM-SYSEX"
   "VALID-BANK-CHECKSUM-P"
   "BANK-COMPUTE-CHECKSUM"
   "BANK-CHECKSUM"
   "BANK-PROGRAM-COUNT"
   "PROGRAM"
   "PROGRAM-NAME"
   "PROGRAM-COLORS"
   "SCHMIDT-CODE-CHAR"
   "SCHMIDT-CHAR-CODE"))

(defpackage "COM.INFORMATIMAGO.SYNTHESIZER.SCHMIDT.LIBRARIAN"
  (:use "COMMON-LISP"
        "COM.INFORMATIMAGO.SYNTHESIZER.SCHMIDT"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.FILE")
  (:export "RUN"))

;;;; THE END ;;;;
