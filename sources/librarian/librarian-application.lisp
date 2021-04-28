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
        "COM.INFORMATIMAGO.MIDI.ABSTRACT-MIDI-APPLICATION"
        "COM.INFORMATIMAGO.MIDI.ABSTRACT-SYNTHESIZER"
        "COM.INFORMATIMAGO.SYNTHESIZER.SCHMIDT"
        "COM.INFORMATIMAGO.SYNTHESIZER.SCHMIDT-SYNTHESIZER"
        "COM.INFORMATIMAGO.COMMON-LISP.CESARUM.FILE")
  (:shadowing-import-from "COM.INFORMATIMAGO.MIDI.ABSTRACT-SYNTHESIZER"
                          "PARAMETER-NAME"
                          "PARAMETER-VALUES"
                          "PROGRAM"
                          "PROGRAM-NAME")
  (:export "RUN"))
(in-package "COM.INFORMATIMAGO.SYNTHESIZER.SCHMIDT.LIBRARIAN.APPLICATION")

(defclass librarian-application (midi-application)
  ())

(defmethod synthesizer ((application librarian-application)))
