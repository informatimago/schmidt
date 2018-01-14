;;;; uml.asd

;; This file is part of UML library.

;; UML is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; UML is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with UML.  If not, see <http://www.gnu.org/licenses/>.

(asdf:defsystem #:uml
  :description "UML file generator"
  :author "Angelo Rossi <angelo.rossi.homelab@gmail.com>"
  :license "GPL3"
  :depends-on (#:cl-svg)
  :serial t
  :components ((:file "package")
               (:file "uml")))

