;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;;
;;; This file is an addendum to GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (wip gcompris-qt)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt))

(define-public gcompris-qt
  (package
    (name "gcompris-qt")
    (version "0.60")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "http://gcompris.net/download/qt/src/gcompris-qt-"
               version ".tar.xz"))
        (sha256
         (base32
          "1xyjlzraizg81mmfh9akscn2vyclchrzlp5b8pvi6b2xfdsqpzq4"))))
    (build-system cmake-build-system)
    ;(arguments
    ; `(#:configure-flags
    ;   (list (string-append "Qt5Qml_DIR=" (assoc-ref %build-inputs "qtdeclarative") "/lib/cmake/Qt5Qml/"))))
     ;`(#:phases
     ;  (modify-phases %standard-phases
     ;    (add-before 'configure 'set-CFLAGS
     ;      (lambda _
     ;        (setenv "Qt5Qml_DIR=" (string-append (assoc-ref %build-inputs "qtdeclarative") "/lib/cmake/Qt5Qml/"))
     ;        #t)))))
    (native-inputs
     `(("extra-cmake-modules" ,extra-cmake-modules)
       ("perl" ,perl)
       ("python-2" ,python-2)))
    (inputs
     `(
       ;("qtbase" ,qtbase)
       ;("qtdeclarative" ,qtdeclarative)
       ;("qttools" ,qttools)
       ;("qtmultimedia" ,qtmultimedia)
       ;("qtsensors" ,qtsensors)
       ;("qtsvg" ,qtsvg)
       ;("qtxmlpatterns" ,qtxmlpatterns)
       ("qt" ,qt)
       ))
    (home-page "http://gcompris.net/index-en.html")
    (synopsis "Educational games for small children")
    (description
     "A large collection of educational games for small children, designed to be
a unified interface to integrate more educational games.  Language-oriented
games contain vocabulary, sounds, and voices for many different languages.
Currently available boards include:
@enumerate
@item learning how to use a mouse and keyboard
@item learning simple arithmetic
@item learning how to read an analog clock
@item recognize letters after hearing their names
@item reading practice
@item small games (memory games, jigsaw puzzles, ...)
@end enumerate\n")
    (license license:gpl3+)))
