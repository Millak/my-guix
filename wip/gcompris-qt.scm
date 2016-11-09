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
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt))

(define-public gcompris-qt
  (package
    (name "gcompris-qt")
    (version "0.61")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "http://gcompris.net/download/qt/src/gcompris-qt-"
               version ".tar.xz"))
        (sha256
         (base32
          "0f6cm1zfkp8mvr7j3mxgq3d07jd1by825sbk28ypgcqdb1k7faw7"))))
    (build-system cmake-build-system)
    ;(arguments
    ; '(#:configure-flags
    ;   (list (string-append "Qt5Qml_DIR=" (assoc-ref %build-inputs "qtdeclarative") "/lib/cmake/Qt5Qml/"))))
     ;'(#:phases
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
       ("qml-box2d" ,qml-box2d)
       ("qtbase" ,qtbase)
       ("qtdeclarative" ,qtdeclarative)
       ("qttools" ,qttools)
       ("qtmultimedia" ,qtmultimedia)
       ("qtsensors" ,qtsensors)
       ("qtsvg" ,qtsvg)
       ("qtxmlpatterns" ,qtxmlpatterns)
       ;("qt" ,qt)
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

(define-public qml-box2d
  (let ((commit "1b37be7d9dfb44ec6d520595a4e4f45f63717822")
        (revision "1"))
    (package
      (name "qml-box2d")
      (version (string-append "0.0.0-" revision "." (string-take commit 7)))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                (url "https://github.com/qml-box2d/qml-box2d.git")
                (commit commit)))
          (file-name (string-append name "-" version "-checkout"))
          (sha256
           (base32
            "08167byrvb7hb8dh3ws49q8yr0b05b72mcqaqvhy720wwsac0x1v"))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (replace 'configure
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (substitute* "box2d.pro"
                              (("target.path = \\$\\$\\{importPath\\}")
                               (string-append "target.path = " out)))
                 (zero? (system* "qmake"
                                 (string-append "PREFIX=" out)))))))
         ;#:tests? #f ; no test target
         ))
      (native-inputs
       `(("qtquick" ,qtdeclarative)))
      (inputs
       `(("qtbase" ,qtbase)))
      (home-page "https://wiki.qt.io/Box2D")
      (synopsis "Qt-based 2D physics engine")
      (description
       "Box2D is an open source C++ physics engine for simulating collisions
and other 2D motions of rigid bodies which can be easily integrated with Qt.")
      (license license:zlib))))
