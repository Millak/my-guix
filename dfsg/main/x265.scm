;;; GNU Guix --- Functional package management for GNU
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

(define-module (dfsg main x265)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages video)
  #:use-module (gnu packages assembly))

(define-public x265-custom
  (package
    (inherit x265)
    (name "x265-custom")
    (arguments
      (substitute-keyword-arguments (package-arguments x265)
        ((#:tests? _) #t)
        ((#:configure-flags _)
         `(list "-DENABLE_TESTS=ON"
                "-DHIGH_BIT_DEPTH=ON")))) ; 64-bit only
    (native-inputs
     `(("yasm" ,yasm)))))
