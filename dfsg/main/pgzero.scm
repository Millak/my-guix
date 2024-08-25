;;; Copyright © 2024 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main pgzero)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages python-xyz))

(define-public python-pgzero
  (package
    (name "python-pgzero")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pgzero" version))
       (sha256
        (base32 "01k1iv1qdy9kyizr3iysxqfmy10w38qvjfxx1hzarjr8y0hc1bcc"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-numpy python-pygame))
    (home-page "http://pypi.python.org/pypi/pgzero")
    (synopsis "Zero-boilerplate 2D games framework")
    (description "This package provides a zero-boilerplate 2D games framework
built around pygame.")
    ;; Not including the licenses of the files in the examples directory.
    (license license:lgpl3)))
