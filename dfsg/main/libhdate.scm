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

(define-module (dfsg main libhdate)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu))

(define-public libhdate
  (package
    (name "libhdate")
    (version "1.6.02")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://sourceforge/libhdate/libhdate/libhdate-"
                            version "/" name "-" version ".tar.bz2"))
        (sha256
         (base32
          "0qkpq412p78znw8gckwcx3l0wcss9s0dgw1pvjb1ih2pxf6hm4rw"))))
    (build-system gnu-build-system)
    (home-page "http://libhdate.sourceforge.net/")
    (synopsis "Library to use Hebrew dates")
    (description "LibHdate is a small library for the Hebrew calendar and times
of day, written in C, and including bindings for C++, pascal, perl, php, python,
and ruby.  It includes two illustrative command-line programs, hcal and hdate,
and some snippets and scripts written in the binding languages.")
    (license license:gpl3+)))
