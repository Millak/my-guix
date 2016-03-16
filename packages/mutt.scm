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

(define-module (packages mutt)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages mail))

(define-public mutt-custom
  (package (inherit mutt)
    (name "mutt-custom")
    (arguments
      (substitute-keyword-arguments (package-arguments mutt)
        ((#:configure-flags cf)
         `(append ,cf '("--enable-hcache"
                        "--with-idn")))))
    (inputs
     `(("libidn" ,libidn)
       ("gdbm" ,gdbm)
       ,@(package-inputs mutt)))))
