;;; Copyright © 2021 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main openssl)
  #:use-module (guix packages)
  #:use-module (gnu packages tls))

(define-public openssl-native
  (package/inherit openssl
    (name "openssl")
    (arguments
     `(#:make-flags (list "CFLAGS=-march=native -Wall -O3")
       #:substitutable? #f
       ,@(package-arguments openssl)))))

;; Openssl doesn't currently use configure-flags.
;; Also this doesn't seem to work
;(define-public openssl-1.0-native
;  (package/inherit openssl-native
;    (name "openssl-1.0-native")
;    (arguments
;     `(#:configure-flags (list "--api=1.0.0")
;       ,@(package-arguments openssl-native)))))
