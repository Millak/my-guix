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

(define-module (packages tar)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression))

(define-public tar-custom
  (package (inherit tar)
    (name "tar-custom")
    (arguments
      (substitute-keyword-arguments (package-arguments tar)
        ((#:configure-flags cf)
         `(append ,cf
                  (string-append "--with-bzip2="
                                 (assoc-ref %build-inputs "pbzip2")
                                 "/bin/pbzip2")
                  (string-append "--with-gzip="
                                 (assoc-ref %build-inputs "pigz")
                                 "/bin/pigz")
                  (string-append "--with-xz="
                                 (assoc-ref %build-inputs "pixz")
                                 "/bin/pixz")))))
    (inputs
     `(("pbzip2" ,pbzip2)
       ("pigz" ,pigz)
       ("pixz" ,pixz)
       ,@(package-inputs tar)))))
