;;; Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main wgetpaste)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages wget))

(define-public my-wgetpaste
  (package
    (inherit wgetpaste)
    (name "my-wgetpaste")
    (arguments
      (substitute-keyword-arguments (package-arguments wgetpaste)
        ((#:phases phases)
         `(modify-phases ,phases
            (add-after 'unpack 'patch-wget
              (lambda _
                (substitute* "wgetpaste"
                  ((" wget -")
                   (string-append " " (which "wget") " -")))
                #t))))))
    (inputs
     `(("wget" ,wget)
       ,@(package-inputs wgetpaste)))))
