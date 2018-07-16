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

(define-module (dfsg main xorriso)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages tcl)
  #:use-module (srfi srfi-1))

(define-public my-xorriso
  (package
    (inherit xorriso)
    (name "my-xorriso")
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'patch-source-shebangs 'patch-wish-shebang
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((tk (assoc-ref inputs "tk")))
               (substitute* "frontend/xorriso-tcltk"
                 (("/usr/bin/wish")
                  (string-append "/usr/bin/wish" ,(version-major+minor
                                                    (package-version tk)))))
               #t))))))
    (inputs
     `(("tk" ,tk)
       ,@(package-inputs xorriso)))))
