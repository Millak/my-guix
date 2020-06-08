;;; Copyright © 2019 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg non-free parallel)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages parallel))

(define-public gnu-parallel
  (package
    (inherit parallel)
    (name "gnu-parallel")
    (arguments
     `(#:configure-flags '("--program-prefix=gnu-")
       ,@(substitute-keyword-arguments (package-arguments parallel)
           ((#:phases phases)
            `(modify-phases ,phases
               (add-after 'strip 'remove-sem
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((out (assoc-ref outputs "out")))
                     (delete-file (string-append out "/bin/sem")))
                   #t))
               (replace 'wrap-program
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (let ((out (assoc-ref outputs "out")))
                     (wrap-program (string-append out "/bin/gnu-parallel")
                       `("PATH" ":" prefix
                         ,(map (lambda (input)
                                 (string-append (assoc-ref inputs input) "/bin"))
                               '("perl"
                                 "procps"))))
                     #t)))
               (replace 'post-install-test
                 (lambda* (#:key outputs #:allow-other-keys)
                   (invoke (string-append
                             (assoc-ref outputs "out") "/bin/gnu-parallel")
                           "echo"
                           ":::" "1" "2" "3"))))))))))
