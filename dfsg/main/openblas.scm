;;; Copyright © 2023 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main openblas)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages maths))

;; Is this worth it? Build time should be much faster, and for my 3900XT
;; machine the library is 14MB instead of 39MB. On the other hand, the larger
;; library is good for all possible x86_64 sub-architectures.

;; It is not possible to combine support for different architectures
;; so no combined 32 and 64 bit or x86_64 and arm64 in the same library.

;; TARGET with DYNAMIC_ARCH selects the oldest CPU model to target.
;; DYNAMIC_ARCH supported by x86_64, i686, aarch64, ppc64le.

;; DYNAMIC_LIST with DYNAMIC_ARCH allows selecting specific CPUs to target, rather than all.
;; As of 0.3.14 only supported by x86_64 and aarch64.

;; And what about openblas-ilp64?

;; 3900XT   -> TARGET=ZEN
;; pbp      -> TARGET=CORTEXA53

(define-public openblas-native
  (package
    (inherit openblas)
    (arguments
     (substitute-keyword-arguments (package-arguments openblas)
       ((#:substitutable? _ #t) #f)
       ((#:modules modules '((guix build gnu-build-system)
                             (guix build utils)))
        `((ice-9 regex)
          (srfi srfi-1)
          (srfi srfi-26)
          ,@modules))
       ((#:make-flags flags ''())
        #~(remove (cut string-match "(DYNAMIC_|TARGET).*" <>)
                  #$flags))))
    ;; Hide the package so we don't have to override the name length.
    (properties `((hidden? . #t)
                  ,@(package-properties openblas)))))

(define-public openblas-zen
  (package
    (inherit openblas-native)
    (arguments
     (substitute-keyword-arguments (package-arguments openblas-native)
       ((#:make-flags flags ''())
        #~(cons*
            "TARGET=ZEN"
            (remove (cut string-match "(DYNAMIC_|TARGET).*" <>)
                    #$flags)))))
    (supported-systems '("x86_64-linux"))))

(define-public openblas-cortexa53
  (package
    (inherit openblas-native)
    (arguments
     (substitute-keyword-arguments (package-arguments openblas-native)
       ((#:make-flags flags ''())
        #~(cons*
            "TARGET=CORTEXA53"
            (remove (cut string-match "(DYNAMIC_|TARGET).*" <>)
                    #$flags)))))
    (supported-systems '("aarch64-linux"))))

(define-public openblas-cortexa53.a57
  (package
    (inherit openblas-native)
    (arguments
     (substitute-keyword-arguments (package-arguments openblas-native)
       ((#:make-flags flags ''())
        #~(cons*
            "DYNAMIC_ARCH=1"
            "DYNAMIC_LIST=CORTEXA53 CORTEXA72"
            (remove (cut string-match "(DYNAMIC_|TARGET).*" <>)
                    #$flags)))))
    (supported-systems '("aarch64-linux"))))

(define-public openblas-ppcg4
  (package
    (inherit openblas-native)
    (arguments
     (substitute-keyword-arguments (package-arguments openblas-native)
       ((#:make-flags flags ''())
        #~(cons*
            "TARGET=PPCG4"
            (remove (cut string-match "(DYNAMIC_|TARGET).*" <>)
                    #$flags)))))
    (supported-systems '("powerpc-linux"))))
