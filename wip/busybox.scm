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

(define-module (wip busybox)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (gnu packages bootstrap)
  #:use-module (gnu packages busybox))

(define-public busybox-boot0
  (package
    (inherit busybox)
    (name "busybox-boot0")
    (arguments
     (substitute-keyword-arguments (package-arguments busybox)
       ((#:guile _ #f) %bootstrap-guile)
       ((#:implicit-inputs? _ #t) #f)
       ((#:tests? _ #t) #f)     ; TODO: Later
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
           #;(replace 'configure
             (lambda* (#:key make-flags #:allow-other-keys)
               (apply invoke "make" "defconfig" make-flags)))
           #;(add-before 'configure 'pre-configure
             (lambda _
               (substitute* "Makefile"
                 ((" gcc") " x86_64-guix-linux-gnu-gcc"))))
           (replace 'build
             (lambda* (#:key make-flags #:allow-other-keys)
               (apply invoke "make" "busybox" make-flags)))
           ))
         ))
    (native-inputs
     `(
       ("binutils" ,(@@ (gnu packages commencement) binutils-boot0))    ; binutils-mesboot misses ld?
       ("coreutils" ,(@@ (gnu packages commencement) coreutils-mesboot))
       ("findutils" ,(@@ (gnu packages commencement) findutils-boot0))
       ("sed" ,(@@ (gnu packages commencement) sed-mesboot))
       ("bison" ,(@@ (gnu packages commencement) bison-boot0))
       ("flex" ,(@@ (gnu packages commencement) flex-boot0))
       ("bzip2" ,(@@ (gnu packages commencement) bzip2-boot0))  ; need bzip2 compression
       ;("libc" ,(@@ (gnu packages commencement) glibc-mesboot))
       ("libc" ,(@@ (gnu packages commencement) glibc-final-with-bootstrap-bash))
       ;("make" ,(@@ (gnu packages commencement) gnu-make-mesboot0))    ; hardcoded /bin/sh from package
       ("make" ,(@@ (gnu packages commencement) gnu-make-boot0))
       ;("kernel-headers" ,((@@ (gnu packages commencement) kernel-headers-boot0)))
       ;("tcc" ,(@@ (gnu packages commencement) tcc-boot0))
       ;("gcc" ,(@@ (gnu packages commencement) gcc-boot0))
       ("gcc" ,(@@ (gnu packages commencement) gcc-boot0-intermediate-wrapped))
       ;("bash" ,(@@ (gnu packages commencement) gash-boot))
       ("bash" ,(@@ (gnu packages commencement) bash-mesboot))
       ("coreutils" ,(@@ (gnu packages commencement) gash-utils-boot))
       ("bootar" ,(@@ (gnu packages commencement) bootar))  ; bzip2 (decompression only), gzip, xz, tar
       ))
    ))
