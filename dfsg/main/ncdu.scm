;;; Copyright © 2022 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main ncdu)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (gnu packages ncdu)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages zig))

(define-public ncdu-2
  (package
    (inherit ncdu)
    (name "ncdu2")      ; only to prevent it from being installed by accident
    (version "2.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://dev.yorhel.nl/download/ncdu-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0j3w8xixz1zkzcpk0xrh6y3r7sii3h3y31lbvs5iqc5q7q6day9g"))))
    (arguments
     (list
       #:make-flags
       #~(list (string-append "PREFIX=" #$output)
               (string-append "CC=" #$(cc-for-target)))
       #:phases
       #~(modify-phases %standard-phases
           (delete 'configure)      ; No configure script.
           (add-before 'build 'pre-build
             (lambda _
               (setenv "ZIG_GLOBAL_CACHE_DIR"
                       (mkdtemp "/tmp/zig-cache-XXXXXX"))))
           (add-after 'build 'build-manpage
             (lambda _
               (delete-file "ncdu.1")
               (invoke "make" "doc")))
           (replace 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (when tests?
                 (invoke "zig" "test" "build.zig")))))))
    (native-inputs
     (list perl zig))))
