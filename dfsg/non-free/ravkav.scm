;;; Copyright © 2017, 2018, 2020-2022 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg non-free ravkav)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages bootstrap) ; glibc-dynamic-linker
  #:use-module (gnu packages elf)
  #:use-module (gnu packages security-token))

;; Check https://ravkavonline.s3.amazonaws.com/ for updates.
(define-public ravkavonline
  (package
    (name "ravkavonline")
    (version "2.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://ravkavonline.s3.amazonaws.com/linux/"
                            "ravkavonline_" version "_amd64.deb"))
        (sha256
         (base32
          "035pjyp7y1pgi1zb1vcp9x97axq6wrnn9cax4m54abh1jk8h89q0"))))
    (build-system gnu-build-system)
    (arguments
     (list
       #:phases
       #~(modify-phases %standard-phases
           (delete 'configure)      ; No configure script.
           (replace 'unpack
             (lambda _
               (invoke "ar" "x" #$source "data.tar.gz")
               (invoke "tar" "xvf" "data.tar.gz")))
           (replace 'build
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((ravkav "usr/bin/ravkavonline")
                     (ld-so  (search-input-file inputs #$(glibc-dynamic-linker)))
                     (rpath  (dirname
                               (search-input-file inputs "/lib/libpcsclite.so"))))
                 (invoke "patchelf" "--set-rpath" rpath ravkav)
                 (invoke "patchelf" "--set-interpreter" ld-so ravkav)
                 (substitute* "usr/share/applications/ravkavonline.desktop"
                   (("/usr") #$output)))))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out #$output))
                 (with-directory-excursion "usr"
                   (install-file "bin/ravkavonline" (string-append out "/bin"))
                   (install-file "share/applications/ravkavonline.desktop"
                                 (string-append out "/share/applications"))
                   (install-file "share/doc/ravkavonline/LICENSE.txt"
                                 (string-append out "/share/doc/"
                                                #$name "-" #$version)))))))
         #:substitutable? #f        ; Non-redistributable.
         #:strip-binaries? #f       ; Causes seg faults.
         #:tests? #f))              ; No tests.
    (native-inputs (list patchelf))
    (inputs (list pcsc-lite))
    (home-page "https://ravkavonline.co.il/he/")
    (synopsis "Charge your ravkav at home")
    (description "Save time and charge your ravkav at home.")
    (supported-systems '("x86_64-linux"))
    (license #f)))
