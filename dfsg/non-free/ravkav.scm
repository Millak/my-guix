;;; Copyright © 2017. 2018 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages bootstrap) ; glibc-dynamic-linker
  #:use-module (gnu packages elf)
  #:use-module (gnu packages security-token))

(define-public ravkavonline
  (package
    (name "ravkavonline")
    (version "1.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://ravkavonline.co.il/releases/linux/"
                            "ravkavonline_" version "_amd64.deb"))
        (sha256
         (base32
          "0kzfxv3vz03m9sym1b0x9k22aav1rbvfb46xz2zy9hzkcvirral8"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'unpack
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((source (assoc-ref inputs "source")))
               (invoke "ar" "x" source "data.tar.xz"))))
         (add-after 'unpack 'unpack-tarball
           (lambda _
             (invoke "tar" "xvf" "data.tar.xz")))
         (replace 'build
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((ravkav  "usr/bin/ravkavonline")
                    (desk    "usr/share/applications/ravkavonline.desktop")
                    (out     (assoc-ref outputs "out"))
                    (libc    (assoc-ref inputs "libc"))
                    (ld-so   (string-append libc ,(glibc-dynamic-linker)))
                    (libpcsc (assoc-ref inputs "pcsc-lite"))
                    (rpath   (string-append libpcsc "/lib")))
                 (invoke "patchelf" "--set-rpath" rpath ravkav)
                 (invoke "patchelf" "--set-interpreter" ld-so ravkav)
                 (substitute* desk
                   (("usr") out)))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (with-directory-excursion "usr"
                 (install-file "bin/ravkavonline" (string-append out "/bin"))
                 (install-file "share/applications/ravkavonline.desktop"
                               (string-append out "/share/applications"))
                 (install-file "share/doc/ravkavonline/LICENSE.txt"
                               (string-append out "/share/doc/ravkavonline")))
               #t))))
       #:tests? #f))
    (native-inputs
     `(("patchelf" ,patchelf)))
    (inputs
     `(("pcsc-lite" ,pcsc-lite)))
    (home-page "https://ravkavonline.co.il/he/")
    (synopsis "Charge your ravkav at home")
    (description "Save time and charge your ravkav at home.")
    (supported-systems '("x86_64-linux"))
    (license #f)))
