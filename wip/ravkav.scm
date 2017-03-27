;;; Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (wip ravkav)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages security-token))

(define-public ravkavonline
  (package
    (name "ravkavonline")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://ravkavonline.co.il/releases/linux/"
                            "ravkavonline_" version "_amd64.deb"))
        (sha256
         (base32
          "0kx1cjywbxl7gn0r81bdgr5m1ka3y45pm0fq1jgkfag1qnqm69hx"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'unpack
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((source (assoc-ref inputs "source")))
             (system* "ar" "x" source "data.tar.xz"))))
         (add-after 'unpack 'unpack-tarball
           (lambda _
             (zero? (system* "tar" "xvf" "data.tar.xz"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (with-directory-excursion "usr"
                 (install-file "bin/ravkavonline" (string-append out "/bin/ravkavonline"))
                 (install-file "share/applications/ravkavonline.desktop"
                               (string-append out "/share/applications/ravkavonline.desktop"))
                 (install-file "share/doc/ravkavonline/LICENSE.txt"
                               (string-append out "/share/doc/ravkavonline/LICENSE.txt"))))))
         )
       #:tests? #f))
    (inputs
     `(("pcsc-lite" ,pcsc-lite)))
    (home-page "https://ravkavonline.co.il/he/")
    (synopsis "")
    (description "")
    (license #f)))
