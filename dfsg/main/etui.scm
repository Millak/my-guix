;;; Copyright © 2019, 2020 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main etui)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system meson)
  #:use-module (dfsg main mupdf)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages djvu)
  #:use-module (gnu packages enlightenment)
  #:use-module (gnu packages image)
  #:use-module (gnu packages pkg-config))

(define-public etui
  (let ((commit "fe278033519cec8852d83fe45af677457bde1325")
        (revision "1"))
  (package
    (name "etui")
    (version (git-version "0.0.4" revision commit))
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/vtorri/etui")
               (commit commit)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "12765smiw31n5bg588r8qgmm69438cv4nby3ss6m621ydv9zx5zm"))))
    (build-system meson-build-system)
    (arguments
     '(#:configure-flags
       (let ((mupdf (assoc-ref %build-inputs "mupdf")))
         (list "-Dlicense=agplv3"
               (string-append "-Dmupdf-includedir=" mupdf "/include")
               (string-append "-Dmupdf-libdir=" mupdf "/lib -lmupdf")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-home-directory
           (lambda _
             (setenv "HOME" (getcwd))
             #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("djvulibre" ,djvulibre)
       ("efl" ,efl)
       ("jbig2dec" ,jbig2dec)
       ("libarchive" ,libarchive)
       ("libtiff" ,libtiff)
       ("mupdf" ,my-mupdf)
       ("openjpeg" ,openjpeg)))
    (home-page "https://github.com/vtorri/etui")
    (synopsis "Multi-document rendering library using the EFL")
    (description "Multi-document rendering library using the EFL.")
    (license license:agpl3+)))) ; because we link to mupdf
