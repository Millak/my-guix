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

(define-module (wip etui)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages enlightenment)
  #:use-module (gnu packages pdf))

(define-public etui
  (let ((commit "852e8aa62118399f23d6d3690368d731b9a3f339")
        (revision "1"))
  (package
    (name "etui")
    (version (string-append "0.0.0-" revision "." (string-take commit 7)))
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/vtorri/etui")
               (commit commit)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "145kg29z97gbvabhwfdikcl87gd3pfa6qan6npx6gip0ljm4wwzg"))))
    (build-system meson-build-system)
    (arguments
     '(#:configure-flags '("-Dlicense=agplv3")))
    (inputs
     `(
       ("efl" ,efl)
       ("mupdf" ,mupdf)
       ))
    (home-page "https://github.com/vtorri/etui")
    (synopsis "Multiple Document Library and Viewer")
    (description "Multiple Document Library and Viewer")
    (license license:gpl3+))))
