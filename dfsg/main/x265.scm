;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main x265)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages assembly))

(define-public x265
  (package
    (name "x265")
    (version "2.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://download.videolan.org/videolan/x265/"
                            "x265_" version ".tar.gz"))
        (sha256
         (base32
          "07yf1fkl4qbbmjc8wiy379k3cpqjcdn9lpdyj3310cz3psbnfzss"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DENABLE_TESTS=ON"
                           "-HIGH_BIT_DEPTH=ON") ; 64-bit only
       ;; x265 tunes itself to the target's CPU
       ;#:substitutable? #f ; not available in cmake-build-system
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'prepare-build
           (lambda _
             (delete-file-recursively "build")
             (chdir "source")
             #t)))))
    (native-inputs
     `(("yasm" ,yasm)))
    (home-page "http://x265.org/")
    (synopsis "Library for encoding h.265/HEVC video streams")
    (description "x265 is a H.265 / HEVC video encoder application library,
designed to encode video or images into an H.265 / HEVC encoded bitstream.")
    (license license:gpl2+)))
