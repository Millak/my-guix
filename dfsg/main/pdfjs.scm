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

(define-module (dfsg main pdfjs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system copy))

(define-public pdfjs
  (package
    (name "pdfjs")
    (version "3.4.120")
    (source
      (origin
        (method url-fetch/zipbomb)
        (uri (string-append "https://github.com/mozilla/pdf.js/releases"
                            "/download/v" version
                            "/pdfjs-" version "-legacy-dist.zip"))
        (sha256
         (base32 "02q00qwl77f2cds0hhknsda10r153xnzkdnhvnl084v19nwhvmp7"))))
    (build-system copy-build-system)
    (arguments `(#:install-plan
                 '(("." "share/pdfjs"))))
    (home-page "https://mozilla.github.io/pdf.js/")
    (synopsis "PDF reader in Javascript")
    (description
     "PDF.js is a Portable Document Format (PDF) viewer that is built with HTML5.")
    (properties
     '((release-monitoring-url . "https://github.com/mozilla/pdf.js/releases")))
    (license license:asl2.0)))
