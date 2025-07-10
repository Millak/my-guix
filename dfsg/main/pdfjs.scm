;;; Copyright © 2023-2025 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (guix build-system copy)
  #:use-module (gnu packages fonts))

(define-public pdfjs
  (package
    (name "pdfjs")
    (version "5.3.93")
    (source
      (origin
        (method url-fetch/zipbomb)
        (uri (string-append "https://github.com/mozilla/pdf.js/releases"
                            "/download/v" version
                            "/pdfjs-" version "-dist.zip"))
        (sha256
         (base32 "1aq95kkgzgibn3f0ixy8mz1r32zplm9acz54nzcj1g82cjhnwdwn"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       '(("." "share/pdfjs"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-fonts
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (liberation (string-append
                                 (assoc-ref inputs "font-liberation")
                                 "/share/fonts/truetype/")))
               (with-directory-excursion
                 (string-append out "/share/pdfjs/web/standard_fonts/")
                 (for-each (lambda (file)
                             (delete-file file)
                             (symlink (string-append liberation file) file))
                           (find-files "." "LiberationSans-.*\\.ttf$"))
                 (delete-file "LICENSE_LIBERATION"))))))))
    (inputs (list font-liberation))
    (home-page "https://mozilla.github.io/pdf.js/")
    (synopsis "PDF reader in Javascript")
    (description
     "PDF.js is a Portable Document Format (PDF) viewer that is built with HTML5.")
    (properties
     '((release-monitoring-url . "https://github.com/mozilla/pdf.js/releases")))
    (license license:asl2.0)))

(define-public pdfjs-legacy
  (package
    (inherit pdfjs)
    (name "pdfjs-legacy")
    (version (package-version pdfjs))
    (source
      (origin
        (method url-fetch/zipbomb)
        (uri (string-append "https://github.com/mozilla/pdf.js/releases"
                            "/download/v" version
                            "/pdfjs-" version "-legacy-dist.zip"))
        (sha256
         (base32 "1ib80jzj2abi43pvwv3kvlyp3i5g9v5rx3y1q13hdaqw46qpqg6d"))))))
