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

(define-module (wip lekha)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages enlightenment)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages python))

(define-public lekha
  (package
    (name "lekha")
    (version "0.2.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "Lekha" version))
              (sha256
               (base32
                "0zr6i74ik58pbzrd7r9l7sawqbdv0r2c1a9927qkqzwga27x8j15"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; no test target
       #:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-data-location
           (lambda _
             (substitute* "setup.py"
                          (("'/usr/")"'"))
             #t)))))
    (inputs
     `(("python2-efl" ,python2-efl)
       ("python2-pypdf2" ,python2-pypdf2)
       ("python2-pyxdg" ,python2-pyxdg)))
    (synopsis "Simple PDF viewer")
    (description "Simple PDF viewer")
    (home-page "https://www.enlightenment.org")
    (license gpl3+)))

(define-public python-pypdf2
  (package
    (name "python-pypdf2")
    (version "1.26.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "PyPDF2" version))
        (sha256
         (base32
          "11a3aqljg4sawjijkvzhs3irpw0y67zivqpbjpm065ha5wpr13z2"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (zero? (system* "python" "-m" "unittest" "Tests.tests")))))))
    (home-page "http://mstamy2.github.com/PyPDF2")
    (synopsis "Utility to read and write PDFs with Python")
    (description "A Pure-Python library built as a PDF toolkit.  It is capable
of extracting document information (title, author, ...), splitting documents
page by page, merging documents page by page, cropping pages, merging multiple
pages into a single page, encrypting and decrypting PDF files, and more.")
    (license bsd-3)))

(define-public python2-pypdf2
  (package-with-python2 python-pypdf2))
