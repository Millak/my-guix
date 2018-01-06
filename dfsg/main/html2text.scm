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

(define-module (dfsg main html2text)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system python))

(define-public html2text
  (package
    (name "html2text")
    (version "2017.10.4")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "html2text" version))
        (sha256
         (base32
          "1b8zikswwlbq4406lqsarqwh8s0637mjd3hqgrdkj2p90vr8vaq2"))))
    (build-system python-build-system)
    (home-page "https://github.com/Alir3z4/html2text/")
    (synopsis "Turn HTML into equivalent Markdown-structured text")
    (description "html2text is a Python script that converts a page of HTML
into clean, easy-to-read plain ASCII text.  Better yet, that ASCII also happens
to be valid Markdown (a text-to-HTML format).")
    (license license:gpl3)))
