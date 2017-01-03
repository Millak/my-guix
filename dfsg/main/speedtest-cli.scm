;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main speedtest-cli)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system python))

(define-public speedtest-cli
  (package
    (name "speedtest-cli")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "speedtest-cli" version))
        (sha256
         (base32
          "1n9jnyv5yyl0zy4ba10sq0a4gkz1knmf4ihh7malpb5npk622cfj"))))
    (build-system python-build-system)
    (home-page "https://github.com/sivel/speedtest-cli/")
    (synopsis "Command line interface for speedtest.net")
    (description
     "Speedtest.net is a webservice that allows you to test your broadband
connection by downloading a file from one of many Speedtest.net servers from
around the world.")
    (license license:asl2.0)))
