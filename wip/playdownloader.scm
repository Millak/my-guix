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

(define-module (wip playdownloader)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system python)
  #:use-module (gnu packages python))

(define-public playdownloader
  (package
    (name "playdownloader")
    (version "1.8")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://codingteam.net/project/google"
                            name "/download/file/google"
                            name "_" version ".orig.tar.gz"))
        (sha256
         (base32
          "1hxl4wdbiyq8ay6vnf3m7789jg0kc63kycjj01x1wm4gcm4qvbkx"))))
    (build-system python-build-system)
    (arguments `(#:python ,python-2))
    (native-inputs
     `(
       ("python2-configparser" ,python2-configparser)
       ("python2-ndg-httpsclient" ,python2-ndg-httpsclient)
       ("python2-protobuf" ,python2-protobuf)
       ("python2-pyasn1" ,python2-pyasn1)
       ("python2-requests" ,python2-requests)
       ;("python2-wxgtk" ,python2-wxgtk)
       ))
    (home-page "https://codingteam.net/project/googleplaydownloader")
    (synopsis "Download APKs from the Play Store")
    (description "GooglePlayDownloader is a graphical software to download
APKs from the Google Play store.")
    (license license:agpl3+)))

(define-public python-protobuf
  (package
    (name "python-protobuf")
    (version "3.0.0b3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://pypi.python.org/packages/"
               "52/f8/1b0d57028ca6144a03e1fdb5eeca6bd10194dcbfc2405d920c47bb7a79ca"
               "/protobuf-" version ".tar.gz"))
        (sha256
         (base32
          "098mnxzd3y67j44y3fdpfszphhljp88flfr4a90qfvvpy4ka7w5l"))))
    (build-system python-build-system)
    (inputs
     `(("python-six" ,python-six)))
    (home-page "https://developers.google.com/protocol-buffers/")
    (synopsis "Protocol Buffers")
    (description "Protocol Buffers")
    (license #f)))

(define-public python2-protobuf
  (package (inherit (package-with-python2 python-protobuf))
    (native-inputs `(("python2-setuptools", python2-setuptools)))))
