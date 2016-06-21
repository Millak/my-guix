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

(define-module (packages onionshare)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system python)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt))

(define-public onionshare
  (package
    (name "onionshare")
    (version "0.9")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/micahflee/onionshare/archive/"
                            version ".tar.gz"))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0pc3xbq379415s0i0y6rz02hay20zbvgra1jmg4mgrl9vbdr8zmw"))))
    (build-system python-build-system)
    (inputs
     `(("python-flask" ,python-flask)
       ("python-nautilus" ,python-nautilus)
       ("python-stem" ,python-stem)
       ("python-pyqt" ,python-pyqt)))
    (home-page "https://onionshare.org/")
    (synopsis "Securely and anonymously share files")
    (description "OnionShare lets you securely and anonymously share files of
any size.  It works by starting a web server, making it accessible as a Tor
hidden service, and generating an unguessable URL to access and download the
files.  It doesn't require setting up a server on the internet somewhere or
using a third party filesharing service.  You host the file on your own computer
and use a Tor hidden service to make it temporarily accessible over the
internet.  The other user just needs to use Tor Browser to download the file
from you.")
    (license license:gpl3+)))

(define-public python-nautilus
  (package
    (name "python-nautilus")
    (version "0.4.9")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://pypi.python.org/packages/"
               "5c/9c/ed9015ffc10219e70bf01a33c912c3fbd1400d356c0e6aa606cec4b0932b"
               "/nautilus-" version ".tar.gz"))
        (sha256
         (base32
          "01hwzjc1zshk4vvxrcghm398fpy4jls66dyz06g07mrwqif8878p"))))
  (build-system python-build-system)
  (inputs
   `(
     ;("python-bcrypt" ,python-bcrypt)
     ("python-py-bcrypt" ,python-py-bcrypt) ; should be compatable
     ("python-click" ,python-click)
     ;("python-consul" ,python-consul)
     ;("python-graphene" ,python-graphene)
     ("python-jinja2" ,python-jinja2)
     ;("python-nose2" ,python-nose2)
     ;("python-peewee" ,python-peewee)
     ;("python-pika" ,python-pika)
     ("python-tornado" ,python-tornado)
     ("python-wtforms" ,python-wtforms)
     ))
    (home-page "https://github.com/AlecAivazis/nautilus")
    (synopsis "A library for creating microservice applications")
    (description
     "A library for creating microservice applications")
    (license #f)))

(define-public python-stem
  (package
    (name "python-stem")
    (version "1.4.1b")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://pypi.python.org/packages/"
               "58/d1/aa1f9cd1f8b5a8a0def0eebd8e58df8d0b98f904a2403f3eedf92f47fbda"
               "/stem-" version ".tar.bz2"))
        (sha256
         (base32
          "09a3amp1y351nwz088ckiibbp666qi2lxwkyknavswfm400s0ns7"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _ (zero? (system* "python" "run_tests.py" "--unit")))))))
    (inputs
     `(("python-mock" ,python-mock)
       ("python-pep8" ,python-pep8)
       ("python-pyflakes" ,python-pyflakes)
       ("python-pycrypto" ,python-pycrypto)))
    (home-page "https://stem.torproject.org/")
    (synopsis "Controller library for Tor")
    (description
     "Stem is a Python controller library that allows applications to interact
with Tor (https://www.torproject.org/).  With it you can use Tor's control
protocol to script against the Tor process.")
    (license license:gpl3)))

(define-public python2-stem
  (package-with-python2 python-stem))
