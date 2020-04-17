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

(define-module (wip dianara)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages file)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages qt))

(define-public dianara
  (package
    (name "dianara")
    (version "1.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://savannah/dianara/dianara-v" version ".tar.gz"))
        (sha256
         (base32
          "1flkrcm26rwsl3mdkb484sihipv9swnlwvwgd50skiarczc466z6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (invoke "qmake"
                       (string-append "PREFIX=" out))))))))
    (inputs
     `(("file" ,file) ; libmagic
       ("qca" ,qca)
       ("qoauth" ,qoauth)
       ("qtbase" ,qtbase)))
    (home-page "https://jancoding.wordpress.com/dianara/")
    (synopsis "Client for the pump.io federated social network")
    (description
     "Dianara is a pump.io client, a desktop application for GNU/linux that
allows users to manage their Pump.io social networking accounts without the
need to use a web browser.")
    (license license:gpl2+)))
