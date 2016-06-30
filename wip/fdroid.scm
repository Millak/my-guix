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

(define-module (wip fdroid)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system python)
  #:use-module (gnu packages python))

(define-public fdroid
  (package
    (name "python-fdroidserver")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "fdroidserver" version))
        (sha256
         (base32
          "1fi4kkgi59gd9nssy9zc86b8rki3m47pd29vc821jgsa89lgjqf5"))))
    (build-system python-build-system)
    (inputs
     `(("python-mwclient" ,python-mwclient)
       ("python-paramiko" ,python-paramiko)
       ("python-pillow" ,python-pillow)
       ("python-pyasn1" ,python-pyasn1)
       ("python-pyasn1-modules" ,python-pyasn1-modules)
       ("python-pyyaml" ,python-pyyaml)
       ("python-requests" ,python-requests)))
    (home-page "https://f-droid.org")
    (synopsis "F-Droid Server Tools")
    (description "F-Droid Server Tools")
    (license license:agpl3+)))

(define-public python-mwclient
  (package
      (name "python-mwclient")
        (version "0.8.1")
        (source
          (origin
            (method url-fetch)
            (uri (pypi-uri "mwclient" version))
            (sha256
             (base32
              "1r322v6i6xps9xh861rbr4ggshydcgp8cycbdlmgy8qbrh8jg2az"))))
    (build-system python-build-system)
    (inputs
    `(("python-mock" ,python-mock)
      ("python-requests" ,python-requests)
      ("python-responses" ,python-responses)
      ("python-six" ,python-six)))
    (home-page "https://github.com/btongminh/mwclient")
    (synopsis "MediaWiki API client")
    (description "MediaWiki API client")
    (license license:expat)))
