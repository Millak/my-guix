;;; Copyright © 2019 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (wip mastodon)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages check)
  #:use-module (gnu packages time)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz))

(define-public fern
  (let ((commit "46067d64ffcc999ce8fe1a4feac76e45b3372438")
        (version "0.0.0")
        (revision "1"))
    (package
      (name "fern")
      (version (git-version version revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/enkiv2/fern.git")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "1f8afjdfd22dygh6mdyf2l69ghgdp45p16v2w3c12ishl460a455"))))
      (build-system python-build-system)
      (arguments
       `(#:tests? #f ; no tests
         #:phases
         (modify-phases %standard-phases
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (setenv "PREFIX" out)
                 (invoke "make" "install"))))
           (delete 'build))))
      (propagated-inputs
       `(("mastodon-py" ,python-mastodon-py)))
      (home-page "https://github.com/enkiv2/fern")
      (synopsis "Curses-based mastodon client")
      (description "Fern is a curses-based mastodon client modeled off usenet
news readers & pine, with an emphasis on getting to 'timeline zero'.")
      (license license:bsd-3))))

(define-public python-mastodon-py
  (package
    (name "python-mastodon-py")
    (version "1.4.6")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "Mastodon.py" version))
        (sha256
         (base32
          "1hvivnqvrhd9d3n3sbwx5bg9fsg1hvyqdnfy5zbhcm0nhya5a09b"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-dateutil" ,python-dateutil)
       ("python-decorator" ,python-decorator)
       ("python-magic" ,python-magic)
       ("python-pytz" ,python-pytz)
       ("python-requests" ,python-requests)
       ("python-six" ,python-six)))
    (native-inputs
     `(("python-blurhash" ,python-blurhash)
       ("python-cryptography" ,python-cryptography)
       ("python-http-ece" ,python-http-ece)
       ("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-pytest-mock" ,python-pytest-mock)
       ("python-pytest-runner" ,python-pytest-runner)
       ("python-pytest-vcr" ,python-pytest-vcr)
       ("python-requests-mock" ,python-requests-mock)
       ("python-vcrpy" ,python-vcrpy)))
    (home-page "https://github.com/halcy/Mastodon.py")
    (synopsis "Python wrapper for the Mastodon API")
    (description
     "Python wrapper for the Mastodon API")
    (license license:expat)))

(define-public python2-mastodon-py
  (package-with-python2 python-mastodon-py))

(define-public python-blurhash
  (package
    (name "python-blurhash")
    (version "1.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "blurhash" version))
        (sha256
         (base32
          "1kg50iy01c6igxsp6f762gj0naz2971c794g6ccx8qr92bl2xsz0"))))
    (build-system python-build-system)
    (arguments
     '(;#:phases
       ;(modify-phases %standard-phases
       ;  (replace 'check
       ;    (with-directory-excursion "tests"
       ;                              (invoke "python" "test_blurhash.py"))))
       #:tests? #f ; no tests in pypi release
       ))
    (native-inputs
     `(("python-numpy" ,python-numpy)
       ("python-pillow" ,python-pillow)
       ("python-pytest" ,python-pytest)))
    (home-page "https://github.com/halcy/blurhash-python")
    (synopsis
     "Pure-Python implementation of the blurhash algorithm.")
    (description
     "Pure-Python implementation of the blurhash algorithm.")
    (license license:expat)))

(define-public python2-blurhash
  (package-with-python2 python-blurhash))

(define-public python-http-ece
  (package
    (name "python-http-ece")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "http_ece" version))
        (sha256
         (base32
          "1y5ln09ji4dwpzhxr77cggk02kghq7lql60a6969a5n2lwpvqblk"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-cryptography" ,python-cryptography)))
    (native-inputs
     `(("python-coverage" ,python-coverage)
       ("python-flake8" ,python-flake8)
       ("python-mock" ,python-mock)
       ("python-nose" ,python-nose)))
    (home-page "https://github.com/martinthomson/encrypted-content-encoding")
    (synopsis "Encrypted Content Encoding for HTTP")
    (description
     "Encrypted Content Encoding for HTTP")
    (license license:expat)))

(define-public python2-http-ece
  (package-with-python2 python-http-ece))

(define-public python-pytest-vcr
  (package
    (name "python-pytest-vcr")
    (version "1.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pytest-vcr" version))
        (sha256
         (base32
          "15hq5vwiixhb5n2mdvbmxfn977zkwjm769r74vcl7k5vbavm3vi3"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pytest" ,python-pytest)
       ("python-vcrpy" ,python-vcrpy)))
    (home-page "https://github.com/ktosiek/pytest-vcr")
    (synopsis "Plugin for managing VCR.py cassettes")
    (description
     "Plugin for managing VCR.py cassettes")
    (license license:expat)))

(define-public python2-pytest-vcr
  (package-with-python2 python-pytest-vcr))
