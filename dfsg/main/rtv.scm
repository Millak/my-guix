;;; Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main rtv)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages web))

(define-public rtv
  (package
    (name "rtv")
    (version "1.24.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "rtv" version))
        (sha256
         (base32
          "0g8klxazbnmqcpcnw8crs4wy4qckqy7am2yy7jwr9xacx02rk16k"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-home-dir
           (lambda _ (setenv "HOME" (getcwd)) #t)))
       #:tests? #f)) ; tests fail: _curses.error: setupterm: could not find terminal
    (propagated-inputs
     `(("python-beautifulsoup4" ,python-beautifulsoup4)
       ("python-decorator" ,python-decorator)
       ("python-kitchen" ,python-kitchen)
       ("python-requests" ,python-requests)
       ("python-six" ,python-six)))
    (native-inputs
     `(("python-coveralls" ,python-coveralls)
       ("python-coverage" ,python-coverage)
       ("python-mock" ,python-mock)
       ("python-pylint" ,python-pylint)
       ("python-pytest" ,python-pytest)
       ("python-vcrpy" ,python-vcrpy)))
    (home-page "https://github.com/michael-lazar/rtv")
    (synopsis
     "Terminal viewer for Reddit (Reddit Terminal Viewer)")
    (description
     "A simple terminal viewer for Reddit (Reddit Terminal Viewer)")
    (license license:expat)))

(define python-vcrpy
  (package
    (name "python-vcrpy")
    (version "1.13.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "vcrpy" version))
        (sha256
         (base32
          "00dwcv6yg5fcy1g6jn7zmd61j12fbh9yr8ml5mnmifbhib3zjcbh"))))
    (build-system python-build-system)
    (arguments `(#:tests? #f)) ; tests require more packages for python-pytest-httpbin
    (propagated-inputs
     `(("python-pyyaml" ,python-pyyaml)
       ("python-six" ,python-six)
       ("python-wrapt" ,python-wrapt)
       ("python-yarl" ,python-yarl)))
    (native-inputs
     `(("python-mock" ,python-mock)
       ("python-pytest" ,python-pytest)
       ("python-pytest-httpbin" ,python-pytest-httpbin)))
    (home-page "https://github.com/kevin1024/vcrpy")
    (synopsis
      "Automatically mock your HTTP interactions to simplify and speed up testing")
    (description
      "Automatically mock your HTTP interactions to simplify and speed up testing")
    (license license:expat)))

(define python-coveralls
  (package
    (name "python-coveralls")
    (version "1.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "coveralls" version))
        (sha256
         (base32
          "08mcbzq3gz5247li96iwi6df3j2i51v4g0kp5g2kcyy1ivkngvlx"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f)) ; Tests require git repo and network connectivity.
    (propagated-inputs
     `(("python-docopt" ,python-docopt)
       ("python-coverage" ,python-coverage)
       ("python-requests" ,python-requests)))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-pytest-runner" ,python-pytest-runner)
       ("python-mock" ,python-mock)
       ("python-sh" ,python-sh)
       ("python-pyyaml" ,python-pyyaml)))
    (home-page
      "http://github.com/coveralls-clients/coveralls-python")
    (synopsis
      "Show coverage stats online via coveralls.io")
    (description
      "Show coverage stats online via coveralls.io")
    (license license:expat)))
