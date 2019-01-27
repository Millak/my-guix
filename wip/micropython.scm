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

(define-module (wip micropython)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python))

(define-public micropython
  (package
    (name "micropython")
    (version "1.10")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/micropython/micropython/"
                            "releases/download/v" version
                            "/micropython-" version ".tar.gz"))
        (sha256
         (base32
          "1g1zjip3rkx6bp16qi1bag72wivnbh56fcsl3nffanrx4j5f4z90"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'preprare-build
           (lambda _
             (chdir "ports/unix")
             ;; see: https://github.com/micropython/micropython/pull/4246
             (substitute* "Makefile"
               (("-Os") "-Os -ffp-contract=off"))
             #t))
         (delete 'configure)) ; no configure
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
                          "V=1")
       #:test-target "test"))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("python" ,python-minimal-wrapper)))
    (inputs
     `(("libffi" ,libffi)))
    (home-page "https://micropython.org/")
    (synopsis "Python implementation for microcontrollers and constrained systems")
    (description "MicroPython is a lean and efficient implementation of the
Python 3 programming language that includes a small subset of the Python
standard library and is optimised to run on microcontrollers and in constrained
environments.  MicroPython is packed full of advanced features such as an
interactive prompt, arbitrary precision integers, closures, list comprehension,
generators, exception handling and more.  Still it is compact enough to fit and
run within just 256k of code space and 16k of RAM.  MicroPython aims to be as
compatible with normal Python as possible to allow you to transfer code with
ease from the desktop to a microcontroller or embedded system.")
    (license license:expat)))
