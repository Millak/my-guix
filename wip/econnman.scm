;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;;
;;; This file is an addendum to GNU Guix.
;;;
;;; GNU Guix is free software; you can reconnmanstribute it and/or modify it
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

(define-module (wip econnman)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages enlightenment)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python))

(define-public econnman
  (package
    (name "econnman")
    (version "1.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://download.enlightenment.org/rel/apps/"
                            "econnman/econnman-" version ".tar.gz"))
        (sha256
         (base32
          "057pwwavlvrrq26bncqnfrf449zzaim0zq717xv86av4n940gwv0"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    ;; needs to be wrapped
    (inputs
     `(("efl" ,efl)
       ("python-2" ,python-2)
       ("python2-efl" ,python2-efl)))
    (home-page "https://www.enlightenment.org")
    (synopsis "ConnMan User Interface written using the EFL")
    (description "ConnMan User Interface written using the EFL.")
    (license license:lgpl3)))
