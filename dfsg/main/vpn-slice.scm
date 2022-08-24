;;; Copyright © 2022 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main vpn-slice)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix build-system python)
  #:use-module (gnu packages python-xyz))

(define-public vpn-slice
  (package
    (name "vpn-slice")
    (version "0.16.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "vpn-slice" version))
              (sha256
               (base32 "1anfx4hn2ggm6sbwqmqx68s3l2rjcy4z4l038xqb440jnk8jvl18"))))
    (build-system python-build-system)
    (arguments
     (list #:tests? #f))    ; No tests.
    (inputs
     (list python-dnspython python-setproctitle))
    (home-page "https://github.com/dlenski/vpn-slice")
    (synopsis "@code{vpnc-script} replacement for split-tunnel VPN setup")
    (description "This package provides a @code{vpnc-script} replacement for
split-tunnel VPN setup.")
    (license license:gpl3+)))
