;;; Copyright © 2023 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main mptcp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config))

(define-public mptcpd
  (package
    (name "mptcpd")
    (version "0.12")
    (source (origin
              (method url-fetch)
              (uri (string-append
                     "https://github.com/multipath-tcp/mptcpd/releases/download/v"
                     version "/mptcpd-" version ".tar.gz"))
              (sha256
               (base32
                "1ghwhgnjp3hah0cndjs1v7cgr50qnj04cpykihb3lxrv72bx81q5"))))
    (build-system gnu-build-system)
    (inputs
     (list ell))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.mptcp.dev/")
    (synopsis "Multipath TCP Daemon")
    (description "The Multipath TCP Daemon is a daemon for Linux based operating
systems that performs Multipath TCP path management related operations in the
user space.  It interacts with the Linux kernel through a generic netlink
connection to track per-connection information (e.g. available remote addresses),
available network interfaces, request new MPTCP subflows, handle requests for
subflows, etc.")
    (license license:bsd-3)))
