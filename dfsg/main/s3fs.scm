;;; Copyright © 2021 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main s3fs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages wget)
  #:use-module (gnu packages xml))

(define-public s3fs
  (package
    (name "s3fs")
    (version "1.90")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/s3fs-fuse/s3fs-fuse")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1iyjn7w4nrrii24icmp0zfk2bjr9d7byc42ks578fz0gv4shijmm"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f          ; Tests require /dev/fuse
       #:configure-flags '("--with-nettle"
                           "--with-gnutls")))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2)
       ("wget" ,wget)))
    (inputs
     `(("curl" ,curl)
       ("fuse" ,fuse-3)
       ("gnutls" ,gnutls)
       ("libgcrypt" ,libgcrypt)
       ("libxml2" ,libxml2)
       ("nettle" ,nettle)))
    (home-page "https://github.com/s3fs-fuse/s3fs-fuse")
    (synopsis "FUSE-based file system backed by Amazon S3")
    (description "@code{s3fs} allows one to mount an S3 bucket via FUSE.
@code{s3fs} preserves the native object format for files, allowing use of other
tools like AWS CLI.")
    (license license:gpl2+)))
