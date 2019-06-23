;;; Copyright © 2016, 2018 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (wip express)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages enlightenment)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages pkg-config))

(define-public express
  (package
    (name "express")
    (version "20151226")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://git.enlightenment.org/apps/express.git")
               (commit "9e4500cb46c4d3d4eeb25cf36aaabfd1e7296eef")))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1gda16css0vbg40x1d8zjx655pm0ag7fds221147568z6mish1xa"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'setenv
           (lambda _
             (setenv "NOCONFIGURE" "TRUE")
             ;; FATAL: Cannot create run dir '/homeless-shelter/.run' - errno=2
             (setenv "HOME" "/tmp")
             #t)))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext" ,gettext-minimal)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("efl" ,efl)))
    (home-page "http://smhouston.us/express/")
    (synopsis "IRC client with enhanced media capabilities")
    (description "EFL-based IRC Client which operates similar to the
Terminology interface.")
    (license license:bsd-2)))
