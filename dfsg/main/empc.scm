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

(define-module (dfsg main empc)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages enlightenment)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages mpd)
  #:use-module (gnu packages pkg-config))

(define-public empc
  (package
    (name "empc")
    (version "20160617")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://git.enlightenment.org/apps/empc.git/")
               (commit "e8509d5935b79ef7387caeb4f7bc0377e265b366")))
        (file-name (string-append name "-" version "-checkout"))
        (sha256
         (base32
          "1svp67jrprqzrn3ifw6chpfn6cppzlym7ip8dnf7rwkmdq88hmi5"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'autoconf
           (lambda _ (zero? (system* "autoreconf" "-vfi")))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext" ,gnu-gettext)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("efl" ,efl)
       ("elementary" ,elementary)
       ;; esql?
       ;; glyr?
       ("libmpdclient" ,libmpdclient)))
    (home-page "https://www.enlightenment.org")
    (synopsis "Enlightenment powered mpd client")
    (description "The best fucking music player ever written")
    (license license:gpl3+)))
