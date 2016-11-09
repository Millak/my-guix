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

(define-module (wip quassel-irssi)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages enlightenment)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages pkg-config))

(define-public quassel-irssi
  (let ((commit "cbd9bd7f4ac44260d9fcafb809fdf153cde193e5")
        (revision "1"))
    (package
      (name "quassel-irssi")
      (version (string-append "0.0.0-" revision "." (string-take commit 7)))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/phhusson/quassel-irssi.git")
                 (commit commit)
                 (recursive? #t)))
          (file-name (string-append name "-" version "-checkout"))
          (sha256
           (base32
            "1gda16css0vbg40x1d8zjx655pm0ag7fds221147568z6mish1xa"))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'autoconf
             (lambda _ (zero? (and (setenv "NOCONFIGURE" "TRUE")
                                   (system* "sh" "autogen.sh"))))))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("gettext" ,gnu-gettext)
         ("libtool" ,libtool)
         ("pkg-config" ,pkg-config)))
      (inputs
       `(("efl" ,efl)))
      (home-page "http://smhouston.us/quassel-irssi/")
      (synopsis "IRC client with enhanced media capabilities")
      (description "EFL-based IRC Client which operates similar to the
Terminology interface.")
      (license license:bsd-2))))
