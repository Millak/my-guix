;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main ecrire)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages enlightenment)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages pkg-config))

(define-public ecrire
  (let ((commit "5739b04db6f9ac076cbfa43f60504ef0722816f2")
        (revision "2"))
    (package
      (name "ecrire")
      (version (string-append "0.0.0-" revision "." (string-take commit 7)))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://git.enlightenment.org/apps/ecrire.git")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "1mzk2zjy81js0hyx491cfmk81z36gk8m385s3r4xng8r9pgbx6lr"))))
      (build-system cmake-build-system)
      (arguments '(#:tests? #f)) ; no tests
      (native-inputs
       `(("gettext" ,gettext-minimal)
         ("pkg-config" ,pkg-config)))
      (inputs
       `(("efl" ,efl)))
      (home-page "https://www.enlightenment.org")
      (synopsis "EFL simple text editor")
      (description "EFL simple text editor")
      (license license:gpl3+))))
