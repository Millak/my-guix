;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main pifs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config))

(define-public pifs
  (let ((commit "3d9cc9db6a01234833c3d77c0846e20485763639")
        (revision "1"))
    (package
      (name "pifs")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/philipl/pifs")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "15xkq9xqpsmanm57xs0vvpl9c5v2g63ywp4gd8l6lkz4gsw3ailp"))))
      (build-system gnu-build-system)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (replace 'bootstrap
             (lambda _
               (invoke "autoreconf" "-vfi"))))))
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("pkg-config" ,pkg-config)))
      (inputs
       `(("fuse" ,fuse)))
      (home-page "https://github.com/philipl/pifs/")
      (synopsis "πfs - the data-free filesystem")
      (description "πfs is a revolutionary new file system that, instead of
wasting space storing your data on your hard drive, stores your data in π!
You'll never run out of space again - π holds every file that could possibly
exist!  They said 100% compression was impossible?  You're looking at it!")
      (license license:gpl3+))))
