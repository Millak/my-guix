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

(define-module (wip got)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages check))

(define-public got
  (package
    (name "got")
    (version "0.15")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://gameoftrees.org/releases/got-"
                            version ".tar.gz"))
        (sha256
         (base32
          "0zxyxnqwqlycn9wa5p125p11iisik3axvbvqr3pcwacmfhjlf34n"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; tests are run after installing
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'pre-build
           (lambda _
             (invoke "make" "obj")))
         (add-after 'install 'check
           (lambda _
             (invoke "make" "regress"))))))
    (home-page "https://gameoftrees.org/")
    (synopsis "Game of trees version control system")
    (description
     "Game of Trees (Got) is a version control system which prioritizes ease
of use and simplicity over flexibility.")
    (license license:isc)))
