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

(define-module (dfsg main vim)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages crates-io))

(define-public gvlc
  (let ((commit "456446e7fa898fed82fef0d0c2dd0adf9b4b3b89")
        (revision "1"))
    (package
      (name "gvlc")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/wafelack/gvlc")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "15y6kmlvrxncxv7p8y4yhd2w10ir9jsqnxrbli330jy85iqrlkgv"))))
      (build-system cargo-build-system)
      (arguments
       `(#:install-source? #f
         #:cargo-inputs
         (("rust-clap" ,rust-clap-2))))
      (home-page "https://github.com/wafelack/gvlc")
      (synopsis "Gentle Vim Lisp Compiler")
      (description "The Gentle Vim Lisp Compiler is a compiler for the Vim Lisp
programming language.")
      (license license:gpl3+))))
