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

(define-module (wip edone)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system python)
  #:use-module (gnu packages enlightenment)
  #:use-module (gnu packages freedesktop))

(define-public edone
  (let ((commit "551d0a3a24863236da5f6a55ec4d355066974b47")
        (revision "1"))
  (package
    (name "edone")
    (version (git-version "0.0.0" revision commit))
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/DaveMDS/edone")
               (commit commit)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "167bps7n8m9fig6z1mgp5k7f82gb0kvjv3g1qhm6ssxp8x8y5sjg"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-home-dir
           (lambda _ (setenv "HOME" (getenv "TMPDIR")) #t)))))
    (propagated-inputs
     `(("python-efl" ,python-efl)
       ("python-pyxdg" ,python-pyxdg)))
    (home-page "https://github.com/DaveMDS/edone")
    (synopsis "GTD apptlication written in efl")
    (description "Edone is fully compliant with the Todo.txt specifications,
this basically means that your todo items are stored in a simple and readable
text file and that you can use any other compliant client to read/edit your
tasks.")
    (license license:gpl3+))))
