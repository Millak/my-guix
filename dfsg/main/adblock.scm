;;; Copyright © 2023, 2025 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main adblock)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system pyproject)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages rust-apps)
  #:use-module (dfsg main rust-crates))

(define-public python-adblock
  ;; A few commits after the 0.6.0 release to fix build issues.
  (let ((commit "a340dfcb37b402b0427b2dd7ac3c64cfe7edb38b")
        (revision "1"))
    (package
      (name "python-adblock")
      (version (git-version "0.6.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/ArniDagur/python-adblock")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "129q3wljhm12s9im9lvvs1n52sjbz21rkb38qhchd2nc66a0mjp5"))))
      (build-system pyproject-build-system)
      (arguments
       (list
         #:imported-modules `(,@%cargo-build-system-modules
                              ,@%pyproject-build-system-modules)
         #:modules '(((guix build cargo-build-system) #:prefix cargo:)
                     (guix build pyproject-build-system)
                     (guix build utils))
         #:phases
         #~(modify-phases %standard-phases
             (add-after 'ensure-no-cythonized-files 'prepare-cargo-build-system
               (lambda args
                 (for-each
                   (lambda (phase)
                     (format #t "Running cargo phase: ~a~%" phase)
                     (apply (assoc-ref cargo:%standard-phases phase)
                            #:vendor-dir "vendor"
                            args))
                   '(unpack-rust-crates
                     configure
                     check-for-pregenerated-files
                     patch-cargo-checksums)))))))
      (native-inputs
       (list maturin
             python-pytest
             python-toml
             python-wrapper
             rust
             `(,rust "cargo")))
      (inputs
       (cargo-inputs 'python-adblock #:module '(dfsg main rust-crates)))
      (home-page "https://github.com/ArniDagur/python-adblock")
      (synopsis "Adblock library in Python")
      (description
       "Python wrapper for Brave's adblocking library, which is written in Rust.")
      (license (list license:expat license:asl2.0)))))
