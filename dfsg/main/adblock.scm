;;; Copyright © 2023 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages rust-apps))

(define-public python-adblock
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
                  "129q3wljhm12s9im9lvvs1n52sjbz21rkb38qhchd2nc66a0mjp5"))
                (snippet
                 #~(begin
                     (use-modules (guix build utils))
                     (substitute* "Cargo.toml"
                       (("\"=([[:digit:]]+(\\.[[:digit:]]+)*)" _ version)
                        (string-append "\"^" version))
                       ;; Only make the cdylib.
                       (("\"rlib\",") ""))))))
      (build-system cargo-build-system)
      (arguments
       (list
         #:imported-modules `(,@%cargo-build-system-modules
                              ,@%pyproject-build-system-modules)
         #:modules '((guix build cargo-build-system)
                     ((guix build pyproject-build-system) #:prefix py:)
                     (guix build utils))
         #:install-source? #f
         #:cargo-inputs
         `(("rust-adblock" ,rust-adblock-0.5)
           ("rust-pyo3" ,rust-pyo3-0.16))
         #:phases
         #~(modify-phases %standard-phases
             (add-after 'build 'build-wheel
               (lambda _
                 (invoke "maturin" "build" "--release" "--out" "dist/")))
             (replace 'install
               (assoc-ref py:%standard-phases 'install))
             ;; Move 'check after 'install like with the pyproject-build-system.
             (add-after 'install 'check
               (lambda* (#:key inputs outputs tests? #:allow-other-keys)
                 (py:add-installed-pythonpath inputs outputs)
                 (when tests?
                   (invoke "pytest" "-vv" "tests" "--color=yes")))))))
      (native-inputs
       (list maturin
             python-pytest
             python-toml
             python-wrapper))
      (home-page "https://github.com/ArniDagur/python-adblock")
      (synopsis "Adblock library in Python")
      (description
       "Python wrapper for Brave's adblocking library, which is written in Rust.")
      (license (list license:expat license:asl2.0)))))
