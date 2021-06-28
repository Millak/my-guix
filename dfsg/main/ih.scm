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

(define-module (dfsg main ih)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages check)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz))

(define-public ih
  (package
    (name "ih")
    (version "0.6.0")
    (source
      (origin
        (method git-fetch)
        ;; Tests not included in pypi release.
        (uri (git-reference
               (url "https://github.com/glasnt/ih")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "16f472yjih9j4s53pvsd2gysj5sncfb47w0svw9km4pg9k0rckr0"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            ;; Remove non-free images
            (delete-file-recursively "demo")
            (delete-file "test/images/aurora.jpg")
            (substitute* "test/test_cli.py"
              ((".*aurora.jpg.*") "")
              (("runner.*TEST_JPG\\)") "")
              (("def test_term_render") "def skip_test_term_render"))
            #t))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "pytest" "-s"))
             #t)))))
    (inputs
     `(("python-click" ,python-click)
       ("python-pillow" ,python-pillow)
       ("python-scipy" ,python-scipy)
       ("python-tabulate" ,python-tabulate)))
    (native-inputs
     `(("python-beautifulsoup4" ,python-beautifulsoup4)
       ("python-pytest" ,python-pytest)))
    (home-page "https://github.com/glasnt/ih")
    (synopsis "Package for creating embroidery patterns")
    (description
     "@code{ih} is a Python command-line tool for generating cross-stitch
patterns from source images.")
    ;; The licenses issue can be fixed by removing demo/ and test/ and skipping tests.
    (license license:bsd-3)))
