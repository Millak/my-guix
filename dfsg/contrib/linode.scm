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

(define-module (dfsg contrib linode)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system python)
  ;#:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  ;#:use-module (gnu packages parallel)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  ;#:use-module (gnu packages tls)
  )

(define-public linode-cli
  (package
    (name "linode-cli")
    (version "3.2.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/linode/linode-cli")
               (commit version)
               ;; For bats submodules repositories.
               ;(recursive? #t)
               ))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1rl3yv0vkbzx95wxlsbygnj70akqpa9ak308f2r8n7f2f087bkkk"))))
          ;"15959hiya8fvn3x5nkd4vy3a01n2dk871j93s41dmixaa3npaqhr"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f  ; Tests expect a configuration file and network access.
       #:phases
       (modify-phases %standard-phases
         ;; Loading the module works, but also launches the CLI.
         (delete 'sanity-check)
         (add-after 'unpack 'patch-sources
           (lambda _
             (substitute* "setup.py"
               (("\\,\\\"enum34\\\"") ""))))
         (replace 'check
           (lambda* (#:key tests? outputs #:allow-other-keys)
             (if tests?
               (with-directory-excursion "test"
                 (setenv "PATH" (string-append (assoc-ref outputs "out")
                                               "/bin:" (getenv "PATH")))
                 (invoke "sh" "test-runner.sh" "--force"))))))))
    (inputs
     `(("python-colorclass" ,python-colorclass)
       ("python-pyyaml" ,python-pyyaml)
       ("python-requests" ,python-requests)
       ("python-terminaltables" ,python-terminaltables)))
    (native-inputs
     `(;; For the tests:
       ;("bats" ,bats)
       ;("openssl" ,openssl)
       ;("parallel" ,parallel)
       ))
    (home-page "https://github.com/linode/linode-cli")
    (synopsis "CLI for the Linode API")
    (description "")
    (license (list license:bsd-3
                   license:expat))))

(define-public python-colorclass
  (package
    (name "python-colorclass")
    (version "2.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/Robpol86/colorclass")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1c9r7v888wavaq8mhzihg42rlfynwzvgw95r5h7sjkqz2zd1pf4b"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; Color tests fail in test environment.
               (delete-file "tests/test_example.py")
               (invoke "py.test" "tests"))
             #t)))))
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (home-page "https://github.com/Robpol86/colorclass")
    (synopsis "Colorful worry-free console applications")
    (description
     "This package provides an ANSI color text library for Python.  It provides
\"auto colors\" for dark/light terminals.")
    (license license:expat)))
