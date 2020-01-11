;;; Copyright © 2018, 2019, 2020 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg contrib dropbox)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz))

(define-public dbxfs
  (package
    (name "dbxfs")
    (version "1.0.43")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "dbxfs" version))
        (sha256
         (base32
          "1f9sy2ax215dxiwszrrcadffjdsmrlxm4kwrbiap9dhxvzm226ks"))
        (patches (search-patches "dbxfs-remove-sentry-sdk.patch"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; tests requires safefs
    (propagated-inputs
     `(("python-appdirs" ,python-appdirs)
       ("python-block-tracing" ,python-block-tracing)
       ("python-dropbox" ,python-dropbox)
       ("python-keyring" ,python-keyring)
       ("python-keyrings.alt" ,python-keyrings.alt)
       ("python-privy" ,python-privy)
       ("python-userspacefs" ,python-userspacefs)))
  (home-page "https://github.com/rianhunter/dbxfs")
  (synopsis "User-space file system for Dropbox")
  (description
   "@code{dbxfs} allows you to mount your Dropbox folder as if it were a
local filesystem.")
  (license license:gpl3+)))

(define-public python-block-tracing
  (package
    (name "python-block-tracing")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "block_tracing" version))
        (sha256
         (base32
          "0s2y729qr5rs7n506qfh8cssk8m2bi6k2y5vbrh2z3raf2d01alz"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f)) ; no tests
    (home-page "https://github.com/rianhunter/block_tracing")
    (synopsis "Protect process memory")
    (description
     "@code{block_tracing} is a tiny Python library that can be used to
prevent debuggers and other applications from inspecting the memory within
your process.")
    (license license:expat)))

(define-public python-dropbox
  (package
    (name "python-dropbox")
    (version "9.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "dropbox" version))
        (sha256
         (base32
          "0qid094qna6bl4zpd08f6snvipwjls1yadacvmwri11djgp0wvj3"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f)) ; Tests require a network connection.
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-pytest-runner" ,python-pytest-runner)))
    (propagated-inputs
     `(("python-certifi" ,python-certifi)
       ("python-chardet" ,python-chardet)
       ("python-requests" ,python-requests)
       ("python-six" ,python-six)
       ("python-urllib3" ,python-urllib3)))
    (home-page "https://www.dropbox.com/developers")
    (synopsis "Official Dropbox API Client")
    (description "A Python SDK for integrating with the Dropbox API v2.")
    (license license:expat)))

(define-public python-privy
  (package
    (name "python-privy")
    (version "6.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               ;; Releases are untagged
               (url "https://github.com/ofek/privy")
               (commit "2838db3df239797c71bddacc48a4c49a83f35747")))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1m32dh5fqc8cy7jyf1z5fs6zvmdkbq5fi98hr609gbl7s0l0y0i9"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (invoke "python" "-m" "pytest"))))))
    (native-inputs
     `(("python-pytest" ,python-pytest)))
    (propagated-inputs
     `(("python-argon2-cffi" ,python-argon2-cffi)
       ("python-cryptography" ,python-cryptography)))
    (home-page "https://www.dropbox.com/developers")
    (synopsis "Library to password-protect your data")
    (description
     "Privy is a small and fast utility for password-protecting secret
data such as API keys, cryptocurrency wallets, or seeds for digital
signatures.")
    (license (list license:expat license:asl2.0)))) ; dual licensed

(define-public python-argon2-cffi
  (package
    (name "python-argon2-cffi")
    (version "19.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "argon2-cffi" version))
        (sha256
         (base32
          "18xxfw30gi3lwaz4vwb05iavzlrk3fa1x9fippzrgd3px8z65apz"))
        (modules '((guix build utils)))
        (snippet '(begin (delete-file-recursively "extras") #t))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (setenv "ARGON2_CFFI_USE_SYSTEM" "1")
             (invoke "python" "setup.py" "build")))
         (replace 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (invoke "pytest")
             (invoke "python" "-m" "argon2" "--help")
             ;; see tox.ini
             (invoke "python" "-m" "argon2" "-n" "1" "-t" "1" "-m" "8" "-p" "1"))))))
    (propagated-inputs
     `(("python-cffi" ,python-cffi)
       ("python-six" ,python-six)))
    (inputs `(("argon2" ,argon2)))
    (native-inputs
     `(("python-hypothesis" ,python-hypothesis)
       ("python-pytest" ,python-pytest)))
    (home-page "https://argon2-cffi.readthedocs.io/")
    (synopsis "Secure Password Hashes for Python")
    (description
     "Argon2 is a secure password hashing algorithm.  It is designed to have
both a configurable runtime as well as memory consumption.  This means that you
can decide how long it takes to hash a password and how much memory is required.")
    (license license:expat)))

(define-public python-userspacefs
  (package
    (name "python-userspacefs")
    (version "1.0.13")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "userspacefs" version))
        (sha256
         (base32
          "0kyz52jyxw3m7hqvn5g6z0sx9cq6k0nq1wj44lvdrghdljjgyk2z"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-fusepyng" ,python-fusepyng)))
    (home-page "https://github.com/rianhunter/userspacefs")
    (synopsis "User-space file systems for Python")
    (description
     "@code{userspacefs} is a library that allows you to easily write
user-space file systems in Python.")
    (license license:gpl3+)))

(define-public python-fusepyng
  (package
    (name "python-fusepyng")
    (version "1.0.7")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "fusepyng" version))
        (sha256
         (base32
          "17w9iw6m6zjbmnhs4ikd27pq4mb1nan6k4ahlwyz40463vw6wkwb"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-libfuse-path
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((fuse (assoc-ref inputs "fuse")))
               (substitute* "fusepyng.py"
                 (("os.environ.get\\('FUSE_LIBRARY_PATH'\\)")
                  (string-append "\"" fuse "/lib/libfuse.so\""))))
             #t)))))
    (inputs
     `(("fuse" ,fuse)))
    (propagated-inputs
     `(("python-paramiko" ,python-paramiko)))
    (home-page "https://github.com/rianhunter/fusepyng")
    (synopsis "Simple ctypes bindings for FUSE")
    (description "@code{fusepyng} is a Python module that provides a simple
interface to FUSE on various operating systems.  It's just one file and is
implemented using @code{ctypes}.")
    (license license:isc)))

(define-public python-keyrings.alt
  (package
    (name "python-keyrings.alt")
    (version "3.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "keyrings.alt" version))
        (sha256
         (base32
          "0gdjdqpq2hf770p6iwi891mil0vbsdhvy88x0v8b2w4y4b28lcli"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (delete-file "keyrings/alt/_win_crypto.py")
            ;; Rely on python-keyring>20:
            ;; https://github.com/jaraco/keyrings.alt/issues/33
            (substitute* '("keyrings/alt/tests/test_Gnome.py"
                           "keyrings/alt/tests/test_Google.py"
                           "keyrings/alt/tests/test_Windows.py"
                           "keyrings/alt/tests/test_file.py"
                           "keyrings/alt/tests/test_pyfs.py")
              (("keyring.tests.test_backend") "keyring.testing.backend")
              (("keyring.tests.util") "keyring.testing.util"))
            #t))))
    (build-system python-build-system)
    (native-inputs
     `(("python-keyring" ,python-keyring)
       ("python-pytest" ,python-pytest)
       ("python-setuptools-scm" ,python-setuptools-scm)))
    (home-page "https://github.com/jaraco/keyrings.alt")
    (synopsis "Alternate keyring implementations")
    (description "Keyrings in this package may have security risks or other
implications.  These backends were extracted from the main keyring project to
make them available for those who wish to employ them, but are discouraged for
general production use.  Include this module and use its backends at your own
risk.")
    (license license:expat)))
