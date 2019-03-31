;;; Copyright © 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  )

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
          "1f9sy2ax215dxiwszrrcadffjdsmrlxm4kwrbiap9dhxvzm226ks"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f)) ; tests requires safefs
    ;(inputs
    ; `(("safefs" ,safefs)))
    (propagated-inputs
     `(("python-appdirs" ,python-appdirs)
       ("python-block-tracing" ,python-block-tracing)
       ("python-dropbox" ,python-dropbox)
       ("python-entrypoints" ,python-entrypoints)
       ("python-keyrings.alt" ,python-keyrings.alt)
       ("python-privy" ,python-privy)
       ("python-secretstorage" ,python-secretstorage)
       ("python-sentry-sdk" ,python-sentry-sdk) ; for error reports
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
    (version "9.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "dropbox" version))
        (sha256
         (base32
          "1ckpbksdby70d70m58b904h8y8v7m82h12n3q3qk58r4yrqwvld5"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f)) ; Tests require a network connection.
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-pytest-runner" ,python-pytest-runner)))
    (propagated-inputs
     `(("python-certifi" ,python-certifi)
       ("python-chardet" ,python-chardet)
       ("python-requests" ,python-requests-2.21) ; >=2.16.2
       ("python-six" ,python-six)
       ("python-urllib3" ,python-urllib3)))
    (home-page "https://www.dropbox.com/developers")
    (synopsis "Official Dropbox API Client")
    (description "A Python SDK for integrating with the Dropbox API v2.")
    (license license:expat)))

(define python-requests-2.21
  (package
    (inherit python-requests)
    (name "python-requests")
    (version "2.21.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "requests" version))
        (sha256
         (base32
          "13jr0wkj9c2j8c0c8iaal9iivi0bpxghnsdn6lxcpnmc657q4ajh"))))))

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
    (version "19.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "argon2_cffi" version))
        (sha256
         (base32
          "09lbgwfhm5mm5slinzbwsrflbygl6c3kb2ljrd0111hrp4kqlm41"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'check)
         (replace 'build
           (lambda _
             (setenv "ARGON2_CFFI_USE_SYSTEM" "1")
             (invoke "python" "setup.py" "build")))
         (add-after 'install 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (invoke "python" "-m" "pytest")
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

(define-public python-sentry-sdk
  (package
    (name "python-sentry-sdk")
    (version "0.7.9")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "sentry-sdk" version))
        (sha256
         (base32
          "1s8pn21f8w7a1z8hdqws6wirwzsfgl1pk09ibnjsh86ccm7jrg1m"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f)) ; wow thats a lot of test requirements
    (propagated-inputs
     `(("python-certifi" ,python-certifi)
       ("python-flask" ,python-flask)
       ("python-urllib3" ,python-urllib3)))
    (home-page "https://github.com/getsentry/sentry-python")
    (synopsis "Python client for Sentry")
    (description
     "Sentry is a client/server architecture.  You integrate Sentry's SDK into
your application and it starts sending errors to Sentry's servers as they happen.")
    (license license:bsd-3)))

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
    (version "3.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "keyrings.alt" version))
        (sha256
         (base32
          "0lgp2d3hrpvbb2rfz18vrv5lrck72k3l2f2cpkbks2kigrfbgiqb"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (delete-file "tests/test_Windows.py")
            (delete-file "keyrings/alt/Windows.py")
            (delete-file "keyrings/alt/_win_crypto.py")
            #t))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-six" ,python-six)))
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

(define-public python-secretstorage
  (package
    (name "python-secretstorage")
    (version "3.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "SecretStorage" version))
        (sha256
         (base32
          "14lznnn916ddn6yrd3w2nr2zq49zc8hw53yjz1k9yhd492p9gir0"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; Tests require a running dbus service.
    (propagated-inputs
     `(("python-cryptography" ,python-cryptography)
       ("python-jeepney" ,python-jeepney)))
    (home-page "https://github.com/mitya57/secretstorage")
    (synopsis "Python bindings to FreeDesktop.org Secret Service API")
    (description
     "@code{python-secretstorage} provides a way for securely storing passwords
and other secrets.  It uses D-Bus Secret Service API that is supported by GNOME
Keyring (since version 2.30) and KSecretsService.  SecretStorage supports most
of the functions provided by Secret Service, including creating and deleting
items and collections, editing items, locking and unlocking collections
(asynchronous unlocking is also supported).")
    (license license:bsd-3)))

(define-public python-jeepney
  (package
    (name "python-jeepney")
    (version "0.4")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "jeepney" version))
        (sha256
         (base32
          "0w1w1rawl9k4lx91w16d19kbmf1349mhy8ph8x3w0qp1blm432b0"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-testpath" ,python-testpath)
       ("python-tornado" ,python-tornado)
       ("python-pytest" ,python-pytest)))
    (home-page "https://gitlab.com/takluyver/jeepney")
    (synopsis "Low-level, pure Python DBus protocol wrapper")
    (description
     "This is a low-level, pure Python DBus protocol client.  It has an
I/O-free core, and integration modules for different event loops.")
    (license license:expat)))
