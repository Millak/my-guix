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

(define-module (dfsg main signify)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system go)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages golang))

(define-public go-minisign
  (package
    (name "go-minisign")
    (version "0.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jedisct1/go-minisign")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0wc0rk5m60yz52f0cncmbgq67yvb1rcx91gvzjg6jpc4mpw2db27"))
        (modules '((guix build utils)))
        (snippet
         '(begin (delete-file-recursively "vendor") #t))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/jedisct1/go-minisign"))
    (propagated-inputs
     `(("go-golang-org-x-crypto" ,go-golang-org-x-crypto)))
    (home-page "https://github.com/jedisct1/go-minisign")
    (synopsis "Minisign verification library for Golang")
    (description "A Golang library to verify Minisign signatures.")
    (license license:expat)))

(define-public rust-minisign
  (package
    (name "rust-minisign")
    (version "0.5.19")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "minisign" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0z3amdkbdlzcvkpzanl945h8baw5wdh7j789n2mnbq8npfj1xrni"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-getrandom" ,rust-getrandom-0.1)
        ("rust-rpassword" ,rust-rpassword-4)
        ("rust-scrypt" ,rust-scrypt-0.2))))
    (home-page "https://github.com/jedisct1/rust-minisign")
    (synopsis "Crate to sign files and verify signatures")
    (description
     "This package provides a crate to sign files and verify signatures.")
    (license license:expat)))

(define-public rust-scrypt-0.2
  (package
    (name "rust-scrypt")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "scrypt" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1pfgqgzdjxjf7c8r1wfka0ackfpv1g8w7wvbr25b42hdx787jv35"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-base64" ,rust-base64-0.9)
        ("rust-byte-tools" ,rust-byte-tools-0.3)
        ("rust-byteorder" ,rust-byteorder-1.3)
        ("rust-hmac" ,rust-hmac-0.7)
        ("rust-pbkdf2" ,rust-pbkdf2-0.3)
        ("rust-rand" ,rust-rand-0.5)
        ("rust-sha2" ,rust-sha2-0.8)
        ("rust-subtle" ,rust-subtle-1.0))))
    (home-page "https://github.com/RustCrypto/password-hashing")
    (synopsis "Scrypt password-based key derivation function")
    (description
     "Scrypt password-based key derivation function.")
    (license (list license:expat license:asl2.0))))

(define-public rust-hmac-0.7
  (package
    (name "rust-hmac")
    (version "0.7.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hmac" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "15cnwpssp2n1kdm9x7abir67f2hp3q6rdfj1mcck3hm4rmj5xjsx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-crypto-mac" ,rust-crypto-mac-0.7)
        ("rust-digest" ,rust-digest-0.8))
       #:cargo-development-inputs
       (("rust-crypto-mac" ,rust-crypto-mac-0.7)
        ("rust-md-5" ,rust-md-5-0.8)
        ("rust-sha2" ,rust-sha2-0.8))))
    (home-page "https://github.com/RustCrypto/MACs")
    (synopsis
     "Generic implementation of Hash-based Message Authentication Code (HMAC)")
    (description
     "Generic implementation of Hash-based Message Authentication Code (HMAC).")
    (license (list license:expat license:asl2.0))))

(define-public rust-md-5-0.8
  (package
    (name "rust-md-5")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "md-5" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1j5rfxy2p76xf5f1lgaw85xla0b1bbv2lknvdhv1j0ibmzfg72m1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-block-buffer" ,rust-block-buffer-0.7)
        ("rust-digest" ,rust-digest-0.8)
        ("rust-md5-asm" ,rust-md5-asm-0.4)
        ("rust-opaque-debug" ,rust-opaque-debug-0.2))
       #:cargo-development-inputs
       (("rust-digest" ,rust-digest-0.8)
        ("rust-hex-literal" ,rust-hex-literal-0.1))))
    (home-page "https://github.com/RustCrypto/hashes")
    (synopsis "MD5 hash function")
    (description "MD5 hash function.")
    (license (list license:expat license:asl2.0))))

(define-public rust-pbkdf2-0.3
  (package
    (name "rust-pbkdf2")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pbkdf2" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1na2fmmfcmksz4xk7m0ihl778501c1krx88dcylrand48f506v00"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-base64" ,rust-base64-0.9)
        ("rust-byteorder" ,rust-byteorder-1.3)
        ("rust-crypto-mac" ,rust-crypto-mac-0.7)
        ("rust-hmac" ,rust-hmac-0.7)
        ("rust-rand" ,rust-rand-0.5)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-sha2" ,rust-sha2-0.8)
        ("rust-subtle" ,rust-subtle-1.0))
       #:cargo-development-inputs
       (("rust-hmac" ,rust-hmac-0.7)
        ("rust-sha-1" ,rust-sha-1-0.8)
        ("rust-sha2" ,rust-sha2-0.8))))
    (home-page "https://github.com/RustCrypto/password-hashing")
    (synopsis "Generic implementation of PBKDF2")
    (description "Generic implementation of PBKDF2.")
    (license (list license:expat license:asl2.0))))

(define-public rust-md5-asm-0.4
  (package
    (name "rust-md5-asm")
    (version "0.4.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "md5-asm" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0gpk5647js1k084jc7pg2gji0cvl6hjkkbfia6lnpk8y4shyairv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cc" ,rust-cc-1.0))))
    (home-page "https://github.com/RustCrypto/asm-hashes")
    (synopsis "Assembly implementation of MD5 compression function")
    (description
     "Assembly implementation of MD5 compression function.")
    (supported-systems '("x86_64-linux" "i686-linux"))
    (license license:expat)))
