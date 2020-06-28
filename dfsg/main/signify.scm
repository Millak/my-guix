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
    (version "0.5.20")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "minisign" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0xmcvh2snravghaar8igc6b9r3s1snnmf9qam9l3zyhm4987767y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-getrandom" ,rust-getrandom-0.1)
        ("rust-rpassword" ,rust-rpassword-4)
        ("rust-scrypt" ,rust-scrypt-0.3))))
    (home-page "https://github.com/jedisct1/rust-minisign")
    (synopsis "Crate to sign files and verify signatures")
    (description
     "This package provides a crate to sign files and verify signatures.")
    (license license:expat)))

(define-public rust-scrypt-0.3
  (package
    (name "rust-scrypt")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "scrypt" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1apicbvp7cgc1z2nl5l48g8h3kp7p592r4zbkx9vsri2ivnvgv43"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-base64" ,rust-base64-0.12)
        ("rust-hmac" ,rust-hmac-0.8)
        ("rust-pbkdf2" ,rust-pbkdf2-0.4)
        ("rust-rand" ,rust-rand-0.7)
        ("rust-rand-core" ,rust-rand-core-0.5)
        ("rust-sha2" ,rust-sha2-0.9)
        ("rust-subtle" ,rust-subtle-2))))
    (home-page
      "https://github.com/RustCrypto/password-hashing")
    (synopsis
      "Scrypt password-based key derivation function")
    (description
      "Scrypt password-based key derivation function")
    (license (list license:expat license:asl2.0))))

(define-public rust-hmac-0.8
  (package
    (name "rust-hmac")
    (version "0.8.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "hmac" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0h48wc7iysh4xd6ci4prh8bb7nszijrh9w3blaaq8a6cilk8hs0j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-crypto-mac" ,rust-crypto-mac-0.8)
        ("rust-digest" ,rust-digest-0.9))
       #:cargo-development-inputs
       (("rust-crypto-mac" ,rust-crypto-mac-0.8)
        ("rust-md-5" ,rust-md-5-0.9)
        ("rust-sha2" ,rust-sha2-0.9))))
    (home-page "https://github.com/RustCrypto/MACs")
    (synopsis
      "Generic implementation of Hash-based Message Authentication Code (HMAC)")
    (description
      "Generic implementation of Hash-based Message Authentication Code (HMAC)")
    (license (list license:expat license:asl2.0))))

(define-public rust-crypto-mac-0.8
  (package
    (name "rust-crypto-mac")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "crypto-mac" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1axfs4zmy74rn9666p92j7nmcv11zdp2d51yrppc2dv26cqa715m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-blobby" ,rust-blobby-0.1)
        ("rust-generic-array" ,rust-generic-array-0.14)
        ("rust-subtle" ,rust-subtle-2))))
    (home-page
      "https://github.com/RustCrypto/traits")
    (synopsis
      "Trait for Message Authentication Code (MAC) algorithms")
    (description
      "Trait for Message Authentication Code (MAC) algorithms")
    (license (list license:expat license:asl2.0))))

(define-public rust-generic-array-0.14
  (package
    (name "rust-generic-array")
    (version "0.14.2")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "generic-array" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "107r1fpm8zcab3lzci4x9par6ik8bra390c60rhxvnmz7dgnlx5c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-serde" ,rust-serde-1.0)
        ("rust-typenum" ,rust-typenum-1.10)
        ("rust-version-check" ,rust-version-check-0.9))
       #:cargo-development-inputs
       (("rust-bincode" ,rust-bincode-1.2)
        ("rust-serde-json" ,rust-serde-json-1.0))))
    (home-page
      "https://github.com/fizyk20/generic-array.git")
    (synopsis
      "Generic types implementing functionality of arrays")
    (description
      "Generic types implementing functionality of arrays")
    (license license:expat)))

(define-public rust-subtle-2
  (package
    (name "rust-subtle")
    (version "2.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "subtle" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1h9jd7v0imksyl5mvnjk2rw54sa3xrril76z0md61mq2gh056bah"))))
    (build-system cargo-build-system)
    (home-page "https://dalek.rs/")
    (synopsis
      "Pure-Rust traits and utilities for constant-time cryptographic implementations.")
    (description
      "Pure-Rust traits and utilities for constant-time cryptographic implementations.")
    (license license:bsd-3)))

(define-public rust-digest-0.9
  (package
    (name "rust-digest")
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "digest" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0rmhvk33rgvd6ll71z8sng91a52rw14p0drjn1da0mqa138n1pfk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-blobby" ,rust-blobby-0.1)
        ("rust-generic-array" ,rust-generic-array-0.14))))
    (home-page
      "https://github.com/RustCrypto/traits")
    (synopsis
      "Traits for cryptographic hash functions")
    (description
      "Traits for cryptographic hash functions")
    (license (list license:expat license:asl2.0))))

(define-public rust-md-5-0.9
  (package
    (name "rust-md-5")
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "md-5" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "14x7yxfi4pk4qy3zmn9dj69yc18fg3cyind346kribjd93077qij"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-block-buffer" ,rust-block-buffer-0.8)
        ("rust-digest" ,rust-digest-0.9)
        ("rust-md5-asm" ,rust-md5-asm-0.4)
        ("rust-opaque-debug" ,rust-opaque-debug-0.2))
       #:cargo-development-inputs
       (("rust-digest" ,rust-digest-0.9)
        ("rust-hex-literal" ,rust-hex-literal-0.2))))
    (home-page
      "https://github.com/RustCrypto/hashes")
    (synopsis "MD5 hash function")
    (description "MD5 hash function")
    (license (list license:expat license:asl2.0))))

(define-public rust-block-buffer-0.8
  (package
    (name "rust-block-buffer")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "block-buffer" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0c9x5b8pk25i13bajqjkzf03bm5hx2y8pi9llfvjpy3nhr295kyv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-block-padding" ,rust-block-padding-0.1)
        ("rust-byte-tools" ,rust-byte-tools-0.3)
        ("rust-byteorder" ,rust-byteorder-1.3)
        ("rust-generic-array" ,rust-generic-array-0.14))))
    (home-page "https://github.com/RustCrypto/utils")
    (synopsis
      "Fixed size buffer for block processing of data")
    (description
      "Fixed size buffer for block processing of data")
    (license (list license:expat license:asl2.0))))

(define-public rust-sha2-0.9
  (package
    (name "rust-sha2")
    (version "0.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sha2" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1hdqrx2d9073hgf34y6ilgw6ni5vv3d5nmccyhkfm9zdvy6kfcr9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-block-buffer" ,rust-block-buffer-0.9)
        ("rust-cfg-if" ,rust-cfg-if-0.1)
        ("rust-cpuid-bool" ,rust-cpuid-bool-0.1)
        ("rust-digest" ,rust-digest-0.9)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-opaque-debug" ,rust-opaque-debug-0.3)
        ("rust-sha2-asm" ,rust-sha2-asm-0.5))
       #:cargo-development-inputs
       (("rust-digest" ,rust-digest-0.9)
        ("rust-hex-literal" ,rust-hex-literal-0.2))))
    (home-page
      "https://github.com/RustCrypto/hashes")
    (synopsis
      "Pure Rust implementation of the SHA-2 hash function family
      including SHA-224, SHA-256, SHA-384, and SHA-512.
      ")
      (description
        "Pure Rust implementation of the SHA-2 hash function family
        including SHA-224, SHA-256, SHA-384, and SHA-512.
        ")
        (license (list license:expat license:asl2.0))))

(define-public rust-block-buffer-0.9
  (package
    (name "rust-block-buffer")
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "block-buffer" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1r4pf90s7d7lj1wdjhlnqa26vvbm6pnc33z138lxpnp9srpi2lj1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-block-padding" ,rust-block-padding-0.2)
        ("rust-generic-array" ,rust-generic-array-0.14))))
    (home-page "https://github.com/RustCrypto/utils")
    (synopsis
      "Fixed size buffer for block processing of data")
    (description
      "Fixed size buffer for block processing of data")
    (license (list license:expat license:asl2.0))))

(define-public rust-block-padding-0.2
  (package
    (name "rust-block-padding")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "block-padding" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0x6b2dgink7rc3755r8jl4kmndydy5563h3wz7z9jqrb25ygv2y9"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/RustCrypto/utils")
    (synopsis
      "Padding and unpadding of messages divided into blocks.")
    (description
      "Padding and unpadding of messages divided into blocks.")
    (license (list license:expat license:asl2.0))))

(define-public rust-cpuid-bool-0.1
  (package
    (name "rust-cpuid-bool")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "cpuid-bool" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1r3v22cxly1shvw8qi0153708kggdqvh8jp0g82wbxi06d1mqdvd"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/RustCrypto/utils")
    (synopsis
      "A lightweight no-std compatible alternative to is_x86_feature_detected")
    (description
      "This package provides a lightweight no-std compatible alternative to is_x86_feature_detected")
    (license (list license:expat license:asl2.0))))

(define-public rust-opaque-debug-0.3
  (package
    (name "rust-opaque-debug")
    (version "0.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "opaque-debug" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1m8kzi4nd6shdqimn0mgb24f0hxslhnqd1whakyq06wcqd086jk2"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/RustCrypto/utils")
    (synopsis
      "Macro for opaque Debug trait implementation")
    (description
      "Macro for opaque Debug trait implementation")
    (license (list license:expat license:asl2.0))))

(define-public rust-pbkdf2-0.4
  (package
    (name "rust-pbkdf2")
    (version "0.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "pbkdf2" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1g8cm3nwrsydazjc1gjs549hzafgxq8qb49gixrhl3qrd9calvi1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-base64" ,rust-base64-0.12)
        ("rust-crypto-mac" ,rust-crypto-mac-0.8)
        ("rust-hmac" ,rust-hmac-0.8)
        ("rust-rand" ,rust-rand-0.7)
        ("rust-rand-core" ,rust-rand-core-0.5)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-sha2" ,rust-sha2-0.9)
        ("rust-subtle" ,rust-subtle-2))
       #:cargo-development-inputs
       (("rust-hmac" ,rust-hmac-0.8)
        ("rust-sha-1" ,rust-sha-1-0.9)
        ("rust-sha2" ,rust-sha2-0.9))))
    (home-page
      "https://github.com/RustCrypto/password-hashing")
    (synopsis "Generic implementation of PBKDF2")
    (description "Generic implementation of PBKDF2")
    (license (list license:expat license:asl2.0))))

(define-public rust-sha-1-0.9
  (package
    (name "rust-sha-1")
    (version "0.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (crate-uri "sha-1" version))
        (file-name
         (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0w37j7swjkbzgi9mf7ihkw0zfik6vl97fs6jdpqs6r68hvm3c2hp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-block-buffer" ,rust-block-buffer-0.9)
        ("rust-cfg-if" ,rust-cfg-if-0.1)
        ("rust-cpuid-bool" ,rust-cpuid-bool-0.1)
        ("rust-digest" ,rust-digest-0.9)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-opaque-debug" ,rust-opaque-debug-0.3)
        ("rust-sha1-asm" ,rust-sha1-asm-0.4))
       #:cargo-development-inputs
       (("rust-digest" ,rust-digest-0.9)
        ("rust-hex-literal" ,rust-hex-literal-0.2))))
    (home-page
      "https://github.com/RustCrypto/hashes")
    (synopsis "SHA-1 hash function")
    (description "SHA-1 hash function")
    (license (list license:expat license:asl2.0))))
