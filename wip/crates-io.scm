(define-module (wip crates-io)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control))

(define-public ffsend
  (package
    (name "ffsend")
    (version "0.2.58")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ffsend" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0yn9di74sz52w92a5z6rp7zrk9pi9rn1nry9hahr6ngsixgkz439"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-chbs" ,rust-chbs-0.0)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-clap" ,rust-clap-2)
        ("rust-clipboard" ,rust-clipboard-0.5)
        ("rust-colored" ,rust-colored-1.9)
        ("rust-derive-builder" ,rust-derive-builder-0.9)
        ("rust-directories" ,rust-directories-2.0)
        ("rust-failure" ,rust-failure-0.1)
        ("rust-ffsend-api" ,rust-ffsend-api-0.4)
        ("rust-fs2" ,rust-fs2-0.4)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-open" ,rust-open-1)
        ("rust-openssl-probe" ,rust-openssl-probe-0.1)
        ("rust-pathdiff" ,rust-pathdiff-0.1)
        ("rust-pbr" ,rust-pbr-1)
        ("rust-prettytable-rs" ,rust-prettytable-rs-0.8)
        ("rust-qr2term" ,rust-qr2term-0.1)
        ("rust-regex" ,rust-regex-1.3)
        ("rust-rpassword" ,rust-rpassword-4.0)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-serde-derive" ,rust-serde-derive-1.0)
        ("rust-tar" ,rust-tar-0.4)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-toml" ,rust-toml-0.5)
        ("rust-urlshortener" ,rust-urlshortener-2.0)
        ("rust-version-compare" ,rust-version-compare-0.0)
        ("rust-which" ,rust-which-3.1))))
    (home-page "https://gitlab.com/timvisee/ffsend")
    (synopsis
     "Easily and securely share files from the command line. A fully featured Firefox Send client.")
    (description
     "Easily and securely share files from the command line.  A fully featured Firefox Send client.")
    (license license:gpl3)))

(define-public starship
  (package
    (name "starship")
    (version "0.33.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "starship" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "17a52d4ddjnnygi4hflih90dwmd4mkwlj7qac7x0191xmnksivwi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-ansi-term" ,rust-ansi-term-0.12)
        ("rust-battery" ,rust-battery-0.7)
        ("rust-byte-unit" ,rust-byte-unit-3.0)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-clap" ,rust-clap-2)
        ("rust-dirs" ,rust-dirs-2.0)
        ("rust-gethostname" ,rust-gethostname-0.2)
        ("rust-git2" ,rust-git2-0.11)
        ("rust-log" ,rust-log-0.4)
        ("rust-nom" ,rust-nom-5.1)
        ("rust-once-cell" ,rust-once-cell-1.3)
        ("rust-open" ,rust-open-1.3)
        ("rust-os-info" ,rust-os-info-1.3)
        ("rust-path-slash" ,rust-path-slash-0.1)
        ("rust-pretty-env-logger" ,rust-pretty-env-logger-0.3)
        ("rust-rayon" ,rust-rayon-1.3)
        ("rust-reqwest" ,rust-reqwest-0.10)
        ("rust-serde-json" ,rust-serde-json-1.0)
        ("rust-starship-module-config-derive"
         ,rust-starship-module-config-derive-0.1)
        ("rust-sysinfo" ,rust-sysinfo-0.10)
        ("rust-term-size" ,rust-term-size-0.3)
        ("rust-textwrap" ,rust-textwrap-0.11)
        ("rust-toml" ,rust-toml-0.5)
        ("rust-unicode-segmentation"
         ,rust-unicode-segmentation-1.6)
        ("rust-unicode-width" ,rust-unicode-width-0.1)
        ("rust-urlencoding" ,rust-urlencoding-1.0)
        ("rust-yaml-rust" ,rust-yaml-rust-0.4))
       #:cargo-development-inputs
       (("rust-tempfile" ,rust-tempfile-3.1))))
    (home-page "https://starship.rs")
    (synopsis
     "The cross-shell prompt for astronauts. â\x98\x84ð\x9f\x8c\x8cï¸\x8f")
    (description
     "The cross-shell prompt for astronauts.  â\x98\x84ð\x9f\x8c\x8cï¸\x8f")
    (license license:isc)))

;; Please keep these packages sorted alphabetically

(define-public rust-accelerate-src-0.3
  (package
    (name "rust-accelerate-src")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "accelerate-src" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "17fiqyq7f9k41pbsyrvk9pxyx9z6fw399wq036cvwkbmb14xcpj1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs
       (("rust-libc" ,rust-libc-0.2))
       #:skip-build? #t)) ; Only available for macOS.
    (home-page "https://github.com/blas-lapack-rs/accelerate-src")
    (synopsis "Source of BLAS and LAPACK via the Accelerate framework")
    (description
     "The package provides a source of BLAS and LAPACK via the Accelerate
 framework.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-addr2line-0.10
  (package
    (name "rust-addr2line")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "addr2line" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1daaxrzk6fmfzaqi06y704hcw0rjz199l0n9214ybfm3m3jnmc4m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cpp-demangle" ,rust-cpp-demange-0.2)
        ("rust-fallible-iterator"
         ,rust-fallible-iterator)
        ("rust-gimli" ,rust-gimli-0.19)
        ("rust-intervaltree" ,rust-intervaltree)
        ("rust-lazycell" ,rust-lazycell-1.2)
        ("rust-object" ,rust-object-0.12)
        ("rust-rustc-demangle" ,rust-rustc-demangle)
        ("rust-smallvec" ,rust-smallvec-0.6))
       #:cargo-development-inputs
       (("rust-backtrace" ,rust-backtrace-0.3)
        ("rust-clap" ,rust-clap-2)
        ("rust-findshlibs" ,rust-findshlibs-0.5)
        ("rust-memmap" ,rust-memmap)
        ("rust-rustc-test" ,rust-rustc-test-0.3))
       #:tests? #f)) ; output_equivalence fails
    (home-page "https://github.com/gimli-rs/addr2line")
    (synopsis
     "Symbolication library written in Rust, using `gimli`")
    (description
     "This package provides a cross-platform symbolication library written in Rust, using `gimli`")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-ansi-term-0.9
  (package
    (inherit rust-ansi-term-0.11)
    (name "rust-ansi-term")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ansi-term" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1xif1bh938qpfc3d0f9xgidibpm65xix11w9gszwqnia00q7rb13"))))
    (arguments '())))

(define-public rust-async-compression-0.2
  (package
    (name "rust-async-compression")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "async-compression" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "085jz6gfchjiykr4myiv7d7jv1nb9kxyv27cbz1qxmi64xi54p1c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-brotli2" ,rust-brotli2-0.3)
        ("rust-bytes" ,rust-bytes-0.5)
        ("rust-bzip2" ,rust-bzip2-0.3)
        ("rust-flate2" ,rust-flate2-1.0)
        ("rust-futures-core" ,rust-futures-core-0.3)
        ("rust-futures-io" ,rust-futures-io-0.3)
        ("rust-memchr" ,rust-memchr-2.2)
        ("rust-pin-project-lite" ,rust-pin-project-lite-0.1)
        ("rust-zstd" ,rust-zstd-0.5)
        ("rust-zstd-safe" ,rust-zstd-safe-2.0))
       #:cargo-development-inputs
       (("rust-futures" ,rust-futures-0.3)
        ("rust-futures-test" ,rust-futures-test-0.3)
        ("rust-ntest" ,rust-ntest-0.3)
        ("rust-proptest" ,rust-proptest-0.9)
        ("rust-proptest-derive" ,rust-proptest-derive-0.1)
        ("rust-rand" ,rust-rand-0.7)
        ("rust-timebomb" ,rust-timebomb-0.1))))
    (home-page "https://github.com/rustasync/async-compression")
    (synopsis
     "Adaptors between compression crates and Rust's modern asynchronous IO types.")
    (description
     "Adaptors between compression crates and Rust's modern asynchronous IO types.")
    (license (list license:expat license:asl2.0))))

(define-public rust-atlatl-0.1
  (package
    (name "rust-atlatl")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "atlatl" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "18kyvdm56fdb52b1sryi80xgs3nkjdylynsv324aiqnj85l1bfrj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-fnv" ,rust-fnv)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-serde" ,rust-serde-1.0))
       #:cargo-development-inputs
       (("rust-fst" ,rust-fst-0.1)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-quickcheck" ,rust-quickcheck-0.7)
        ("rust-rand" ,rust-rand-0.6))))
    (home-page "https://github.com/tapeinosyne/atlatl")
    (synopsis "Double-array tries.")
    (description "Double-array tries.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-automod-0.1
  (package
    (name "rust-automod")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "automod" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "17am5i7z7jpsrq9bm0wyhf4q9850g2kqvzl3ik900x5gc7brwv2a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1.0))))
    (home-page "https://github.com/dtolnay/automod")
    (synopsis "Pull in every source file in a directory as a module.")
    (description
     "Pull in every source file in a directory as a module.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-average-0.10
  (package
    (name "rust-average")
    (version "0.10.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "average" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "040i7gzx83djpqqcqlpynsd2nskc4qk6vk0w8kyr6cm03hc5mfb7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-conv" ,rust-conv-0.3)
        ("rust-float-ord" ,rust-float-ord-0.2)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-serde-big-array" ,rust-serde-big-array-0.1)
        ("rust-serde-derive" ,rust-serde-derive-1.0))
       #:cargo-development-inputs
       (("rust-bencher" ,rust-bencher-0.1)
        ("rust-proptest" ,rust-proptest-0.9)
        ("rust-quantiles" ,rust-quantiles-0.7)
        ("rust-rand" ,rust-rand-0.7)
        ("rust-rand-distr" ,rust-rand-distr-0.2)
        ("rust-rand-xoshiro" ,rust-rand-xoshiro-0.3)
        ("rust-serde-json" ,rust-serde-json-1.0)
        ("rust-streaming-stats" ,rust-streaming-stats-0.2))))
    (home-page "https://github.com/vks/average")
    (synopsis "Calculate statistics iteratively")
    (description "Calculate statistics iteratively")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-base64-0.9
  (package
    (inherit rust-base64-0.10)
    (name "rust-base64")
    (version "0.9.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "base64" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0hs62r35bgxslawyrn1vp9rmvrkkm76fqv0vqcwd048vs876r7a8"))))
    (arguments
     `(#:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-1.3))
       #:cargo-development-inputs
       (("rust-safemem" ,rust-safemem)
        ("rust-rand" ,rust-rand-0.4))))))

(define-public rust-battery-0.7
  (package
    (name "rust-battery")
    (version "0.7.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "battery" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "01srwjgbddlmpbbsfd9yi30lnbjypyc5k0aak4c5sjh297j9i9in"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if-0.1)
        ("rust-core-foundation" ,rust-core-foundation-0.6)
        ("rust-lazycell" ,rust-lazycell-1.2)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-mach" ,rust-mach-0.2)
        ("rust-nix" ,rust-nix-0.15)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-uom" ,rust-uom-0.26)
        ("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs
       (("rust-approx" ,rust-approx-0.3)
        ("rust-tempfile" ,rust-tempfile-3.0))))
    (home-page "https://github.com/svartalf/rust-battery")
    (synopsis
     "Cross-platform information about the notebook batteries")
    (description
     "Cross-platform information about the notebook batteries")
    (license (list license:asl2.0 license:expat))))

(define-public rust-bindgen-0.46
  (package
    (inherit rust-bindgen-0.50)
    (name "rust-bindgen")
    (version "0.46.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bindgen" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1qclvj5pydn5camw396b0r3nz4nn3p5wpxg4fgg1favp043pyzwg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags)
        ("rust-cexpr" ,rust-cexpr-0.3)
        ("rust-cfg-if" ,rust-cfg-if)
        ("rust-clang-sys" ,rust-clang-sys-0.26)
        ("rust-clap" ,rust-clap-2)
        ("rust-env-logger" ,rust-env-logger-0.6)
        ("rust-hashbrown" ,rust-hashbrown-0.1)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-peeking-take-while" ,rust-peeking-take-while)
        ("rust-proc-macro2" ,rust-proc-macro2)
        ("rust-quote" ,rust-quote)
        ("rust-regex" ,rust-regex-0.1)
        ("rust-which" ,rust-which-2.0))
       #:cargo-development-inputs
       (("rust-clap" ,rust-clap-2)
        ("rust-diff" ,rust-diff-0.1)
        ("rust-shlex" ,rust-shlex))))))

(define-public rust-bindgen-0.39
  (package
    (inherit rust-bindgen-0.50)
    (name "rust-bindgen")
    (version "0.39.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bindgen" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "14awv09jqiayzjdm77crizln61wxlj7lc8mpgk4c7vz95mgyvi7a"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags)
        ("rust-cexpr" ,rust-cexpr-0.2)
        ("rust-cfg-if" ,rust-cfg-if)
        ("rust-clang-sys" ,rust-clang-sys-0.23)
        ("rust-clap" ,rust-clap-2)
        ("rust-env-logger" ,rust-env-logger-0.5)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-peeking-take-while" ,rust-peeking-take-while)
        ("rust-proc-macro2" ,rust-proc-macro2-0.3.5) ; 0.3.5
        ("rust-quote" ,rust-quote-0.5)
        ("rust-regex" ,rust-regex-0.1)
        ("rust-which" ,rust-which-1))
       #:cargo-development-inputs
       (("rust-clap" ,rust-clap-2)
        ("rust-diff" ,rust-diff-0.1)
        ("rust-shlex" ,rust-shlex))))))

(define-public rust-bitflags-0.9
  (package
    (inherit rust-bitflags-1)
    (name "rust-bitflags")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bitflags" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "19dk39gfwmhi3iy1x0wgml1fv1bkb525ywy25zwihbm063i05zaf"))))))

(define-public rust-bitflags-0.8
  (package
    (inherit rust-bitflags-1)
    (name "rust-bitflags")
    (version "0.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bitflags" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1x5z8hmirpnapkx6sww8gkc6x0q8ppni0lbsigm3mrba5byfjw0k"))))))

(define-public rust-bitflags-0.7
  (package
    (inherit rust-bitflags-1)
    (name "rust-bitflags")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bitflags" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0v8hh6wdkpk9my8z8442g4hqrqf05h0qj53dsay6mv18lqvqklda"))))))

(define-public rust-blas-src-0.3
  (package
    (name "rust-blas-src")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "blas-src" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0cxln7bgwxbknaf3qbv4hscy9k53msax14x0szvvp680km3z9zs6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-accelerate-src" ,rust-accelerate-src-0.3)
        ("rust-intel-mkl-src" ,rust-intel-mkl-src)
        ("rust-netlib-src" ,rust-netlib-src)
        ("rust-openblas-src" ,rust-openblas-src-0.6))))
    (home-page "https://github.com/blas-lapack-rs/blas-src")
    (synopsis
     "The package provides a BLAS source of choice.")
    (description
     "The package provides a BLAS source of choice.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-blas-src-0.2
  (package
    (inherit rust-blas-src-0.3)
    (name "rust-blas-src")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "blas-src" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0l9c1gjhld3ajalak1ipklxfjvwqyy3l7xl019spdbqlrk8r9f57"))))))

(define-public rust-block-0.1
  (package
    (name "rust-block")
    (version "0.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "block" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "16k9jgll25pzsq14f244q22cdv0zb4bqacldg3kx6h89d7piz30d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs
       (("rust-objc-test-utils" ,rust-objc-test-utils))))
    (home-page "http://github.com/SSheldon/rust-block")
    (synopsis
     "Rust interface for Apple's C language extension of blocks.")
    (description
     "Rust interface for Apple's C language extension of blocks.")
    (license license:expat)))

(define-public rust-bodyparser-0.8
  (package
    (name "rust-bodyparser")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bodyparser" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0c1gynj9l7wv3mjrzr5jifmy0pjdwachfqz09aygdmmab3xan8zh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-iron" ,rust-iron)
        ("rust-persistent" ,rust-persistent)
        ("rust-plugin" ,rust-plugin)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-serde-json" ,rust-serde-json-1.0))
       #:cargo-development-inputs
       (("rust-serde-derive" ,rust-serde-derive-1.0))))
    (home-page "https://github.com/iron/body-parser")
    (synopsis "Body parsing middleware for Iron.")
    (description "Body parsing middleware for Iron.")
    (license license:expat)))

(define-public rust-byte-unit-3.0
  (package
    (name "rust-byte-unit")
    (version "3.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "byte-unit" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0mlh8xngs5izs8nl10ryr1q0x0zql1nql4wsy7cr0x40a2asg538"))))
    (build-system cargo-build-system)
    (home-page "https://magiclen.org/byte-unit")
    (synopsis "Library for interaction with units of bytes")
    (description
     "This package provides a library for interaction with units of bytes.")
    (license license:expat)))

(define-public rust-bytecount-0.6
  (package
    (inherit rust-bytecount-0.5)
    (name "rust-bytecount")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bytecount" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0vplsx73zncb7mz8x0fs3k0p0rz5bmavj09vjk5nqn4z6fa7h0dh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-packed-simd" ,rust-packed-simd-0.3))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.3)
        ("rust-quickcheck" ,rust-quickcheck-0.9)
        ("rust-rand" ,rust-rand-0.6))))))

(define-public rust-bytecount-0.4
  (package
    (inherit rust-bytecount-0.5)
    (name "rust-bytecount")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "bytecount" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "13qpy38z5wx0rzcdvr2h0ixbfgi1dbrif068il3hwn3k2mah88mr"))))
    (arguments
     `(#:cargo-inputs
       (("rust-packed-simd" ,rust-packed-simd-0.3))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.2)
        ("rust-quickcheck" ,rust-quickcheck-0.6)
        ("rust-rand" ,rust-rand-0.4))))))

(define-public rust-byteorder-0.5
  (package
    (inherit rust-byteorder-1.3)
    (name "rust-byteorder")
    (version "0.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "byteorder" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0ma8pkyz1jbglr29m1yzlc9ghmv6672nvsrn7zd0yn5jqs60xh8g"))))
    (arguments
     `(#:cargo-development-inputs
       (("rust-quickcheck" ,rust-quickcheck-0.2)
        ("rust-rand" ,rust-rand-0.3))
       #:tests? #f)))) ; Tests needs 'unicode' crate.

(define-public rust-cargo-metadata-0.8
  (package
    (name "rust-cargo-metadata")
    (version "0.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cargo-metadata" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "09n4fp9hrg0z84y5q0q98rlinh0832zls3q0s0ip4dbxzlqkf2vh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-semver" ,rust-semver-0.9)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-serde-derive" ,rust-serde-derive-1.0)
        ("rust-serde-json" ,rust-serde-json-1.0))
       #:cargo-development-inputs
       (("rust-clap" ,rust-clap-2)
        ("rust-docopt" ,rust-docopt-1.1)
        ("rust-structopt" ,rust-structopt-0.2))
       #:cargo-test-flags '("--release" "--test" "selftest")))
    (home-page "https://github.com/oli-obk/cargo_metadata")
    (synopsis
     "structured access to the output of `cargo metadata`")
    (description
     "structured access to the output of `cargo metadata`")
    (license license:expat)))

(define-public rust-cargo-metadata-0.6
  (package
    (inherit rust-cargo-metadata-0.8)
    (name "rust-cargo-metadata")
    (version "0.6.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cargo-metadata" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1givpi2w7iwqqnl87x5yc15zcm5hs6yw490sb6abkfp1h39v9lg5"))))
    (arguments
     `(#:cargo-inputs
       (("rust-error-chain" ,rust-error-chain-0.12)
        ("rust-semver" ,rust-semver-0.9)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-serde-derive" ,rust-serde-derive-1.0)
        ("rust-serde-json" ,rust-serde-json-1.0))
       #:cargo-development-inputs
       (("rust-clap" ,rust-clap-2)
        ("rust-docopt" ,rust-docopt-0.8))))))

(define-public rust-cargo-metadata-0.5
  (package
    (inherit rust-cargo-metadata-0.8)
    (name "rust-cargo-metadata")
    (version "0.5.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cargo-metadata" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1l3ba9mb0ihh4n33s41y3lifpfy41dgcbccz216fs0yacfwa1z0y"))))
    (arguments
     `(#:cargo-inputs
       (("rust-error-chain" ,rust-error-chain-0.11)
        ("rust-semver" ,rust-semver-0.9)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-serde-derive" ,rust-serde-derive-1.0)
        ("rust-serde-json" ,rust-serde-json-1.0))
       #:cargo-development-inputs
       (("rust-clap" ,rust-clap-2)
        ("rust-docopt" ,rust-docopt-0.8))))))

(define-public rust-cexpr-0.2
  (package
    (inherit rust-cexpr-0.3)
    (name "rust-cexpr")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cexpr" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0v1xa3758czmj8h97gh548mr8g0v13ixxvrlm1s79nb7jmgc9aj2"))))
    (arguments
     `(#:cargo-inputs
       (("rust-nom" ,rust-nom-3))
       #:cargo-development-inputs
       (("rust-clang-sys" ,rust-clang-sys-0.11))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-environmental-variable
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((clang (assoc-ref inputs "libclang")))
               (setenv "LIBCLANG_PATH"
                       (string-append clang "/lib")))
             #t)))))
    (inputs
     `(("libclang" ,clang)))))

(define-public rust-cgl-0.3
  (package
    (name "rust-cgl")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cgl" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1zs7skrsyrsm759vfy2cygkx52fx91b567a12bpaz1sf4d8hbv8c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-libc" ,rust-libc-0.2))
       #:skip-build? #t)) ; Only for macOS.
    (home-page "https://github.com/servo/cgl-rs")
    (synopsis "Rust bindings for CGL on Mac")
    (description "Rust bindings for CGL on Mac")
    (license #f)))

(define-public rust-cgmath-0.17
  (package
    (name "rust-cgmath")
    (version "0.17.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cgmath" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1rvgila6ivr0dh1bxza450a4yfwdi2pwj3h1vnwg0jy4xk6l8f98"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-approx" ,rust-approx-0.3)
        ("rust-mint" ,rust-mint)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-rand" ,rust-rand-0.6)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-simd" ,rust-simd-0.2))
       #:cargo-development-inputs
       (("rust-glium" ,rust-glium-0.26)
        ("rust-serde-json" ,rust-serde-json-1.0))))
    (home-page "https://github.com/brendanzab/cgmath")
    (synopsis
     "A linear algebra and mathematics library for computer graphics.")
    (description
     "This package provides a linear algebra and mathematics library for computer graphics.")
    (license license:asl2.0)))

(define-public rust-cgmath-0.16
  (package
    (inherit rust-cgmath-0.17)
    (name "rust-cgmath")
    (version "0.16.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cgmath" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "07754c03v3srzf64ghsl3fggrdi4kjy6l3vyq2d2wfjfixybb934"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-approx" ,rust-approx-0.1)
        ("rust-mint" ,rust-mint)
        ("rust-num-traits" ,rust-num-traits-0.1)
        ("rust-rand" ,rust-rand-0.4)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-simd" ,rust-simd-0.2))
       #:cargo-development-inputs
       (("rust-glium" ,rust-glium-0.26)
        ("rust-serde-json" ,rust-serde-json-1.0))))))

(define-public rust-chbs-0.0
  (package
    (name "rust-chbs")
    (version "0.0.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "chbs" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0nli626g17d80xr6l1rs6z1hqm93zsv5r81bznr1fihlcpdzrbbh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-derive-builder" ,rust-derive-builder-0.9)
        ("rust-failure" ,rust-failure-0.1)
        ("rust-rand" ,rust-rand-0.7))))
    (home-page "https://gitlab.com/timvisee/chbs")
    (synopsis
     "A crate providing secure passphrase generation based on a wordlist")
    (description
     "This package provides a crate providing secure passphrase generation based on a wordlist")
    (license license:expat)))

(define-public rust-ci-info-0.7
  (package
    (inherit rust-ci-info-0.3)
    (name "rust-ci-info")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ci_info" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "01n3gxmwp765m6xg1fl8v1y12wsvbqvlcai27kdr5d2skrijyfb7"))))
    (arguments
     `(#:cargo-inputs
       (("rust-envmnt" ,rust-envmnt-0.6)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-serde-derive" ,rust-serde-derive-1.0))
       #:tests? #f)))) ; Tests require internet.

(define-public rust-ci-info-0.4
  (package
    (inherit rust-ci-info-0.3)
    (name "rust-ci-info")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ci-info" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0cmhcsxvgq7nh07s2wycz6xp02zrky6fv1cwfxrn1960gqs6ikyz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-serde" ,rust-serde-1.0)
        ("rust-serde-derive" ,rust-serde-derive-1.0))))))

(define-public rust-clang-sys-0.23
  (package
    (inherit rust-clang-sys-0.28)
    (name "rust-clang-sys")
    (version "0.23.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clang-sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1hjr333izzhs6bic84qwnyzy5xzmvasib8f3zkzj4ln3a97c1xyp"))))
    (arguments
     `(#:cargo-inputs
       (("rust-glob" ,rust-glob-0.2)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-libloading" ,rust-libloading))
       #:cargo-development-inputs
       (("rust-glob" ,rust-glob-0.2))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-environmental-variable
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((clang (assoc-ref inputs "libclang")))
               (setenv "LIBCLANG_PATH"
                       (string-append clang "/lib")))
             #t)))))
    (inputs
     `(("libclang" ,clang)))))

(define-public rust-clang-sys-0.11
  (package
    (inherit rust-clang-sys-0.28)
    (name "rust-clang-sys")
    (version "0.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clang-sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "17i47skqp1d9svil2m1wspnhz7ci1x0fipia70ns0qffciwiz48r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-0.7)
        ("rust-clippy" ,rust-clippy-0.0)
        ("rust-glob" ,rust-glob-0.2)
        ("rust-lazy-static" ,rust-lazy-static-0.2)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-libloading" ,rust-libloading-0.3))
       #:cargo-development-inputs
       (("rust-clippy" ,rust-clippy-0.0)
        ("rust-glob" ,rust-glob-0.2))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-environmental-variable
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((clang (assoc-ref inputs "libclang")))
               (setenv "LIBCLANG_PATH"
                       (string-append clang "/lib")))
             #t)))))
    (inputs
     `(("libclang" ,clang)))))

(define-public rust-clipboard-0.5
  (package
    (name "rust-clipboard")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clipboard" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1rxjfn811h09g6jpjjs2vx7z52wj6dxnflbwryfj6h03dij09a95"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-clipboard-win" ,rust-clipboard-win-2.1)
        ("rust-objc" ,rust-objc-0.2)
        ("rust-objc-foundation" ,rust-objc-foundation-0.1)
        ("rust-objc-id" ,rust-objc-id-0.1)
        ("rust-x11-clipboard" ,rust-x11-clipboard-0.3))))
    (home-page "https://github.com/aweinstock314/rust-clipboard")
    (synopsis
     "rust-clipboard is a cross-platform library for getting and setting the contents of the OS-level clipboard.")
    (description
     "rust-clipboard is a cross-platform library for getting and setting the contents of the OS-level clipboard.")
    (license (list license:expat license:asl2.0))))

;; This is the last version hosted on crates.io
(define-public rust-clippy-0.0.212
  (package
    (inherit rust-clippy-0.0)
    (name "rust-clippy")
    (version "0.0.212")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clippy" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0cq5s3m6ckgi2nxjzzlmb3fdx4ym96zg25ng49zrrhqc7bqkl9by"))))
    (arguments
     `(#:cargo-inputs
       (("rust-backtrace" ,rust-backtrace-0.3)
        ("rust-clippy-lints" ,rust-clippy-lints)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-regex" ,rust-regex-0.1)
        ("rust-semver" ,rust-semver-0.9)
        ("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs
       (("rust-ansi-term" ,rust-ansi-term)
        ("rust-cargo-metadata" ,rust-cargo-metadata-0.5)
        ("rust-clippy-mini-macro-test"
         ,rust-clippy-mini-macro-test)
        ("rust-compiletest-rs" ,rust-compiletest-rs-0.3)
        ("rust-derive-new" ,rust-derive-new)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-rustc-version" ,rust-rustc-version-0.2)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-serde-derive" ,rust-serde-derive-1.0))
       ;; thread 'main' panicked at 'current rustc version information does not contain a rustc commit date'
       #:skip-build? #t))))

(define-public rust-clippy-mini-macro-test
  (package
    (name "rust-clippy-mini-macro-test")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clippy-mini-macro-test" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "01175ynzasmyymx7w4rgh0dzcp9mqac9y4fgz9xa9xb56cgjz9x2"))))
    (build-system cargo-build-system)
    (arguments '(#:skip-build? #t)) ; Requires unstable features.
    (home-page "https://github.com/rust-lang-nursery/rust-clippy")
    (synopsis
     "A macro to test clippy's procedural macro checks")
    (description
     "This package provides a macro to test clippy's procedural macro checks")
    (license license:mpl2.0)))

(define-public rust-clippy-lints
  (package
    (name "rust-clippy-lints")
    (version "0.0.212")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "clippy-lints" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "04l579yx9485qx8ksr9m153kmb9gml6v6p5xmmr9cr05ah32c8xx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cargo-metadata" ,rust-cargo-metadata-0.5)
        ("rust-if-chain" ,rust-if-chain-0.1)
        ("rust-itertools" ,rust-itertools-0.7)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-matches" ,rust-matches)
        ("rust-pulldown-cmark" ,rust-pulldown-cmark-0.1)
        ("rust-quine-mc-cluskey" ,rust-quine-mc-cluskey)
        ("rust-regex-syntax" ,rust-regex-syntax)
        ("rust-semver" ,rust-semver-0.9)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-serde-derive" ,rust-serde-derive-1.0)
        ("rust-toml" ,rust-toml-0.4)
        ("rust-unicode-normalization"
         ,rust-unicode-normalization)
        ("rust-url" ,rust-url-1.7))
       #:skip-build? #t)) ; Everything fails hard
    (home-page "https://github.com/rust-lang-nursery/rust-clippy")
    (synopsis
     "A bunch of helpful lints to avoid common pitfalls in Rust")
    (description
     "This package provides a bunch of helpful lints to avoid common pitfalls in Rust")
    (license license:mpl2.0)))

(define-public rust-cocoa-0.19
  (package
    (name "rust-cocoa")
    (version "0.19.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cocoa" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1n9pklag536ghbw93hhld8gzp1fykag67mc6h953p2c0x12h1llc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags)
        ("rust-block" ,rust-block-0.1)
        ("rust-core-foundation" ,rust-core-foundation-0.6)
        ("rust-core-graphics" ,rust-core-graphics-0.17)
        ("rust-foreign-types" ,rust-foreign-types-0.4)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-objc" ,rust-objc-0.2))))
    (home-page "https://github.com/servo/core-foundation-rs")
    (synopsis "Bindings to Cocoa for macOS")
    (description "Bindings to Cocoa for macOS")
    (license #f)))

(define-public rust-color-quant-1.0
  (package
    (name "rust-color-quant")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "color-quant" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1ga56jrafnjm80903nnqjkyii4bwd6a7visxh0g8hgi6cmrvbfqd"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/PistonDevelopers/color_quant.git")
    (synopsis
     "Color quantization library to reduce n colors to 256 colors.")
    (description
     "Color quantization library to reduce n colors to 256 colors.")
    (license license:expat)))

(define-public rust-colored-1.9
  (package
    (name "rust-colored")
    (version "1.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "colored" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0gaviala6knn5xxz8hqf4h53526fxvxl3q9jzhl9k9gkg2my45c8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-atty" ,rust-atty-0.2)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs
       (("rust-ansi-term" ,rust-ansi-term-0.12)
        ("rust-rspec" ,rust-rspec-1.0.0-beta3))))
    (home-page "https://github.com/mackwic/colored")
    (synopsis
     "The most simple way to add colors in your terminal")
    (description
     "The most simple way to add colors in your terminal")
    (license license:mpl2.0)))

(define-public rust-colored-1.8
  (package
    (inherit rust-colored-1.9)
    (name "rust-colored")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "colored" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "00vwd3r2jrd6qz4r91bwqhmkl371wyyjvirrc7bzh9r91yv91nvc"))))
    (arguments
     `(#:cargo-inputs
       (("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-winconsole" ,rust-winconsole))
       #:cargo-development-inputs
       (("rust-ansi-term" ,rust-ansi-term-0.9)
        ("rust-rspec" ,rust-rspec-1.0.0-beta3))))))

(define-public rust-compiletest-rs-0.2
  (package
    (inherit rust-compiletest-rs-0.3)
    (name "rust-compiletest-rs")
    (version "0.2.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "compiletest-rs" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0njz4shbhl1pvb6ngpi1wpz2gr5lf2dcha22lpdk995pzrwd6h97"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-log" ,rust-log-0.3)
        ("rust-rustc-serialize" ,rust-rustc-serialize-0.3)
        ("rust-tempdir" ,rust-tempdir-0.3))
       #:skip-build? #t)))) ; Requires unstable features.

(define-public rust-cookie-0.12
  (package
    (name "rust-cookie")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cookie" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1mdvqixahcywvqp0y8k2skkgbpfhsp0w73l9mz93dcrx1gq091l8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-time" ,rust-time)
        ("rust-base64" ,rust-base64-0.10)
        ("rust-ring" ,rust-ring-0.16)
        ("rust-url" ,rust-url-2.1))))
    (home-page "https://github.com/SergioBenitez/cookie-rs")
    (synopsis "Crate for parsing HTTP cookie headers and managing a cookie jar. Supports signed and private (encrypted + signed) jars.")
    (description "Crate for parsing HTTP cookie headers and managing a cookie jar. Supports signed and private (encrypted + signed) jars.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-cookie-store-0.8
  (package
    (name "rust-cookie-store")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cookie_store" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0204vzqszkjs5j4bqf8rrzrjj9c6f1zdydsid8ndkc7h7bx1v90l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cookie" ,rust-cookie-0.12)
        ("rust-idna" ,rust-idna-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-publicsuffix" ,rust-publicsuffix)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-serde-json" ,rust-serde-json-1.0)
        ("rust-time" ,rust-time)
        ("rust-try-from" ,rust-try-from)
        ("rust-url" ,rust-url-2.1))
       #:cargo-development-inputs
       (("rust-env-logger" ,rust-env-logger-0.6)
        ("rust-pretty-assertions" ,rust-pretty-assertions-0.6))))
    (home-page "https://github.com/pfernie/cookie_store")
    (synopsis
     "Implementation of Cookie storage and retrieval per [RFC6265](http://tools.ietf.org/html/rfc6265)")
    (description
     "Implementation of Cookie storage and retrieval per [RFC6265](http://tools.ietf.org/html/rfc6265)")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-cookie-store-0.7
  (package
    (inherit rust-cookie-store-0.8)
    (name "rust-cookie-store")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "cookie_store" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "174i9k9g62pfx7y1nqynywdpjplkl3j4hi3ck6bz2r996qzhnxa6"))))
    (arguments
     `(#:cargo-inputs
       (("rust-cookie" ,rust-cookie-0.12)
        ("rust-idna" ,rust-idna-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-publicsuffix" ,rust-publicsuffix)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-serde-json" ,rust-serde-json-1.0)
        ("rust-time" ,rust-time)
        ("rust-try-from" ,rust-try-from)
        ("rust-url" ,rust-url-2.1))
       #:cargo-development-inputs
       (("rust-env-logger" ,rust-env-logger-0.6)
        ("rust-pretty-assertions" ,rust-pretty-assertions-0.6))))))

(define-public rust-core-foundation-0.6
  (package
    (name "rust-core-foundation")
    (version "0.6.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "core-foundation" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0va97wf49c8dzm9c8pgyk1jn7z21rl0bj1syf2zz5m2z2hzy1f95"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-chrono" ,rust-chrono-0.4)
        ("rust-core-foundation-sys"
         ,rust-core-foundation-sys)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-uuid" ,rust-uuid-0.7))))
    (home-page "https://github.com/servo/core-foundation-rs")
    (synopsis "Bindings to Core Foundation for macOS")
    (description
     "Bindings to Core Foundation for macOS")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-core-graphics-0.17
  (package
    (name "rust-core-graphics")
    (version "0.17.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "core-graphics" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1acm3vygngnilzlr6klym5ywh7kfzh2xxrh2l41152hwmdl0jyan"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags)
        ("rust-core-foundation" ,rust-core-foundation-0.6)
        ("rust-foreign-types" ,rust-foreign-types-0.4)
        ("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/servo/core-graphics-rs")
    (synopsis "Bindings to Core Graphics for OS X")
    (description
     "Bindings to Core Graphics for OS X")
    (license #f)))

(define-public rust-criterion-0.3
  (package
    (inherit rust-criterion-0.2)
    (name "rust-criterion")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "criterion" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1iig7r9c6bkn5qb6axxkblc1amif6k49lix35rhqs728cphh71wk"))))
    (arguments
     `(#:cargo-inputs
       (("rust-atty" ,rust-atty)
        ("rust-cast" ,rust-cast-0.2)
        ("rust-clap" ,rust-clap-2)
        ("rust-criterion-plot" ,rust-criterion-plot-0.4)
        ("rust-csv" ,rust-csv-1.1)
        ("rust-itertools" ,rust-itertools-0.8)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-rand-core" ,rust-rand-core-0.5)
        ("rust-rand-os" ,rust-rand-os)
        ("rust-rand-xoshiro" ,rust-rand-xoshiro-0.3)
        ("rust-rayon" ,rust-rayon-1.1)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-serde-derive" ,rust-serde-derive-1.0)
        ("rust-serde-json" ,rust-serde-json-1.0)
        ("rust-tinytemplate" ,rust-tinytemplate)
        ("rust-walkdir" ,rust-walkdir))
       #:cargo-development-inputs
       (("rust-approx" ,rust-approx-0.3)
        ("rust-quickcheck" ,rust-quickcheck-0.8)
        ("rust-rand" ,rust-rand-0.6)
        ("rust-tempdir" ,rust-tempdir-0.3))))))

(define-public rust-criterion-plot-0.4
  (package
    (inherit rust-criterion-plot-0.3)
    (name "rust-criterion-plot")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "criterion-plot" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "18kjl0fh2n5ws6ssiqskikmz893dm9rfdgi5j2l2qddyig7cdkgc"))))
    (arguments
     `(#:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-1.3)
        ("rust-cast" ,rust-cast-0.2)
        ("rust-itertools" ,rust-itertools-0.8))
       #:cargo-development-inputs
       (("rust-itertools-num" ,rust-itertools-num-0.1)
        ("rust-num-complex" ,rust-num-complex-0.2)
        ("rust-rand" ,rust-rand-0.4))))))

(define-public rust-crossbeam-deque-0.2
  (package
    (inherit rust-crossbeam-deque-0.7)
    (name "rust-crossbeam-deque")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crossbeam-deque" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1wwwbnvxh0rza38xiws8qc46klzhv19zgvarn37pijis6v2zhfgp"))))
    (arguments
     `(#:cargo-inputs
       (("rust-crossbeam-epoch" ,rust-crossbeam-epoch-0.3)
        ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.6))
       #:cargo-development-inputs
       (("rust-rand" ,rust-rand-0.4))))))

(define-public rust-crossbeam-epoch-0.3
  (package
    (inherit rust-crossbeam-epoch-0.8)
    (name "rust-crossbeam-epoch")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crossbeam-epoch" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0l4igvp2i7b6dgaiq040j8kj8hygwdpr6ppzh1hrbsbx83sj2wcj"))))
    (arguments
     `(#:cargo-inputs
       (("rust-arrayvec" ,rust-arrayvec-0.4)
        ("rust-cfg-if" ,rust-cfg-if)
        ("rust-crossbeam-utils" ,rust-crossbeam-utils-0.2)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-memoffset" ,rust-memoffset-0.2)
        ("rust-nodrop" ,rust-nodrop)
        ("rust-rand" ,rust-rand-0.3)
        ("rust-scopeguard" ,rust-scopeguard-0.3))))))

(define-public rust-crossbeam-utils-0.2
  (package
    (inherit rust-crossbeam-utils-0.6)
    (name "rust-crossbeam-utils")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "crossbeam-utils" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1n8qr52sw9y6yxzyfxi1phh55rsxms7ry4iipdd8vmd16ag8jq17"))))
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if))))))

(define-public rust-ct-logs-0.6
  (package
    (name "rust-ct-logs")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ct-logs" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "04wiwiv4ghni3x2vni3z711mlz0ndqvh04vmdkbw3nr7zbsqcdjd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-sct" ,rust-sct-0.6))))
    (home-page "https://github.com/ctz/ct-logs")
    (synopsis
     "Google's list of Certificate Transparency logs for use with sct crate")
    (description
     "Google's list of Certificate Transparency logs for use with sct crate")
    (license (list license:asl2.0
                   license:isc
                   license:expat))))

(define-public rust-ctor-0.1
  (package
    (name "rust-ctor")
    (version "0.1.12")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ctor" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1p7fd2zp3lkb098sn740jlf3np8qg5ivycsc037b4jhqsixf736d"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-quote" ,rust-quote)
        ("rust-syn" ,rust-syn-0.15))
       #:cargo-development-inputs
       (("rust-libc-print" ,rust-libc-print))))
    (home-page
     "https://github.com/mmastrac/rust-ctor")
    (synopsis
     "__attribute__((constructor)) for Rust")
    (description
     "__attribute__((constructor)) for Rust")
    (license (list license:asl2.0 license:expat))))

(define-public rust-darling-0.10
  (package
    (name "rust-darling")
    (version "0.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "darling" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "13ia8dx03gy867j3gjqs03zxfnkdz000gysf8lk5bbgg6ajjkriz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-darling-core" ,rust-darling-core-0.10)
        ("rust-darling-macro" ,rust-darling-macro-0.10))
       #:cargo-development-inputs
       (("rust-proc-macro2" ,rust-proc-macro2)
        ("rust-quote" ,rust-quote)
        ("rust-syn" ,rust-syn-0.15))))
    (home-page
     "https://github.com/TedDriggs/darling")
    (synopsis
     "A proc-macro library for reading attributes into structs when
      implementing custom derives.
      ")
    (description
     "This package provides a proc-macro library for reading attributes into structs when
        implementing custom derives.
        ")
    (license license:expat)))

(define-public rust-darling-core-0.10
  (package
    (name "rust-darling-core")
    (version "0.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "darling-core" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "119vd2nkgc6phshw4ka9733x9iskvgxds8ks6gr1rd2lxhmm2m7f"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-fnv" ,rust-fnv)
        ("rust-ident-case" ,rust-ident-case)
        ("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-strsim" ,rust-strsim)
        ("rust-syn" ,rust-syn-1.0))))
    (home-page
     "https://github.com/TedDriggs/darling")
    (synopsis
     "Helper crate for proc-macro library for reading attributes into structs when
      implementing custom derives. Use https://crates.io/crates/darling in your code.
      ")
    (description
     "Helper crate for proc-macro library for reading attributes into structs when
        implementing custom derives.  Use https://crates.io/crates/darling in your code.
        ")
    (license license:expat)))

(define-public rust-darling-macro-0.10
  (package
    (name "rust-darling-macro")
    (version "0.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "darling-macro" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1hb2bajmf18kgbg6rzvxa78ph7bbsrlnlacq52vi021cwlrf9lqc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-darling-core" ,rust-darling-core-0.10)
        ("rust-quote" ,rust-quote)
        ("rust-syn" ,rust-syn-0.15))))
    (home-page
     "https://github.com/TedDriggs/darling")
    (synopsis
     "Internal support for a proc-macro library for reading attributes into structs when
      implementing custom derives. Use https://crates.io/crates/darling in your code.
      ")
    (description
     "Internal support for a proc-macro library for reading attributes into structs when
        implementing custom derives.  Use https://crates.io/crates/darling in your code.
        ")
    (license license:expat)))

(define-public rust-derivative-1.0
  (package
    (name "rust-derivative")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "derivative" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0fpfcw0if70gnp8hvz6ki2wasldzi31pnwx6jmjq18zpxqqa8b4l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2)
        ("rust-quote" ,rust-quote)
        ("rust-syn" ,rust-syn-0.15)
        ("rust-trybuild" ,rust-trybuild-1.0))))
    (home-page
     "https://github.com/mcarton/rust-derivative")
    (synopsis
     "A set of alternative `derive` attributes for Rust")
    (description
     "This package provides a set of alternative `derive` attributes for Rust")
    (license #f)))

(define-public rust-derive-builder-0.9
  (package
    (name "rust-derive-builder")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "derive-builder" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1h4f8vnggmpyw27fznl3cpyjrzz1nw5xrxx6ca3zcb3z54hqcrd2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-compiletest-rs" ,rust-compiletest-rs-0.3)
        ("rust-darling" ,rust-darling-0.10)
        ("rust-derive-builder-core" ,rust-derive-builder-core-0.9)
        ("rust-env-logger" ,rust-env-logger-0.5)
        ("rust-log" ,rust-log-0.4)
        ("rust-proc-macro2" ,rust-proc-macro2-1.0)
        ("rust-quote" ,rust-quote-1.0)
        ("rust-skeptic" ,rust-skeptic-0.13)
        ("rust-syn" ,rust-syn-1.0)
        ;; Build dependencies:
        ("rust-env-logger" ,rust-env-logger-0.5)
        ("rust-log" ,rust-log-0.4)
        ("rust-skeptic" ,rust-skeptic-0.13))
       #:cargo-development-inputs
       (("rust-pretty-assertions" ,rust-pretty-assertions-0.6))))
    (home-page "https://github.com/colin-kiegel/rust-derive-builder")
    (synopsis
     "Rust macro to automatically implement the builder pattern for arbitrary structs.")
    (description
     "Rust macro to automatically implement the builder pattern for arbitrary structs.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-derive-builder-0.5
  (package
    (inherit rust-derive-builder-0.9)
    (name "rust-derive-builder")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "derive-builder" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0fgl8dsigr7h70clxjq8xmsfc021w5ag262wfgcqv0ian1m8x6cc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-compiletest-rs" ,rust-compiletest-rs-0.3)
        ("rust-derive-builder-core" ,rust-derive-builder-core-0.2)
        ("rust-env-logger" ,rust-env-logger-0.6)
        ("rust-log" ,rust-log-0.4)
        ("rust-quote" ,rust-quote)
        ("rust-skeptic" ,rust-skeptic)
        ("rust-syn" ,rust-syn-0.15))
       #:cargo-development-inputs
       (("rust-env-logger" ,rust-env-logger-0.6)
        ("rust-log" ,rust-log-0.4)
        ("rust-pretty-assertions" ,rust-pretty-assertions-0.6)
        ("rust-skeptic" ,rust-skeptic))))))

(define-public rust-derive-builder-core-0.8
  (package
    (name "rust-derive-builder-core")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "derive-builder-core" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1af5fkgswbxyyhy39wjqzi16ss90j05kdck072zs3p8gb3za8s4b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-darling" ,rust-darling-0.10)
        ("rust-log" ,rust-log-0.4)
        ("rust-proc-macro2" ,rust-proc-macro2)
        ("rust-quote" ,rust-quote)
        ("rust-syn" ,rust-syn-0.15))
       #:cargo-development-inputs
       (("rust-pretty-assertions" ,rust-pretty-assertions-0.5))))
    (home-page
     "https://github.com/colin-kiegel/rust-derive-builder")
    (synopsis
     "Internal helper library for the derive_builder crate.")
    (description
     "Internal helper library for the derive_builder crate.")
    (license #f)))

(define-public rust-derive-builder-core-0.2
  (package
    (inherit rust-derive-builder-core-0.8)
    (name "rust-derive-builder-core")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "derive-builder-core" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0mxpl1ja3l60w1v5vr3733hr5mcpds2hfl6shrmy3a2zkvp28pkk"))))
    (arguments
     `(#:cargo-inputs
       (("rust-log" ,rust-log-0.3)
        ("rust-quote" ,rust-quote-0.3)
        ("rust-syn" ,rust-syn-0.11))
       #:cargo-development-inputs
       (("rust-pretty-assertions"
         ,rust-pretty-assertions-0.2))))))

(define-public rust-derive-new-0.5
  (package
    (name "rust-derive-new")
    (version "0.5.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "derive-new" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1ncibp4jhpkym7namg3viqyw8hljd32n6abg64af8qjwrn91iwvi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1.0))))
    (home-page "https://github.com/nrc/derive-new")
    (synopsis
     "`#[derive(new)]` implements simple constructor functions for structs and enums.")
    (description
     "`#[derive(new)]` implements simple constructor functions for structs and enums.")
    (license license:expat)))

(define-public rust-difference-1
  (package
    (inherit rust-difference-2.0)
    (name "rust-difference")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "difference" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1a5v0b73z7vywbclll32wjsfkdgh6wn9prnq91z0d3lag4clsc5k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-getopts" ,rust-getopts))
       #:cargo-development-inputs
       (("rust-term" ,rust-term))))))

(define-public rust-docopt-0.8
  (package
    (inherit rust-docopt-1.1)
    (name "rust-docopt")
    (version "0.8.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "docopt" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0jha611mffc2qnxvdl3pmglz07akl99lk1vihhb3nl1cd69x7b6q"))))
    (arguments
     `(#:cargo-inputs
       (("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-regex" ,rust-regex-0.2)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-serde-derive" ,rust-serde-derive-1.0)
        ("rust-strsim" ,rust-strsim-0.6))))))

(define-public rust-docopt-0.7
  (package
    (inherit rust-docopt-1.1)
    (name "rust-docopt")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "docopt" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1n6gbhsks2w9y0b4bwqyawh4ghbkka09w6pjcrq9i1sd51pflcmb"))))
    (arguments
     `(#:cargo-inputs
       (("rust-lazy-static" ,rust-lazy-static-0.2)
        ("rust-regex" ,rust-regex-0.2)
        ("rust-rustc-serialize" ,rust-rustc-serialize-0.3)
        ("rust-strsim" ,rust-strsim-0.6))))))

(define-public rust-duct-0.12
  (package
    (inherit rust-duct-0.13)
    (name "rust-duct")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "duct" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1vm1nzyi434h2zwix7c925qfa886ri1qx4nkq4hdrgkq7h9ayh1n"))))
    (arguments
     `(#:cargo-inputs
       (("rust-lazycell" ,rust-lazycell-1.2)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-os-pipe" ,rust-os-pipe)
        ("rust-shared-child" ,rust-shared-child))
       #:cargo-development-inputs
       (("rust-tempdir" ,rust-tempdir-0.3))))))

(define-public rust-enum-as-inner-0.2
  (package
    (name "rust-enum-as-inner")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "enum-as-inner" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0zg3h7k3g1z7a9ayqy63sk302d4dg5g2h274ddv80mj4jxn2cn1x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2)
        ("rust-quote" ,rust-quote)
        ("rust-syn" ,rust-syn-0.15))))
    (home-page "https://github.com/bluejekyll/enum-as-inner")
    (synopsis "A deriving proc-macro for generating functions to automatically give access to the inner members of enum.")
    (description "A deriving proc-macro for generating functions to automatically give access to the inner members of enum.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-error-chain-0.11
  (package
    (inherit rust-error-chain-0.12)
    (name "rust-error-chain")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "error-chain" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1wykkr0naizbkwxjwia1rch8xhwvgij9khqvjzs07mrmqifislgz"))))
    (arguments
     `(#:cargo-inputs
       (("rust-backtrace" ,rust-backtrace-0.3))
       #:tests? #f)))) ; has_backtrace_depending_on_env fails

(define-public rust-expectest-0.11
  (package
    (name "rust-expectest")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "expectest" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "00dv47irmsyq7brzjhz4xns3p722gm98zp39h9hq2mrzd5marpgq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-num-traits" ,rust-num-traits-0.2))))
    (home-page
     "https://github.com/zummenix/expectest")
    (synopsis
     "Crate provides matchers and matcher functions for unit testing.")
    (description
     "Crate provides matchers and matcher functions for unit testing.")
    (license (list license:expat license:asl2.0))))

(define-public rust-expectest-0.9
  (package
    (inherit rust-expectest-0.11)
    (name "rust-expectest")
    (version "0.9.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "expectest" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0f24q2a53x7sfmmrqjbwbk7pahzwkpd829fcr023kb7q5xnd6z4g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-num-traits" ,rust-num-traits-0.1))))))

(define-public rust-filetime-0.1
  (package
    (inherit rust-filetime-0.2)
    (name "rust-filetime")
    (version "0.1.15")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "filetime" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "03xishfxzpr4nfz4g3r218d6b6g94rxsqw9pw96m6wa8wgrm6iki"))))
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-redox-syscall" ,rust-redox-syscall))
       #:cargo-development-inputs
       (("rust-tempdir" ,rust-tempdir-0.3))))))

(define-public rust-findshlibs-0.4
  (package
    (inherit rust-findshlibs-0.5)
    (name "rust-findshlibs")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "findshlibs" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "045csyaxhygdiwsr21mqcd9m4c3r270xg3vrv6rssaz5nzwmhzrg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-bindgen" ,rust-bindgen-0.39))
       #:cargo-development-inputs
       (("rust-bindgen" ,rust-bindgen-0.39)
        ("rust-cfg-if" ,rust-cfg-if))))))

(define-public rust-rustfix-0.4
  (package
    (name "rust-rustfix")
    (version "0.4.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustfix" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "01zn0ysnass3mmrhxk90584y713vjfq1x97mi4saac99g9vsql3i"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-failure" ,rust-failure-0.1)
        ("rust-log" ,rust-log-0.4)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-serde-json" ,rust-serde-json-1.0))
       #:cargo-development-inputs
       (("rust-difference" ,rust-difference-2.0)
        ("rust-duct" ,rust-duct-0.12)
        ("rust-env-logger" ,rust-env-logger-0.6)
        ("rust-log" ,rust-log-0.4)
        ("rust-proptest" ,rust-proptest-0.9)
        ("rust-tempdir" ,rust-tempdir-0.3))))
    (home-page
     "https://github.com/rust-lang-nursery/rustfix")
    (synopsis
     "Automatically apply the suggestions made by rustc")
    (description
     "Automatically apply the suggestions made by rustc")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-flamer-0.4
  (package
    (inherit rust-flamer-0.3)
    (name "rust-flamer")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "flamer" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1avszq3fn4ix7p6wjfdkli6fjyxccks1qhzja92a6kpxakd35drn"))))
    (arguments
     `(#:cargo-inputs
       (("rust-flame" ,rust-flame-0.2)
        ("rust-quote" ,rust-quote)
        ("rust-syn" ,rust-syn-0.15))))))

(define-public rust-foreign-types-0.4
  (package
    (inherit rust-foreign-types-0.3)
    (name "rust-foreign-types")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "foreign-types" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0ca4i38yrf9iy5k47lr1ylb3rvcbn36d81k5pr5kzf6kmj6p111n"))))
    (arguments
     `(#:cargo-inputs
       (("rust-foreign-types-macros" ,rust-foreign-types-macros-0.1)
        ("rust-foreign-types-shared" ,rust-foreign-types-shared))))))

(define-public rust-fs2-0.4
  (package
    (name "rust-fs2")
    (version "0.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "fs2" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "04v2hwk7035c088f19mfl5b1lz84gnvv2hv6m935n0hmirszqr4m"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs
       (("rust-tempdir" ,rust-tempdir-0.3))
       #:tests? #f)) ; Tests require unstable features.
    (home-page "https://github.com/danburkert/fs2-rs")
    (synopsis
     "Cross-platform file locks and file duplication.")
    (description
     "Cross-platform file locks and file duplication.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-fs2-0.2
  (package
    (inherit rust-fs2-0.4)
    (name "rust-fs2")
    (version "0.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "fs2" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1vsih93cvds3x6f3w9bc5rnkyv8haix1px4jpcqvjyd9l7ji9m5w"))))
    (arguments
     `(#:cargo-inputs
       (("rust-kernel32-sys" ,rust-kernel32-sys-0.2)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-winapi" ,rust-winapi-0.2))
       #:cargo-development-inputs
       (("rust-tempdir" ,rust-tempdir-0.3))
       #:tests? #f)))) ; Tests require unstable features.

(define-public rust-fst-0.3
  (package
    (name "rust-fst")
    (version "0.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "fst" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1mpby7wa5mkpgjiilam94a2l9mxx9wpgs3nw2nr1a0czzwsb8zwj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-0.5)
        ("rust-memmap" ,rust-memmap-0.6))
       #:cargo-development-inputs
       (("rust-fnv" ,rust-fnv)
        ("rust-fst-levenshtein" ,rust-fst-levenshtein-0.2)
        ("rust-fst-regex" ,rust-fst-regex-0.2)
        ("rust-lazy-static" ,rust-lazy-static-0.2)
        ("rust-quickcheck" ,rust-quickcheck-0.7)
        ("rust-rand" ,rust-rand-0.6))
       #:tests? #f)) ; TODO: Fix tests.
    (home-page "https://github.com/BurntSushi/fst")
    (synopsis
     "Use finite state transducers to compactly represents sets or maps of many strings (> 1 billion is possible).")
    (description
     "Use finite state transducers to compactly represents sets or maps of many strings (> 1 billion is possible). ")
    (license (list license:unlicense
                   license:expat))))

(define-public rust-fst-0.1
  (package
    (inherit rust-fst-0.3)
    (name "rust-fst")
    (version "0.1.38")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "fst" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "15hm5ijqbb9a67zpz79i9sbk3f1cfp71rgqfqfffl3kgbs54crs6"))))
    (arguments
     `(#:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-0.5)
        ("rust-memmap" ,rust-memmap-0.4)
        ("rust-regex-syntax" ,rust-regex-syntax-0.3)
        ("rust-utf8-ranges" ,rust-utf8-ranges-0.1))
       #:cargo-development-inputs
       (("rust-fnv" ,rust-fnv)
        ("rust-lazy-static" ,rust-lazy-static-0.1)
        ("rust-quickcheck" ,rust-quickcheck-0.2)
        ("rust-rand" ,rust-rand-0.6))
       #:tests? #f)))) ; Tests require 'unicode' create.

(define-public rust-fst-levenshtein-0.2
  (package
    (name "rust-fst-levenshtein")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "fst-levenshtein" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1s5ml10442bbnpmilmwjh4pfixsj6837rg68vjzg63i3djd4524y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-fst" ,rust-fst-0.3)
        ("rust-utf8-ranges" ,rust-utf8-ranges-1.0))))
    (home-page "https://github.com/BurntSushi/fst")
    (synopsis
     "Search finite state transducers with fuzzy queries using Levenshtein automata.")
    (description
     "Search finite state transducers with fuzzy queries using Levenshtein automata.")
    (license (list license:unlicense
                   license:expat))))

(define-public rust-fst-regex-0.2
  (package
    (name "rust-fst-regex")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "fst-regex" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "126xrv3s8mrq8nqsahmpy0nlks6l3wlivqyf6a0i4g7d3vcs3b47"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-fst" ,rust-fst-0.3)
        ("rust-regex-syntax" ,rust-regex-syntax-0.3)
        ("rust-utf8-ranges" ,rust-utf8-ranges-1.0))))
    (home-page "https://github.com/BurntSushi/fst")
    (synopsis
     "Search finite state transducers with regular expression.")
    (description
     "Search finite state transducers with regular expression.")
    (license (list license:unlicense
                   license:expat))))

(define-public rust-futures-preview-0.3
  (package
    (name "rust-futures-preview")
    (version "0.3.0-alpha.18")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "futures-preview" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1dg4qijba037xqykminifxpnjasabcjx9pwa3ww8wcmj9w6gka7g"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-futures-channel-preview"
         ,rust-futures-channel-preview-0.3)
        ("rust-futures-core-preview"
         ,rust-futures-core-preview)
        ("rust-futures-executor-preview"
         ,rust-futures-executor-preview-0.3)
        ("rust-futures-io-preview"
         ,rust-futures-io-preview)
        ("rust-futures-sink-preview"
         ,rust-futures-sink-preview)
        ("rust-futures-util-preview"
         ,rust-futures-util-preview-0.3))))
    (home-page "https://rust-lang-nursery.github.io/futures-rs/")
    (synopsis
     "An implementation of futures and streams featuring zero allocations, composability, and iterator-like interfaces.")
    (description
     "An implementation of futures and streams featuring zero allocations, composability, and iterator-like interfaces.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-futures-timer-0.3
  (package
    (name "rust-futures-timer")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "futures-timer" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0l35r7nm8p43j0adkhybnwxzbjiqy0b00kgccjy3l513m9abb7lg"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-futures-preview" ,rust-futures-preview-0.3)
        ("rust-pin-utils" ,rust-pin-utils))
       #:cargo-development-inputs
       (("rust-runtime" ,rust-runtime))))
    (home-page "https://github.com/rustasync/futures-timer")
    (synopsis
     "Timeouts and intervals for futures.")
    (description
     "Timeouts and intervals for futures.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-genmesh-0.6
  (package
    (name "rust-genmesh")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "genmesh" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "17qybydyblf3hjiw7mq181jpi4vrbb8dmsj0wi347r8k0m354g89"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cgmath" ,rust-cgmath-0.16)
        ("rust-mint" ,rust-mint))))
    (home-page "https://github.com/gfx-rs/genmesh")
    (synopsis "A package for generating 3D meshes")
    (description
     "This package provides a package for generating 3D meshes")
    (license license:asl2.0)))

(define-public rust-gethostname-0.2
  (package
    (name "rust-gethostname")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gethostname" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0a609j9dhk816il2f2a01avvi5sqzxh0p38nxwrja7dcpybf54p6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs
       (("rust-pretty-assertions" ,rust-pretty-assertions-0.6))))
    (home-page "https://github.com/lunaryorn/gethostname.rs")
    (synopsis "gethostname for all platforms")
    (description "gethostname for all platforms")
    (license license:asl2.0)))

(define-public rust-gif-0.10
  (package
    (name "rust-gif")
    (version "0.10.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gif" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1bw174f7civdfgryvc8pvyhicpr96hzdajnda4s3y8iv3ch907a7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-color-quant" ,rust-color-quant-1.0)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-lzw" ,rust-lzw))
       #:cargo-development-inputs
       (("rust-glob" ,rust-glob))))
    (home-page
     "https://github.com/image-rs/image-gif")
    (synopsis "GIF de- and encoder")
    (description "GIF de- and encoder")
    (license #f)))

(define-public rust-gimli-0.19
  (package
    (inherit rust-gimli-0.18)
    (name "rust-gimli")
    (version "0.19.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gimli" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "006dpaa63y01wb58nvs2hhj3qqx52yxg20njjflr0frfbyp1hb8n"))))
    (arguments
     `(#:cargo-inputs
       (("rust-arrayvec" ,rust-arrayvec-0.4)
        ("rust-byteorder" ,rust-byteorder-1.3)
        ("rust-fallible-iterator"
         ,rust-fallible-iterator)
        ("rust-indexmap" ,rust-indexmap-1.0)
        ("rust-stable-deref-trait"
         ,rust-stable-deref-trait))
       #:cargo-development-inputs
       (("rust-crossbeam" ,rust-crossbeam-0.7)
        ("rust-getopts" ,rust-getopts)
        ("rust-memmap" ,rust-memmap)
        ("rust-num-cpus" ,rust-num-cpus)
        ("rust-object" ,rust-object-0.13)
        ("rust-rayon" ,rust-rayon-1.1)
        ("rust-regex" ,rust-regex-0.1)
        ("rust-test-assembler" ,rust-test-assembler-0.1)
        ("rust-typed-arena" ,rust-typed-arena-1.4))))))

(define-public rust-glium-0.26
  (package
    (name "rust-glium")
    (version "0.26.0-alpha3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glium" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0gsvz19cnfaqygyqmvskl2qz3ykr7mjiab7478awpw77vhpn3vkb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-backtrace" ,rust-backtrace-0.3)
        ("rust-fnv" ,rust-fnv)
        ("rust-glutin" ,rust-glutin-0.21)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-smallvec" ,rust-smallvec-0.6)
        ("rust-takeable-option" ,rust-takeable-option-0.5))
       #:cargo-development-inputs
       (("rust-cgmath" ,rust-cgmath-0.17)
        ("rust-genmesh" ,rust-genmesh-0.6)
        ("rust-gl-generator" ,rust-gl-generator-0.11)
        ("rust-image" ,rust-image)
        ("rust-obj" ,rust-obj-0.9)
        ("rust-rand" ,rust-rand-0.6)
        ("rust-rental" ,rust-rental-0.5))))
    (home-page "https://github.com/glium/glium")
    (synopsis
     "Elegant and safe OpenGL wrapper.

      Glium is an intermediate layer between OpenGL and your application. You still need to manually handle
      the graphics pipeline, but without having to use OpenGL's old and error-prone API.

      Its objectives:

      - Be safe to use. Many aspects of OpenGL that can trigger a crash if misused are automatically handled by glium.
      - Provide an API that enforces good pratices such as RAII or stateless function calls.
      - Be compatible with all OpenGL versions that support shaders, providing unified API when things diverge.
      - Avoid all OpenGL errors beforehand.
      - Produce optimized OpenGL function calls, and allow the user to easily use modern OpenGL techniques.
      ")
    (description
     "Elegant and safe OpenGL wrapper.

        Glium is an intermediate layer between OpenGL and your application.  You still need to manually handle
        the graphics pipeline, but without having to use OpenGL's old and error-prone API.

        Its objectives:

        - Be safe to use.  Many aspects of OpenGL that can trigger a crash if misused are automatically handled by glium.
        - Provide an API that enforces good pratices such as RAII or stateless function calls.
        - Be compatible with all OpenGL versions that support shaders, providing unified API when things diverge.
        - Avoid all OpenGL errors beforehand.
        - Produce optimized OpenGL function calls, and allow the user to easily use modern OpenGL techniques.
        ")
    (license license:asl2.0)))

(define-public rust-gl-generator-0.11
  (package
    (name "rust-gl-generator")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "gl-generator" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1gdchvay0k0g931b2ki33mkfixcw4radk5b8sqsm29rahxg3v8ir"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-khronos-api" ,rust-khronos-api)
        ("rust-log" ,rust-log-0.4)
        ("rust-xml-rs" ,rust-xml-rs))))
    (home-page
     "https://github.com/brendanzab/gl-rs/")
    (synopsis
     "Code generators for creating bindings to the Khronos OpenGL APIs.")
    (description
     "Code generators for creating bindings to the Khronos OpenGL APIs.")
    (license license:asl2.0)))

(define-public rust-glutin-0.21
  (package
    (name "rust-glutin")
    (version "0.21.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glutin" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0jcr3fg5wmq32db4jjvrs9867d61z6ivwcv12qsibzmvn6ifg34k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-android-glue" ,rust-android-glue-0.2)
        ("rust-cgl" ,rust-cgl-0.3)
        ("rust-cocoa" ,rust-cocoa-0.19)
        ("rust-core-foundation" ,rust-core-foundation-0.6)
        ("rust-core-graphics" ,rust-core-graphics-0.17)
        ("rust-derivative" ,rust-derivative-1.0)
        ("rust-glutin-egl-sys" ,rust-glutin-egl-sys-0.1)
        ("rust-glutin-emscripten-sys"
         ,rust-glutin-emscripten-sys-0.1)
        ("rust-glutin-gles2-sys" ,rust-glutin-gles2-sys-0.1)
        ("rust-glutin-glx-sys" ,rust-glutin-glx-sys-0.1)
        ("rust-glutin-wgl-sys" ,rust-glutin-wgl-sys-0.1)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libloading" ,rust-libloading)
        ("rust-objc" ,rust-objc-0.2)
        ("rust-osmesa-sys" ,rust-osmesa-sys)
        ("rust-parking-lot" ,rust-parking-lot-0.9)
        ("rust-wayland-client" ,rust-wayland-client)
        ("rust-winapi" ,rust-winapi-0.3)
        ("rust-winit" ,rust-winit))))
    (home-page "https://github.com/tomaka/glutin")
    (synopsis
     "Cross-platform OpenGL context provider.")
    (description
     "Cross-platform OpenGL context provider.")
    (license license:asl2.0)))

(define-public rust-glutin-egl-sys-0.1
  (package
    (name "rust-glutin-egl-sys")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glutin-egl-sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "09nk7nknjsw2svzqrxmggc53h37xl9a9xd83v4dbdckcmf3qkx13"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs
       (("rust-gl-generator" ,rust-gl-generator-0.11))))
    (home-page "https://github.com/tomaka/glutin")
    (synopsis "The egl bindings for glutin")
    (description "The egl bindings for glutin")
    (license license:asl2.0)))

(define-public rust-glutin-emscripten-sys-0.1
  (package
    (name "rust-glutin-emscripten-sys")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glutin-emscripten-sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1ix0jmm8p5if4qarzdfl5mz9rbq4hhgqarakb3bzwvyz13dkynr4"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/tomaka/glutin")
    (synopsis "The emscripten bindings for glutin")
    (description
     "The emscripten bindings for glutin")
    (license license:asl2.0)))

(define-public rust-glutin-gles2-sys-0.1
  (package
    (name "rust-glutin-gles2-sys")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glutin-gles2-sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1pswvl5zyqmqwzjr674yzslj0al2xbqsp2ai9ggb9qbshlq6r6c9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-objc" ,rust-objc-0.2))
       #:cargo-development-inputs
       (("rust-gl-generator" ,rust-gl-generator-0.11))))
    (home-page "https://github.com/tomaka/glutin")
    (synopsis "The gles2 bindings for glutin")
    (description "The gles2 bindings for glutin")
    (license license:asl2.0)))

(define-public rust-glutin-glx-sys-0.1
  (package
    (name "rust-glutin-glx-sys")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glutin-glx-sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0mxs3mil68xqqb49466n5rpwpcllj6fwqjgrcrzzmz26bv5ab40j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-x11-dl" ,rust-x11-dl))
       #:cargo-development-inputs
       (("rust-gl-generator" ,rust-gl-generator-0.11))))
    (inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://github.com/tomaka/glutin")
    (synopsis "The glx bindings for glutin")
    (description "The glx bindings for glutin")
    (license license:asl2.0)))

(define-public rust-glutin-wgl-sys-0.1
  (package
    (name "rust-glutin-wgl-sys")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "glutin-wgl-sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "08chlfzpj59q36qm212i4k879gvjzha7i90q90fds8pw3v4vn0gq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs
       (("rust-gl-generator" ,rust-gl-generator-0.11))))
    (home-page "https://github.com/tomaka/glutin")
    (synopsis "The wgl bindings for glutin")
    (description "The wgl bindings for glutin")
    (license license:asl2.0)))

(define-public rust-goblin-0.0.22
  (package
    (inherit rust-goblin-0.0)
    (name "rust-goblin")
    (version "0.0.22")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "goblin" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1a76i6zz71hjwd11pwmc8iirddj6345mfp02zl5d6bzb04sdambz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-log" ,rust-log-0.4)
        ("rust-plain" ,rust-plain)
        ("rust-scroll" ,rust-scroll-0.9))))))

(define-public rust-h2-0.1
  (package
    (name "rust-h2")
    (version "0.1.26")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "h2" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0qn457y8xh03p7c7cpk76r22gqpyqxc58g5022j3iya7d0j4rcx5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-1.3)
        ("rust-bytes" ,rust-bytes-0.4)
        ("rust-fnv" ,rust-fnv)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-http" ,rust-http-0.1)
        ("rust-indexmap" ,rust-indexmap-1.0)
        ("rust-log" ,rust-log-0.4)
        ("rust-slab" ,rust-slab)
        ("rust-string" ,rust-string-0.2)
        ("rust-tokio-io" ,rust-tokio-io-0.1))
       #:cargo-development-inputs
       (("rust-env-logger" ,rust-env-logger-0.6)
        ("rust-hex" ,rust-hex-0.3)
        ("rust-quickcheck" ,rust-quickcheck-0.8)
        ("rust-rand" ,rust-rand-0.6)
        ("rust-rustls" ,rust-rustls-0.16)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-serde-json" ,rust-serde-json-1.0)
        ("rust-tokio" ,rust-tokio-0.1)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.12)
        ("rust-walkdir" ,rust-walkdir)
        ("rust-webpki" ,rust-webpki)
        ("rust-webpki-roots" ,rust-webpki-roots))))
    (home-page "https://github.com/hyperium/h2")
    (synopsis
     "A Tokio aware, HTTP/2.0 client & server implementation for Rust.")
    (description
     "A Tokio aware, HTTP/2.0 client & server implementation for Rust.")
    (license license:expat)))

(define-public rust-hashbrown-0.1
  (package
    (inherit rust-hashbrown-0.5)
    (name "rust-hashbrown")
    (version "0.1.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hashbrown" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1np350nrzysy021ndn2135q5vpzrp5nli78ywz114d1vcnv2kbiv"))))
    (arguments
     `(#:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-1.3)
        ("rust-rayon" ,rust-rayon-1.1)
        ("rust-scopeguard" ,rust-scopeguard)
        ("rust-serde" ,rust-serde-1.0))
       #:cargo-development-inputs
       (("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-rand" ,rust-rand-0.5)
        ("rust-rayon" ,rust-rayon-1.1)
        ("rust-rustc-hash" ,rust-rustc-hash)
        ("rust-serde-test" ,rust-serde-test-1.0))))))

(define-public rust-hex-literal-0.1
  (package
    (inherit rust-hex-literal-0.2)
    (name "rust-hex-literal")
    (version "0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hex-literal" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0ffnn5g9q5xhdmzj2ic5hk9y18kyqflbmqcssqcya9gixs5r5hnx"))))
    (arguments
     `(#:cargo-inputs
       (("rust-hex-literal-impl" ,rust-hex-literal-impl-0.1)
        ("rust-proc-macro-hack" ,rust-proc-macro-hack))))))

(define-public rust-hex-literal-impl-0.1
  (package
    (inherit rust-hex-literal-impl-0.2)
    (name "rust-hex-literal-impl")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hex-literal-impl" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1nnxqhyn9l998ma04ip79bmpqv1as6003s03g26ynhrr471p022j"))))
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro-hack" ,rust-proc-macro-hack-0.4))))))

(define-public rust-html5ever
  (package
    (inherit rust-html5ever-0.23)
    (name "rust-html5ever")
    (version "0.24.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "html5ever" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1js4cr04941ld4r4fqpblvfigy75ds48qcbqhnr7nmz4l6q86m02"))))
    (arguments
     `(#:cargo-inputs
       (("rust-log" ,rust-log-0.4)
        ("rust-mac" ,rust-mac-0.1)
        ("rust-markup5ever" ,rust-markup5ever))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.2)
        ("rust-phf-codegen" ,rust-phf-codegen-0.7)
        ("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-rustc-test" ,rust-rustc-test-0.3)
        ("rust-serde-json" ,rust-serde-json-1.0)
        ("rust-string-cache-codegen" ,rust-string-cache-codegen-0.4)
        ("rust-syn" ,rust-syn-1.0)
        ("rust-typed-arena" ,rust-typed-arena-1.4))))))

(define-public rust-html5ever-0.22
  (package
    (inherit rust-html5ever-0.23)
    (name "rust-html5ever")
    (version "0.22.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "html5ever" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0vn2hj096dkp0s4lxv4j7j48wpdhfjx5ry2l5xaxmhcdc5mgl4y2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-log" ,rust-log-0.4)
        ("rust-mac" ,rust-mac-0.1)
        ("rust-markup5ever" ,rust-markup5ever-0.7))
       #:cargo-development-inputs
       (("rust-proc-macro2" ,rust-proc-macro2)
        ("rust-quote" ,rust-quote)
        ("rust-rustc-serialize" ,rust-rustc-serialize-0.3)
        ("rust-rustc-test" ,rust-rustc-test-0.3)
        ("rust-syn" ,rust-syn-0.15)
        ("rust-typed-arena" ,rust-typed-arena-1.4))))))

(define-public rust-http-body-0.1
  (package
    (name "rust-http-body")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "http-body" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0b99404k4mw6a92hvyr0qwzkqv4f866ykg0x7913limjq5cwhhb7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bytes" ,rust-bytes-0.4)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-http" ,rust-http-0.1)
        ("rust-tokio-buf" ,rust-tokio-buf-0.2))))
    (home-page
     "https://github.com/hyperium/http-body")
    (synopsis
     "Trait representing an asynchronous, streaming, HTTP request or response body.")
    (description
     "Trait representing an asynchronous, streaming, HTTP request or response body.")
    (license license:expat)))

(define-public rust-hyper-0.12
  (package
    (name "rust-hyper")
    (version "0.12.33")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyper" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "13rc9js5n8b338lwif94v6mfni5cjjvxf2jcdsrlzvnqx6y4rd3w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bytes" ,rust-bytes-0.4)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-futures-cpupool" ,rust-futures-cpupool)
        ("rust-h2" ,rust-h2-0.1)
        ("rust-http" ,rust-http-0.1)
        ("rust-http-body" ,rust-http-body-0.1)
        ("rust-httparse" ,rust-httparse-1.3)
        ("rust-iovec" ,rust-iovec)
        ("rust-itoa" ,rust-itoa)
        ("rust-log" ,rust-log-0.4)
        ("rust-net2" ,rust-net2)
        ("rust-time" ,rust-time)
        ("rust-tokio" ,rust-tokio-0.1)
        ("rust-tokio-buf" ,rust-tokio-buf-0.2)
        ("rust-tokio-executor" ,rust-tokio-executor-0.1)
        ("rust-tokio-io" ,rust-tokio-io-0.1)
        ("rust-tokio-reactor" ,rust-tokio-reactor-0.1)
        ("rust-tokio-tcp" ,rust-tokio-tcp-0.1)
        ("rust-tokio-threadpool" ,rust-tokio-threadpool-0.1)
        ("rust-tokio-timer" ,rust-tokio-timer-0.1)
        ("rust-want" ,rust-want))
       #:cargo-development-inputs
       (("rust-futures-timer" ,rust-futures-timer-0.3)
        ("rust-num-cpus" ,rust-num-cpus)
        ("rust-pretty-env-logger"
         ,rust-pretty-env-logger-0.3)
        ("rust-rustc-version" ,rust-rustc-version-0.2)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-serde-derive" ,rust-serde-derive-1.0)
        ("rust-serde-json" ,rust-serde-json-1.0)
        ("rust-spmc" ,rust-spmc-0.3)
        ("rust-tokio-fs" ,rust-tokio-fs-0.1)
        ("rust-tokio-mockstream" ,rust-tokio-mockstream-1.1)
        ("rust-url" ,rust-url-2.1))))
    (home-page "https://hyper.rs")
    (synopsis "A fast and correct HTTP library.")
    (description
     "This package provides a fast and correct HTTP library.")
    (license license:expat)))

(define-public rust-hyper-0.10
  (package
    (inherit rust-hyper-0.12)
    (name "rust-hyper")
    (version "0.10.16")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyper" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0wwjh9p3mzvg3fss2lqz5r7ddcgl1fh9w6my2j69d6k0lbcm41ha"))))
    (arguments
     `(#:cargo-inputs
       (("rust-base64" ,rust-base64-0.9)
        ("rust-httparse" ,rust-httparse-1.3)
        ("rust-language-tags" ,rust-language-tags)
        ("rust-log" ,rust-log-0.3)
        ("rust-mime" ,rust-mime-0.2)
        ("rust-num-cpus" ,rust-num-cpus)
        ("rust-time" ,rust-time)
        ("rust-traitobject" ,rust-traitobject)
        ("rust-typeable" ,rust-typeable)
        ("rust-unicase" ,rust-unicase)
        ("rust-url" ,rust-url-2.1))
       #:cargo-development-inputs
       (("rust-env-logger" ,rust-env-logger-0.6))))))

(define-public rust-hyper-old-types
  (package
    (name "rust-hyper-old-types")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyper-old-types" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1i69sks0bwamzqdbx8ffgkssxffv6crdmwjgl47nr5pkxi8vx5k8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-base64" ,rust-base64-0.10)
        ("rust-bytes" ,rust-bytes-0.4)
        ("rust-http" ,rust-http-0.1)
        ("rust-httparse" ,rust-httparse-1.3)
        ("rust-language-tags" ,rust-language-tags)
        ("rust-log" ,rust-log-0.4)
        ("rust-mime" ,rust-mime)
        ("rust-percent-encoding" ,rust-percent-encoding)
        ("rust-time" ,rust-time)
        ("rust-unicase" ,rust-unicase))))
    (home-page "https://hyper.rs")
    (synopsis "HTTP types from hyper 0.11.x")
    (description "HTTP types from hyper 0.11.x")
    (license license:expat)))

(define-public rust-hyper-native-tls
  (package
    (name "rust-hyper-native-tls")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyper-native-tls" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0s30y20qy0akzss91yxsq1x1q7rr04jy33i0cq72nx22yjc5advd"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-antidote" ,rust-antidote)
        ("rust-hyper" ,rust-hyper-0.12)
        ("rust-native-tls" ,rust-native-tls))))
    (home-page "https://github.com/sfackler/hyper-native-tls")
    (synopsis "native-tls support for Hyper 0.10.")
    (description "native-tls support for Hyper 0.10.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-hyper-rustls
  (package
    (name "rust-hyper-rustls")
    (version "0.17.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyper-rustls" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0li9xkzmqd40dbjbl9g0nbf2ka9y0q538ififyd30zsavz3qb7bi"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bytes" ,rust-bytes-0.4)
        ("rust-ct-logs" ,rust-ct-logs-0.6)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-hyper" ,rust-hyper-0.12)
        ("rust-rustls" ,rust-rustls-0.16)
        ("rust-tokio-io" ,rust-tokio-io-0.1)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.12)
        ("rust-webpki" ,rust-webpki)
        ("rust-webpki-roots" ,rust-webpki-roots))
       #:cargo-development-inputs
       (("rust-tokio" ,rust-tokio-0.1)
        ("rust-tokio-tcp" ,rust-tokio-tcp-0.1))))
    (home-page "https://github.com/ctz/hyper-rustls")
    (synopsis
     "Rustls+hyper integration for pure rust HTTPS")
    (description
     "Rustls+hyper integration for pure rust HTTPS")
    (license (list license:asl2.0
                   license:isc
                   license:expat))))

(define-public rust-hyper-tls
  (package
    (name "rust-hyper-tls")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyper-tls" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0kqp4sz8613j6nv375wfj3gh95ff4nb6a3rb1f2vbx0almm0v01s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bytes" ,rust-bytes-0.4)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-hyper" ,rust-hyper-0.12)
        ("rust-native-tls" ,rust-native-tls)
        ("rust-tokio-io" ,rust-tokio-io-0.1))
       #:cargo-development-inputs
       (("rust-tokio" ,rust-tokio-0.1))))
    (home-page "https://hyper.rs")
    (synopsis
     "Default TLS implementation for use with hyper")
    (description
     "Default TLS implementation for use with hyper")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-hyphenation
  (package
    (name "rust-hyphenation")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyphenation" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0k5msv8calmnfd5kw1rmq4bg5hn1vcd39kbsxl57sdld63xwd4q4"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-atlatl" ,rust-atlatl-0.1)
        ("rust-bincode" ,rust-bincode-1.1)
        ("rust-hyphenation-commons"
         ,rust-hyphenation-commons)
        ("rust-serde" ,rust-serde-1.0))
       #:cargo-development-inputs
       (("rust-atlatl" ,rust-atlatl-0.1)
        ("rust-bincode" ,rust-bincode-1.1)
        ("rust-hyphenation-commons"
         ,rust-hyphenation-commons)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-pocket-resources" ,rust-pocket-resources)
        ("rust-quickcheck" ,rust-quickcheck-0.8)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-unicode-normalization"
         ,rust-unicode-normalization)
        ("rust-unicode-segmentation"
         ,rust-unicode-segmentation-1.6))))
    (home-page
     "https://github.com/tapeinosyne/hyphenation")
    (synopsis
     "Knuth-Liang hyphenation for a variety of languages")
    (description
     "Knuth-Liang hyphenation for a variety of languages")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-hyphenation-commons
  (package
    (name "rust-hyphenation-commons")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "hyphenation_commons" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1pasnbk3rbdgf30jjjh1h24a9pxpdrnn0ihcivmpnzqha6mn2d4y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-atlatl" ,rust-atlatl-0.1)
        ("rust-serde" ,rust-serde-1.0))))
    (home-page
     "https://github.com/tapeinosyne/hyphenation")
    (synopsis
     "Proemial code for the `hyphenation` library")
    (description
     "Proemial code for the `hyphenation` library")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-ident-case
  (package
    (name "rust-ident-case")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ident-case" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0fac21q6pwns8gh1hz3nbq15j8fi441ncl6w4vlnd1cmc55kiq5r"))))
    (build-system cargo-build-system)
    (home-page
     "https://github.com/TedDriggs/ident_case")
    (synopsis
     "Utility for applying case rules to Rust identifiers.")
    (description
     "Utility for applying case rules to Rust identifiers.")
    (license #f)))

(define-public rust-if-chain
  (package
    (name "rust-if-chain")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "if-chain" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0zgcn31bahnsmsjc0cgk0cy38p8sfjs79yvi6rjs5zz5b5xhqdn3"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/lfairy/if_chain")
    (synopsis
     "Macro for writing nested `if let` expressions.")
    (description
     "Macro for writing nested `if let` expressions.")
    (license #f)))

(define-public rust-if-chain-0.1
  (package
    (inherit rust-if-chain)
    (name "rust-if-chain")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "if-chain" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1v2phdgq9i313svbrcaldygivd0jn25gpml7h6vyf906mbcrbb2b"))))))

(define-public rust-image
  (package
    (name "rust-image")
    (version "0.22.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "image" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "09kqym26z03j31rfzr9zd8i8y2d0apxmm4mf8bf4axdyxymfhjvv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-1.3)
        ("rust-gif" ,rust-gif-0.10)
        ("rust-jpeg-decoder" ,rust-jpeg-decoder)
        ("rust-num-iter" ,rust-num-iter)
        ("rust-num-rational" ,rust-num-rational)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-png" ,rust-png)
        ("rust-scoped-threadpool"
         ,rust-scoped-threadpool)
        ("rust-tiff" ,rust-tiff))
       #:cargo-development-inputs
       (("rust-crc32fast" ,rust-crc32fast-1.2)
        ("rust-glob" ,rust-glob)
        ("rust-num-complex" ,rust-num-complex-0.2)
        ("rust-quickcheck" ,rust-quickcheck-0.8))))
    (home-page "https://github.com/image-rs/image")
    (synopsis
     "Imaging library written in Rust. Provides basic filters and decoders for the most common image formats.")
    (description
     "Imaging library written in Rust.  Provides basic filters and decoders for the most common image formats.")
    (license license:expat)))

(define-public rust-intel-mkl-src
  (package
    (name "rust-intel-mkl-src")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "intel-mkl-src" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "16kq725cl4rnvfpwq9x4rl83ylcqs7d0xryagx8ijm6bdblbfabc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-failure" ,rust-failure-0.1)
        ("rust-pkg-config" ,rust-pkg-config-0.3)
        ("rust-reqwest" ,rust-reqwest-0.9)
        ("rust-tar" ,rust-tar)
        ("rust-xz2" ,rust-xz2-0.1))
       #:cargo-development-inputs
       (("rust-libc" ,rust-libc-0.2))))
    (inputs
     `(("pkg-config" ,pkg-config)
       ("xz" ,xz)))
    (home-page "https://github.com/rust-math/intel-mkl-src")
    (synopsis "Redistribution of Intel MKL as a crate")
    (description
     "Redistribution of Intel @acronym{MKL, Math Kernel Library} as a crate.")
    (license (list license:non-copyleft "Intel Simplified Software License"
                   license:expat)))) ; some wrapper codes

(define-public rust-iron
  (package
    (name "rust-iron")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "iron" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1s4mf8395f693nhwsr0znw3j5frzn56gzllypyl50il85p50ily6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-hyper" ,rust-hyper-0.10)
        ("rust-log" ,rust-log-0.4)
        ("rust-mime-guess" ,rust-mime-guess)
        ("rust-modifier" ,rust-modifier)
        ("rust-num-cpus" ,rust-num-cpus)
        ("rust-plugin" ,rust-plugin)
        ("rust-typemap" ,rust-typemap)
        ("rust-url" ,rust-url-2.1)
        ("rust-hyper-native-tls" ,rust-hyper-native-tls))
       #:cargo-development-inputs
       (("rust-mime" ,rust-mime)
        ("rust-time" ,rust-time))))
    (home-page "https://github.com/iron/iron")
    (synopsis "Extensible, Concurrency Focused Web Development in Rust.")
    (description "Extensible, Concurrency Focused Web Development in Rust.")
    (license license:expat)))

(define-public rust-ipconfig
  (package
    (name "rust-ipconfig")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ipconfig" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0gyqiqr4nk2dw9ild1aq3hnv6984sgydfdq7ki586q5ydwhzlyda"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-socket2" ,rust-socket2)
        ("rust-widestring" ,rust-widestring)
        ("rust-winapi" ,rust-winapi-0.3)
        ("rust-winreg" ,rust-winreg))))
    (home-page
     "https://github.com/liranringel/ipconfig")
    (synopsis
     "Get network adapters information and network configuration for windows.")
    (description
     "Get network adapters information and network configuration for windows.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-jemalloc-ctl-0.3
  (package
    (name "rust-jemalloc-ctl")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "jemalloc-ctl" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1dx4mik2ww5ic4qqv5zx9dxq6pbkclxnxa9bscg4z4njkpzsa0n5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-jemalloc-sys" ,rust-jemalloc-sys)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-paste" ,rust-paste-0.1))
       #:cargo-development-inputs
       (("rust-jemallocator" ,rust-jemallocator-0.3))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'override-jemalloc
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((jemalloc (assoc-ref inputs "jemalloc")))
               (setenv "JEMALLOC_OVERRIDE"
                       (string-append jemalloc "/lib/libjemalloc_pic.a")))
             #t)))))
    (inputs
     `(("jemalloc" ,jemalloc)))
    (home-page "https://github.com/gnzlbg/jemallocator")
    (synopsis
     "A safe wrapper over jemalloc's control and introspection APIs")
    (description
     "This package provides a safe wrapper over jemalloc's control and introspection APIs")
    (license #f)))

(define-public rust-jemalloc-sys-0.1
  (package
    (inherit rust-jemalloc-sys-0.3)
    (name "rust-jemalloc-sys")
    (version "0.1.8")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "jemalloc-sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1bh07rlzgg39ys1lsgnpxgvjj6blagp2h17fx267d0g3a272rimz"))))
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc-0.2))
       #:cargo-development-inputs
       (("rust-cc" ,rust-cc-1.0)
        ("rust-fs-extra" ,rust-fs-extra-1.1))))))

(define-public rust-jemallocator-0.1
  (package
    (inherit rust-jemallocator-0.3)
    (name "rust-jemallocator")
    (version "0.1.9")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "jemallocator" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1csabk36p06nlh3qxxsg6nkf074b2jq2cld5zriq0xazqqmd834z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-jemalloc-sys" ,rust-jemalloc-sys-0.1)
        ("rust-libc" ,rust-libc-0.2))))))

(define-public rust-jpeg-decoder
  (package
    (name "rust-jpeg-decoder")
    (version "0.1.16")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "jpeg-decoder" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1flj2wq4xdzv6nqs3vk2l3jsg4lpwiz6lfrccb30kr7azs7y3an1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-1.3)
        ("rust-rayon" ,rust-rayon-1.1))
       #:cargo-development-inputs
       (("rust-png" ,rust-png)
        ("rust-walkdir" ,rust-walkdir))))
    (home-page
     "https://github.com/kaksmet/jpeg-decoder")
    (synopsis "JPEG decoder")
    (description "JPEG decoder")
    (license #f)))

(define-public rust-khronos-api
  (package
    (name "rust-khronos-api")
    (version "3.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "khronos-api" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1p0xj5mlbagqyvvnv8wmv3cr7l9y1m153888pxqwg3vk3mg5inz2"))))
    (build-system cargo-build-system)
    (home-page
     "https://github.com/brendanzab/gl-rs/")
    (synopsis
     "The Khronos XML API Registry, exposed as byte string constants.")
    (description
     "The Khronos XML API Registry, exposed as byte string constants.")
    (license license:asl2.0)))

(define-public rust-lazy-static-1.2
  (package
    (inherit rust-lazy-static-1.4)
    (name "rust-lazy-static")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lazy-static" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "18fy414dxmg92qjsh1dl045yrpnrc64f7hbl792ran5mkndwhx53"))))
    (arguments
     `(#:cargo-inputs (("rust-spin" ,rust-spin-0.4))))))

(define-public rust-lazy-static-1.0
  (package
    (inherit rust-lazy-static-1.4)
    (name "rust-lazy-static")
    (version "1.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lazy-static" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1lvdk5mkhw0im12fignd0i38miy2gyh5cjfrrwqs7dk2scspqjgv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-spin" ,rust-spin-0.4))
       #:tests? #f)))) ; TODO: Fix doc test.

(define-public rust-lazy-static-0.2
  (package
    (inherit rust-lazy-static-1.4)
    (name "rust-lazy-static")
    (version "0.2.11")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lazy-static" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0wxy8vak7jsx6r8gx475pjqpx11p2bfq4wvw6idmqi31mp3k7w3n"))))
    (arguments
     `(#:cargo-inputs
       (("rust-compiletest-rs" ,rust-compiletest-rs-0.3)
        ("rust-spin" ,rust-spin-0.4))
       #:tests? #f)))) ; Tests fail to compile.

(define-public rust-lazy-static-0.1
  (package
    (inherit rust-lazy-static-1.4)
    (name "rust-lazy-static")
    (version "0.1.16")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lazy-static" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "05vl1h4b0iv800grsdyc3fg2bq29p70wjav6zpjvxxd5i8d6s66g"))))
    (arguments '())))

(define-public rust-libc-print
  (package
    (name "rust-libc-print")
    (version "0.1.11")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libc-print" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1sh4l815w7zxg8w17fvwj63y421sjqxxrdamzwyvg90n6mr70phv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-libc" ,rust-libc-0.2))))
    (home-page
     "https://github.com/mmastrac/rust-libc-print")
    (synopsis
     "println! and eprintln! macros on libc without stdlib")
    (description
     "println! and eprintln! macros on libc without stdlib")
    (license (list license:asl2.0 license:expat))))

(define-public rust-libflate
  (package
    (name "rust-libflate")
    (version "0.1.27")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libflate" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1p8z839c5lpl0g01mf8iglys9lgcjxw6xjw56crhwp8z7gs5s4yr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-adler32" ,rust-adler32)
        ("rust-crc32fast" ,rust-crc32fast-1.2)
        ("rust-rle-decode-fast" ,rust-rle-decode-fast)
        ("rust-take-mut" ,rust-take-mut))
       #:cargo-development-inputs
       (("rust-clap" ,rust-clap-2))))
    (home-page "https://github.com/sile/libflate")
    (synopsis
     "A Rust implementation of DEFLATE algorithm and related formats (ZLIB, GZIP)")
    (description
     "This package provides a Rust implementation of DEFLATE algorithm and related formats (ZLIB, GZIP)")
    (license license:expat)))

(define-public rust-libloading-0.3
  (package
    (inherit rust-libloading-0.5)
    (name "rust-libloading")
    (version "0.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "libloading" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0risz19rllhdc0d7nkpwkf4pcbjjgg1iim0kkmzb6kkp874hl0ha"))))
    (arguments
     `(#:cargo-inputs
       (("rust-kernel32-sys" ,rust-kernel32-sys-0.2)
        ("rust-lazy-static" ,rust-lazy-static-0.2)
        ("rust-winapi" ,rust-winapi-0.3)
        ("rust-target-build-utils" ,rust-target-build-utils-0.3))
       #:cargo-development-inputs
       (("rust-target-build-utils" ,rust-target-build-utils-0.3))
       #:tests? #f)))) ; TODO: Fix tests

(define-public rust-linked-hash-map-0.4
  (package
    (inherit rust-linked-hash-map-0.5)
    (name "rust-linked-hash-map")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "linked-hash-map" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0fd958y02ggwpa2246kmjky9xmnww7vxg0ik3rxgy23hgwlyqq3q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-clippy" ,rust-clippy-0.0)
        ("rust-heapsize" ,rust-heapsize-0.3)
        ("rust-serde" ,rust-serde-0.9)
        ("rust-serde-test" ,rust-serde-test-0.9))))))

(define-public rust-lipsum
  (package
    (name "rust-lipsum")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lipsum" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0nlxkz8zjxqmbrxqasr36a5fqn2n33cxy11w0x0a0b6mcx04dr2q"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-rand" ,rust-rand-0.6))
       #:cargo-development-inputs
       (("rust-rand-xorshift" ,rust-rand-xorshift)
        ("rust-version-sync" ,rust-version-sync-0.8))))
    (home-page "https://github.com/mgeisler/lipsum/")
    (synopsis
     "Lipsum is a lorem ipsum text generation library. Use this if you need
      some filler text for your application.

      The text is generated using a simple Markov chain, which you can also
      instantiate to generate your own pieces of pseudo-random text.")
    (description
     "Lipsum is a lorem ipsum text generation library.  Use this if you need
      some filler text for your application.

      The text is generated using a simple Markov chain, which you can also
      instantiate to generate your own pieces of pseudo-random text.")
    (license license:expat)))

(define-public rust-loom
  (package
    (inherit rust-loom-0.1)
    (name "rust-loom")
    (version "0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "loom" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0w95rjw0sv3f5psaxlgfl4fsj6imjv16v2pap18alx2w7n2f8j24"))))
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if)
        ("rust-futures-util-preview"
         ,rust-futures-util-preview-0.3)
        ("rust-generator" ,rust-generator)
        ("rust-scoped-tls" ,rust-scoped-tls)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-serde-json" ,rust-serde-json-1.0))))))

(define-public rust-lru-cache
  (package
    (name "rust-lru-cache")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "lru-cache" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "071viv6g2p3akwqmfb3c8vsycs5n7kr17b70l7la071jv0d4zqii"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-heapsize" ,rust-heapsize)
        ("rust-linked-hash-map" ,rust-linked-hash-map-0.5))))
    (home-page
     "https://github.com/contain-rs/lru-cache")
    (synopsis
     "A cache that holds a limited number of key-value pairs")
    (description
     "This package provides a cache that holds a limited number of key-value pairs")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-mach-0.2
  (package
    (name "rust-mach")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mach" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1qdhs16cl1j3w7kvy6ak7h8lbyqmr6i3i15qfzpnv9gyrn3j9pc6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-libc" ,rust-libc-0.2))))
    (home-page "https://github.com/fitzgen/mach")
    (synopsis
     "A Rust interface to the user-space API of the Mach 3.0 kernel that underlies OSX.")
    (description
     "This package provides a Rust interface to the user-space API of the Mach 3.0 kernel that underlies OSX.")
    (license license:bsd-2)))

(define-public rust-malloc-buf
  (package
    (name "rust-malloc-buf")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "malloc-buf" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1zap9m0xmd5sdsxil7v2rgb1dzlq0308f826pwvqdvjyaz0chciz"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-libc" ,rust-libc-0.2))))
    (home-page
     "https://github.com/SSheldon/malloc_buf")
    (synopsis
     "Structs for handling malloc'd memory passed to Rust.")
    (description
     "Structs for handling malloc'd memory passed to Rust.")
    (license license:expat)))

(define-public rust-malloc-buf-0.0
  (package
    (inherit rust-malloc-buf)
    (name "rust-malloc-buf")
    (version "0.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "malloc-buf" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1jqr77j89pwszv51fmnknzvd53i1nkmcr8rjrvcxhm4dx1zr1fv2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-libc" ,rust-libc-0.2))))))

(define-public rust-markup5ever
  (package
    (inherit rust-markup5ever-0.8)
    (name "rust-markup5ever")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "markup5ever" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "00wxigkiw8f777pjp7q5kfq77xpwda9zskkwp698assh8yfisf35"))))
    (arguments
     `(#:cargo-inputs
       (("rust-log" ,rust-log-0.4)
        ("rust-phf" ,rust-phf-0.7)
        ("rust-string-cache" ,rust-string-cache-0.7)
        ("rust-tendril" ,rust-tendril-0.4))
       #:cargo-development-inputs
       (("rust-phf-codegen" ,rust-phf-codegen-0.7)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-serde-derive" ,rust-serde-derive-1.0)
        ("rust-serde-json" ,rust-serde-json-1.0)
        ("rust-string-cache-codegen" ,rust-string-cache-codegen-0.4))))))

(define-public rust-markup5ever-0.7
  (package
    (inherit rust-markup5ever-0.8)
    (name "rust-markup5ever")
    (version "0.7.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "markup5ever" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1zv2k29zkpf2nb54c20ghs9r7p2kxn1hcm550m4yyghchpwkcxl9"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-phf" ,rust-phf-0.7)
        ("rust-string-cache" ,rust-string-cache-0.7)
        ("rust-tendril" ,rust-tendril-0.4)
        ("rust-phf-codegen" ,rust-phf-codegen-0.7)
        ("rust-string-cache-codegen" ,rust-string-cache-codegen-0.4))
       #:cargo-development-inputs
       (("rust-phf-codegen" ,rust-phf-codegen-0.7)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-serde-derive" ,rust-serde-derive-1.0)
        ("rust-serde-json" ,rust-serde-json-1.0)
        ("rust-string-cache-codegen" ,rust-string-cache-codegen-0.4))))))

(define-public rust-matrixmultiply
  (package
    (inherit rust-matrixmultiply-0.1)
    (name "rust-matrixmultiply")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "matrixmultiply" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "16sgc1j87hmsqmhlqpqgcpbrb00f267ikbr55fhxla8nhwnxgznw"))))
    (arguments
     `(#:cargo-inputs
       (("rust-rawpointer" ,rust-rawpointer))
       #:cargo-development-inputs
       (("rust-bencher" ,rust-bencher-0.1)
        ("rust-itertools" ,rust-itertools-0.7))))))

(define-public rust-maybe-uninit
  (package
    (name "rust-maybe-uninit")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "maybe-uninit" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "004y0nzmpfdrhz251278341z6ql34iv1k6dp1h6af7d6nd6jwc30"))))
    (build-system cargo-build-system)
    (home-page
     "https://github.com/est31/maybe-uninit")
    (synopsis
     "MaybeUninit for friends of backwards compatibility")
    (description
     "MaybeUninit for friends of backwards compatibility")
    (license (list license:asl2.0 license:expat))))

(define-public rust-memmap-0.4
  (package
    (inherit rust-memmap-0.7)
    (name "rust-memmap")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "memmap" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1skgf49pasjqkq639frhg4fjpz039blxpscgx9ahh1qhm8j349b9"))))
    (arguments
     `(#:cargo-inputs
       (("rust-fs2" ,rust-fs2-0.2)
        ("rust-kernel32-sys" ,rust-kernel32-sys-0.2)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs
       (("rust-tempdir" ,rust-tempdir-0.3))))))

(define-public rust-mime-0.2
  (package
    (inherit rust-mime-0.3)
    (name "rust-mime")
    (version "0.2.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mime" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1q1s1ax1gaz8ld3513nvhidfwnik5asbs1ma3hp6inp5dn56nqms"))))
    (arguments
     `(#:cargo-inputs
       (("rust-log" ,rust-log-0.3)
        ("rust-heapsize" ,rust-heapsize-0.3)
        ("rust-serde" ,rust-serde-0.8))
       #:cargo-development-inputs
       (("rust-serde-json" ,rust-serde-json-0.8))))))

(define-public rust-mime-guess
  (package
    (name "rust-mime-guess")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "mime_guess" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "16c5ssgali30db6jh1cndy77dd1qgcykhshiyfyjvxxf94wx03hs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-mime" ,rust-mime)
        ("rust-unicase" ,rust-unicase))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.2))))
    (home-page "https://github.com/abonander/mime_guess")
    (synopsis
     "MIME/MediaType guessing by file extension. Uses a static map of known file extension -> MIME type mappings.")
    (description
     "MIME/MediaType guessing by file extension. Uses a static map of known file extension -> MIME type mappings.")
    (license license:expat)))

(define-public rust-native-tls
  (package
    (name "rust-native-tls")
    (version "0.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "native-tls" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0ki7cj4wzyd2nach4qdjly69sp7rs0yz3n3z2ii4mm1gqajg2bab"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-openssl-probe" ,rust-openssl-probe)
        ("rust-openssl-sys" ,rust-openssl-sys)
        ("rust-schannel" ,rust-schannel)
        ("rust-security-framework" ,rust-security-framework-0.3)
        ("rust-security-framework-sys" ,rust-security-framework-sys)
        ("rust-tempfile" ,rust-tempfile-3.1))
       #:cargo-development-inputs
       (("rust-hex" ,rust-hex-0.3))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'find-openssl
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((openssl (assoc-ref inputs "openssl")))
               (setenv "OPENSSL_DIR" openssl))
             #t)))))
    (inputs
     `(("openssl" ,openssl)))
    (home-page "https://github.com/sfackler/rust-native-tls")
    (synopsis
     "An abstraction over platform-specific TLS implementations.")
    (description
     "An abstraction over platform-specific TLS implementations.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-ndarray
  (package
    (name "rust-ndarray")
    (version "0.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ndarray" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0a5rfwcbqnvbwi3nw5sfz6kf0flhmjxs64s0b4kxc6lhmyl81wvw"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-itertools" ,rust-itertools-0.7)
        ("rust-matrixmultiply" ,rust-matrixmultiply-0.1)
        ("rust-num-complex" ,rust-num-complex-0.2)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-blas-src" ,rust-blas-src-0.2)
        ("rust-cblas-sys" ,rust-cblas-sys)
        ("rust-rustc-serialize" ,rust-rustc-serialize-0.3)
        ("rust-serde" ,rust-serde-1.0))
       #:cargo-development-inputs
       (("rust-defmac" ,rust-defmac-0.1)
        ("rust-quickcheck" ,rust-quickcheck-0.7)
        ("rust-rawpointer" ,rust-rawpointer))))
    (home-page "https://github.com/rust-ndarray/ndarray")
    (synopsis
     "ndarray implements an n-dimensional container for general elements and for numerics.")
    (description
     "ndarray implements an n-dimensional container for general elements and for numerics.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-new-debug-unreachable
  (package
    (name "rust-new-debug-unreachable")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "new-debug-unreachable" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0c1br326qa0rrzxrn2rd5ah7xaprig2i9r4rwsx06vnvc1f003zl"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f)) ; SIGILL: illegal instruction
    (home-page
     "https://github.com/mbrubeck/rust-debug-unreachable")
    (synopsis
     "panic in debug, intrinsics::unreachable() in release (fork of debug_unreachable)")
    (description
     "panic in debug, intrinsics::unreachable() in release (fork of debug_unreachable)")
    (license license:expat)))

(define-public rust-no-panic
  (package
    (name "rust-no-panic")
    (version "0.1.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "no-panic" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "089gmyxg7kviimqn5nmghm5kngnmi77a0c6fbv0j67jxx7pjhq3r"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2)
        ("rust-quote" ,rust-quote)
        ("rust-syn" ,rust-syn-0.15))
       #:cargo-development-inputs
       (("rust-tempfile" ,rust-tempfile-3.1))))
    (home-page "https://github.com/dtolnay/no-panic")
    (synopsis
     "Attribute macro to require that the compiler prove a function can't ever panic.")
    (description
     "Attribute macro to require that the compiler prove a function can't ever panic.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-nom-5.1
  (package
    (inherit rust-nom-4.2)
    (name "rust-nom")
    (version "5.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nom" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "01sfs72wrrfbicb2dz41w682d7qb7ahjygajg3zyfv7y0paz8cy4"))))
    (arguments
     `(#:cargo-inputs
       (("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-lexical-core" ,rust-lexical-core-0.4)
        ("rust-memchr" ,rust-memchr-2.2)
        ("rust-regex" ,rust-regex-1.3)
        ;; Build dependencies:
        ("rust-version-check" ,rust-version-check-0.1))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.2)
        ("rust-doc-comment" ,rust-doc-comment-0.3)
        ("rust-jemallocator" ,rust-jemallocator-0.1))))))

(define-public rust-nom-3
  (package
    (inherit rust-nom-4.2)
    (name "rust-nom")
    (version "3.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "nom" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0yr8fazcspgawl6s7wmx5llz61s68jl88cnrph18fa7xf06cbbh5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-compiler-error" ,rust-compiler-error-0.1)
        ("rust-lazy-static" ,rust-lazy-static-0.2)
        ("rust-memchr" ,rust-memchr-1.0)
        ("rust-regex" ,rust-regex-0.2))
       #:tests? #f)))) ; stream::tests::seeking_consumer fails

(define-public rust-num-rational
  (package
    (name "rust-num-rational")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "num-rational" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0m5l76rdzzq98cfhnbjsxfngz6w75pal5mnfflpxqapysmw5527j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-num-bigint" ,rust-num-bigint)
        ("rust-num-integer" ,rust-num-integer)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-serde" ,rust-serde-1.0))
       #:cargo-development-inputs
       (("rust-autocfg" ,rust-autocfg))))
    (home-page
     "https://github.com/rust-num/num-rational")
    (synopsis
     "Rational numbers implementation for Rust")
    (description
     "Rational numbers implementation for Rust")
    (license #f)))

(define-public rust-obj-0.9
  (package
    (name "rust-obj")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "obj" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1p550ws511h9qic01ppn1p3kgzwyhd2gd1rnrb2z17hgc8yv9bxh"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-genmesh" ,rust-genmesh-0.6))))
    (home-page "https://github.com/kvark/obj")
    (synopsis
     "A package for loading Wavefront .obj files")
    (description
     "This package provides a package for loading Wavefront .obj files")
    (license license:asl2.0)))

(define-public rust-objc-0.2
  (package
    (name "rust-objc")
    (version "0.2.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "03ar7qxhailxgb0zi5lszv7fhwl6b1xkas5y4m8wy1vyng90zlii"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-malloc-buf" ,rust-malloc-buf-0.0)
        ("rust-objc-exception" ,rust-objc-exception-0.1))))
    (inputs
     `(("gcc-objc" ,gcc-objc)))
    (home-page
     "http://github.com/SSheldon/rust-objc")
    (synopsis
     "Objective-C Runtime bindings and wrapper for Rust.")
    (description
     "Objective-C Runtime bindings and wrapper for Rust.")
    (license license:expat)))

(define-public rust-objc-exception-0.1
  (package
    (name "rust-objc-exception")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "objc-exception" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1p00damjvy4nbfmrc90d9kbdygycrk76kq1s8v9k1hm35ydd5309"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-gcc" ,rust-gcc))))
    (inputs
     `(("gcc-objc" ,gcc-objc)))
    (home-page "http://github.com/SSheldon/rust-objc-exception")
    (synopsis
     "Rust interface for Objective-C's throw and try/catch statements.")
    (description
     "Rust interface for Objective-C's throw and try/catch statements.")
    (license license:expat)))

(define-public rust-object-0.13
  (package
    (inherit rust-object-0.12)
    (name "rust-object")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "object" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "05gfh6da1aavksxxq52hpzy40yiqpwzra8lfk7pcc45qqrdw97nq"))))
    (arguments
     `(#:cargo-inputs
       (("rust-goblin" ,rust-goblin-0.0)
        ("rust-scroll" ,rust-scroll-0.9)
        ("rust-target-lexicon" ,rust-target-lexicon-0.4)
        ("rust-uuid" ,rust-uuid-0.7)
        ("rust-crc32fast" ,rust-crc32fast-1.2)
        ("rust-flate2" ,rust-flate2-1.0)
        ("rust-indexmap" ,rust-indexmap-1.0)
        ("rust-parity-wasm" ,rust-parity-wasm))
       #:cargo-development-inputs
       (("rust-memmap" ,rust-memmap))))))

(define-public rust-once-cell-1.3
  (package
    (inherit rust-once-cell-1.2)
    (name "rust-once-cell")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "once-cell" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0nyf4659py8ccnrrwdvw242h3j3qlrw6zi5gsjb9bjbm0n0h3imi"))))
    (arguments
     `(#:cargo-inputs
       (("rust-parking-lot" ,rust-parking-lot-0.9))
       #:cargo-development-inputs
       (("rust-crossbeam-utils" ,rust-crossbeam-utils-0.6)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-regex" ,rust-regex-1.3))))))

(define-public rust-open-1.3
  (package
    (name "rust-open")
    (version "1.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "open" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0jqkq28r4zcrfsgavgl1303hhdz6ggs6yp1323gv0a3313hj9d4l"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/Byron/open-rs")
    (synopsis
     "Open a path or URL using the program configured on the system")
    (description
     "Open a path or URL using the program configured on the system")
    (license license:expat)))

(define-public rust-openblas-src
  (package
    (name "rust-openblas-src")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "openblas-src" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1dyf7yh6rmkk7k3pgcp5p8248f08hhajkigw42bfwjw1d3jk6d8b"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs
       (("rust-libc" ,rust-libc-0.2))
       #:cargo-build-flags '("--release" "--features=system")
       #:cargo-test-flags '("--release" "--features=system")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unbundle-openblas
           (lambda _
             (delete-file-recursively "source")
             #t)))))
    (inputs
     `(("gfortran" ,gfortran "lib")
       ("openblas" ,openblas)))
    (home-page "https://github.com/blas-lapack-rs/openblas-src")
    (synopsis "Source of BLAS and LAPACK via OpenBLAS")
    (description
     "The package provides a source of BLAS and LAPACK via OpenBLAS.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-openblas-src-0.6
  (package
    (inherit rust-openblas-src)
    (name "rust-openblas-src")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "openblas-src" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1bbs55s8bz2z0gcj7kh9ykxqn3x79m4cnmip7r6n5w4msyinalmg"))))
    (arguments
     `(#:cargo-development-inputs
       (("rust-libc" ,rust-libc-0.2))
       #:cargo-build-flags '("--release" "--features=system")
       #:cargo-test-flags '("--release" "--features=system")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unbundle-openblas
           (lambda _
             (delete-file-recursively "source")
             #t)))))
    (inputs
     `(("gfortran" ,gfortran "lib")
       ("openblas" ,openblas)))))

(define-public rust-ordermap
  (package
    (inherit rust-ordermap-0.3)
    (name "rust-ordermap")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ordermap" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1m0vxmlm1x92m1ydgpddzg5mrfk3ddy8gk3r9dmpml18qrs9ch4i"))))
    (arguments
     `(#:cargo-inputs
       (("rust-serde" ,rust-serde-1.0))
       #:cargo-development-inputs
       (("rust-fnv" ,rust-fnv)
        ("rust-itertools" ,rust-itertools-0.7)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-quickcheck" ,rust-quickcheck-0.6)
        ("rust-rand" ,rust-rand-0.4)
        ("rust-serde-test" ,rust-serde-test-1.0))))))

(define-public rust-os-info-1.3
  (package
    (name "rust-os-info")
    (version "1.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "os-info" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0q5wn2fbz0c0m1rnaj77d7lwcpxv3fbfms3cq8qw5q67wvsg30pc"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-regex" ,rust-regex-1.3)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-serde-derive" ,rust-serde-derive-1.0)
        ("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs
       (("rust-doc-comment" ,rust-doc-comment-0.3)
        ("rust-itertools" ,rust-itertools-0.8)
        ("rust-pretty-assertions" ,rust-pretty-assertions-0.6))))
    (home-page
     "https://github.com/darkeld3r/os_info")
    (synopsis
     "Detect the operating system type and version.")
    (description
     "Detect the operating system type and version.")
    (license license:expat)))

(define-public rust-osmesa-sys
  (package
    (name "rust-osmesa-sys")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "osmesa-sys" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0fq1q1zcgfb0qydrg9r2738jlwc4hqxgb9vj11z72bjxx7kfrkw8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-shared-library" ,rust-shared-library))))
    (home-page
     "https://github.com/Daggerbot/osmesa-rs.git")
    (synopsis "OSMesa library bindings for Rust")
    (description "OSMesa library bindings for Rust")
    (license license:cc0)))

(define-public rust-output-vt100
  (package
    (name "rust-output-vt100")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "output-vt100" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1ygqplpxz4gg3i8f3rkan2q69pqll7gv65l2mmd8r9dphnvwbkak"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-winapi" ,rust-winapi-0.3))))
    (home-page
     "https://github.com/Phundrak/output-vt100-rs")
    (synopsis
     "Utility to activate escape codes in Windows' CMD and PowerShell")
    (description
     "Utility to activate escape codes in Windows' CMD and PowerShell")
    (license license:expat)))

(define-public rust-path-slash-0.1
  (package
    (name "rust-path-slash")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "path-slash" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0bvfzywc2nh6kdwh3nanyd6dibbhmzhppb2f3xa7aqhkv7s8m1d0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs
       (("rust-lazy-static" ,rust-lazy-static-1))))
    (home-page "https://github.com/rhysd/path-slash")
    (synopsis
     "Conversion to/from a file path from/to slash path")
    (description
     "Conversion to/from a file path from/to slash path")
    (license license:expat)))

(define-public rust-persistent
  (package
    (name "rust-persistent")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "persistent" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1iblxjjzd0h784l5y573nw5z3pdb3330k69hh413agagkh0a13wf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-iron" ,rust-iron)
        ("rust-plugin" ,rust-plugin))))
    (home-page "https://github.com/iron/persistent")
    (synopsis "A set of middleware for sharing server-global data in Iron.")
    (description "A set of middleware for sharing server-global data in Iron.")
    (license license:expat)))

(define-public rust-pretty-env-logger-0.3
  (package
    (name "rust-pretty-env-logger")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pretty_env_logger" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0x4hyjlnvvhyk9m74iypzybm22w3dl2k8img4b956239n5vf8zki"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-chrono" ,rust-chrono-0.4)
        ("rust-env-logger" ,rust-env-logger-0.6)
        ("rust-log" ,rust-log-0.4))))
    (home-page
     "https://github.com/seanmonstar/pretty-env-logger")
    (synopsis
     "A simple logger built on top off env_logger. It is configured via an environment variable and writes to standard error with nice colored output for log levels.")
    (description
     "A simple logger built on top off env_logger. It is configured via an environment variable and writes to standard error with nice colored output for log levels.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-pretty-assertions-0.6
  (package
    (name "rust-pretty-assertions")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pretty_assertions" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "09yl14gnmpygiqrdlsa64lcl4w6ydjl9m8jri6kgam0v9rjf309z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-ansi-term" ,rust-ansi-term)
        ("rust-ctor" ,rust-ctor-0.1)
        ("rust-difference" ,rust-difference-2.0)
        ("rust-output-vt100" ,rust-output-vt100))))
    (home-page
     "https://github.com/colin-kiegel/rust-pretty-assertions")
    (synopsis
     "When writing tests in Rust, you'll probably use assert_eq!(a, b) a lot.

      If such a test fails, it will present all the details of a and b. But you have to spot the differences yourself, which is not always straightforward, like here:")
    (description
     "When writing tests in Rust, you'll probably use assert_eq!(a, b) a lot.

      If such a test fails, it will present all the details of a and b. But you have to spot the differences yourself, which is not always straightforward, like here:")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-pretty-assertions-0.5
  (package
    (inherit rust-pretty-assertions-0.6)
    (name "rust-pretty-assertions")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pretty-assertions" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1ins6swkpxmrh8q5h96h8nv0497d3dclsiyx2lyvqi6py0q980is"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-ansi-term" ,rust-ansi-term)
        ("rust-difference" ,rust-difference-2.0))))))

(define-public rust-pretty-assertions-0.2
  (package
    (inherit rust-pretty-assertions-0.6)
    (name "rust-pretty-assertions")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pretty-assertions" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1b3nv70i16737w3qkk1q5vqswwnb19znz8r9v2kcg1qyhh3h0l8x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-difference" ,rust-difference-1))))))

(define-public rust-proc-macro2-0.3.5
  (package
    (inherit rust-proc-macro2-0.4)
    (name "rust-proc-macro2")
    (version "0.3.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "proc-macro2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1m0ksg6hbm46zblq0dpkwrg3n1h7n90yq1zcgwc6vpbfmr9pr6bp"))))
    (arguments
     `(#:cargo-inputs
       (("rust-unicode-xid" ,rust-unicode-xid-0.1))))))
(define-public rust-proc-macro2-0.3
  rust-proc-macro2-0.3.5)

(define-public rust-proc-macro-hack-0.4
  (package
    (inherit rust-proc-macro-hack-0.5)
    (name "rust-proc-macro-hack")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "proc-macro-hack" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0fxn3qfhw76c518dfal2qqjwj5dbf0a1f7z0r5c4wd0igygg4fs6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro-hack-impl" ,rust-proc-macro-hack-impl))
       #:cargo-development-inputs
       (("rust-demo-hack" ,rust-demo-hack-0.0)
        ("rust-demo-hack-impl" ,rust-demo-hack-impl-0.0))))))

(define-public rust-publicsuffix
  (package
    (name "rust-publicsuffix")
    (version "1.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "publicsuffix" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "07ddqpri1xy7nxg5b7z8w49gw90rkn4qjvr423b4y7ngdnlcpzjs"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-error-chain" ,rust-error-chain-0.12)
        ("rust-idna" ,rust-idna-0.2)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-regex" ,rust-regex-0.1)
        ("rust-url" ,rust-url-2.1)
        ("rust-native-tls" ,rust-native-tls))
       #:cargo-development-inputs
       (("rust-rspec" ,rust-rspec-1.0))))
    (home-page
     "https://github.com/rushmorem/publicsuffix")
    (synopsis
     "Robust domain name parsing and RFC compliant email address validation")
    (description
     "Robust domain name parsing and RFC compliant email address validation")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-pulldown-cmark
  (package
    (inherit rust-pulldown-cmark-0.4)
    (name "rust-pulldown-cmark")
    (version "0.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pulldown-cmark" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0mq3cgjwxlrv0p1svjg93m1jkybzyfrl9p0jwa76hx1352hks13p"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags)
        ("rust-getopts" ,rust-getopts)
        ("rust-memchr" ,rust-memchr-2.2)
        ("rust-unicase" ,rust-unicase))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.2)
        ("rust-html5ever" ,rust-html5ever)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-regex" ,rust-regex-0.1)
        ("rust-tendril" ,rust-tendril-0.4))))))

(define-public rust-pulldown-cmark-0.2
  (package
    (inherit rust-pulldown-cmark)
    (name "rust-pulldown-cmark")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pulldown-cmark" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "05gfnqa0wzix5m17jrmgj0yyr9sflqm0knn79ndppsnhcan2zxgf"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags)
        ("rust-getopts" ,rust-getopts))
       #:tests? #f)))) ; Spec tests fail because of encoding errors.

(define-public rust-pulldown-cmark-0.1
  (package
    (inherit rust-pulldown-cmark)
    (name "rust-pulldown-cmark")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "pulldown-cmark" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0ckflr6w5vfvgb2xnzbnph9b6c0k8cfncm4a8bjzmbbcv9fgizfn"))))
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-0.9)
        ("rust-getopts" ,rust-getopts))
       #:tests? #f)))) ; Spec tests fail because of encoding errors.

(define-public rust-quickcheck-0.9
  (package
    (inherit rust-quickcheck-0.8)
    (name "rust-quickcheck")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "quickcheck" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0qi67dg7cf50i23ac7n5qhg4vhhsm6xznhpl2wsqv86s5x551jnm"))))
    (arguments
     `(#:cargo-inputs
       (("rust-env-logger" ,rust-env-logger-0.6)
        ("rust-log" ,rust-log-0.4)
        ("rust-rand" ,rust-rand-0.6)
        ("rust-rand-core" ,rust-rand-core-0.5))))))

(define-public rust-quickcheck-0.4
  (package
    (inherit rust-quickcheck-0.8)
    (name "rust-quickcheck")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "quickcheck" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "01hligcv1h4pvc8ykch65qjzi7jgcq2s462v69j27slc84fl3hh2"))))
    (arguments
     `(#:cargo-inputs
       (("rust-env-logger" ,rust-env-logger-0.3)
        ("rust-log" ,rust-log-0.3)
        ("rust-rand" ,rust-rand-0.3))))))

(define-public rust-quickcheck-0.3
  (package
    (inherit rust-quickcheck-0.8)
    (name "rust-quickcheck")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "quickcheck" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "01a6s6lmnjld9lahbl54qp7h7x2hnkkzhcyr2gdhbk460sj3scqb"))))
    (arguments
     `(#:cargo-inputs
       (("rust-env-logger" ,rust-env-logger-0.3)
        ("rust-log" ,rust-log-0.3)
        ("rust-rand" ,rust-rand-0.3))
       #:skip-build? #t)))) ; Package needs 'unicode' crate.

(define-public rust-quine-mc-cluskey
  (package
    (name "rust-quine-mc-cluskey")
    (version "0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "quine-mc-cluskey" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0iazdlwffhrlksb8yhhs1prgwpa68rwjwqm4v26hr9hrswarcn07"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-quickcheck" ,rust-quickcheck-0.3))))
    (home-page
     "https://github.com/oli-obk/quine-mc_cluskey")
    (synopsis
     "Rust implementation of the Quine-McCluskey algorithm and Petrick's method")
    (description
     "Rust implementation of the Quine-McCluskey algorithm and Petrick's method")
    (license license:expat)))

(define-public rust-quote-0.5
  (package
    (inherit rust-quote-0.6)
    (name "rust-quote")
    (version "0.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "quote" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1s01fh0jl8qv4xggs85yahw0h507nzrxkjbf7vay3zw8d3kcyjcr"))))
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-0.3))))))

(define-public rust-rand-distr-0.2
  (package
    (name "rust-rand-distr")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rand_distr" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1cpz577qid09lirjjhhn98yqdwsv0c01jf973pxpcr9svp5pm5wn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-rand" ,rust-rand-0.7))
       #:cargo-development-inputs
       (("rust-average" ,rust-average-0.10)
        ("rust-rand-pcg" ,rust-rand-pcg-0.2))))
    (home-page "https://crates.io/crates/rand_distr")
    (synopsis
     "Sampling from random number distributions")
    (description
     "Sampling from random number distributions")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rand-isaac
  (package
    (name "rust-rand-isaac")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rand_isaac" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0xlb9415x518ffkazxhvk8b04i9i548nva4i5l5s34crvjrv1xld"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-rand-core" ,rust-rand-core-0.5)
        ("rust-serde" ,rust-serde-1.0))
       #:cargo-development-inputs
       (("rust-bincode" ,rust-bincode-1.1))))
    (home-page "https://crates.io/crates/rand_isaac")
    (synopsis "ISAAC random number generator")
    (description "ISAAC random number generator")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rand-jitter
  (package
    (name "rust-rand-jitter")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rand_jitter" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1mnjbfzj97g788jslz0k77bpsg6qjhz676cibk82ibbvgqp4sy43"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-log" ,rust-log-0.4)
        ("rust-rand-core" ,rust-rand-core-0.5)
        ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/rust-random/rand")
    (synopsis
     "Random number generator based on timing jitter")
    (description
     "Random number generator based on timing jitter")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-rayon-1.0
  (package
    (inherit rust-rayon-1.3)
    (name "rust-rayon")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rayon" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0wq41f15y05nlarijn9c1vxscxj5sazn3lhd6mmnicj5fzr18f1p"))))
    (arguments
     `(#:cargo-inputs
       (("rust-crossbeam-deque" ,rust-crossbeam-deque-0.2)
        ("rust-either" ,rust-either-1.5)
        ("rust-rand" ,rust-rand-0.5)
        ("rust-rayon-core" ,rust-rayon-core-1.5))
       #:cargo-development-inputs
       (("rust-docopt" ,rust-docopt-1.1)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-serde-derive" ,rust-serde-derive-1.0))))))

(define-public rust-rayon-0.8
  (package
    (inherit rust-rayon-1.3)
    (name "rust-rayon")
    (version "0.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rayon" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1j2l9x98ma63qkh9w8zik0vcpwqf9cvc2ynh66ibjp36nq4gw55n"))))
    (arguments
     `(#:cargo-inputs
       (("rust-rayon-core" ,rust-rayon-core-1.5))
       #:cargo-development-inputs
       (("rust-compiletest-rs" ,rust-compiletest-rs-0.2)
        ("rust-docopt" ,rust-docopt-0.7)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-rand" ,rust-rand-0.3)
        ("rust-rustc-serialize" ,rust-rustc-serialize-0.3))
       #:tests? #f)))) ; Fails to compile compiletest-rs.

(define-public rust-rdrand
  (package
    (inherit rust-rdrand-0.4)
    (name "rust-rdrand")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rdrand" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "02dsyibvl1iygkmljr0ld9vjyp525q4mjy5j9yazrrkyvhvw7gvp"))))
    (arguments
     `(#:cargo-inputs
       (("rust-rand-core" ,rust-rand-core-0.5))))))

(define-public rust-regex
  (package
    (inherit rust-regex-1.3)
    (name "rust-regex")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "regex" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1k6jpx0bqlg6vb8rsdnlmcw2szczbl51j047w3blpa4qzn6xl8vb"))))
    (arguments
     `(#:cargo-inputs
       (("rust-aho-corasick" ,rust-aho-corasick-0.7)
        ("rust-memchr" ,rust-memchr-2.2)
        ("rust-regex-syntax" ,rust-regex-syntax)
        ("rust-thread-local" ,rust-thread-local)
        ("rust-utf8-ranges" ,rust-utf8-ranges-1.0))
       #:cargo-development-inputs
       (("rust-doc-comment" ,rust-doc-comment)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-quickcheck" ,rust-quickcheck-0.8)
        ("rust-rand" ,rust-rand-0.6))))))

(define-public rust-rental-0.5
  (package
    (name "rust-rental")
    (version "0.5.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rental" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0zw4cxyqf2w67kl5v6nrab0hg6qfaf5n3n6a0kxkkcdjk2zdwic5"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-rental-impl" ,rust-rental-impl-0.5)
        ("rust-stable-deref-trait"
         ,rust-stable-deref-trait))))
    (home-page "https://github.com/jpernst/rental")
    (synopsis
     "A macro to generate safe self-referential structs, plus premade types for common use cases.")
    (description
     "This package provides a macro to generate safe self-referential structs, plus premade types for common use cases.")
    (license #f)))

(define-public rust-rental-impl-0.5
  (package
    (name "rust-rental-impl")
    (version "0.5.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rental-impl" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1pj0qgmvwwsfwyjqyjxzikkwbwc3vj7hm3hdykr47dy5inbnhpj7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1)
        ("rust-quote" ,rust-quote-1)
        ("rust-syn" ,rust-syn-1.0))))
    (home-page "https://www.jpernst.com")
    (synopsis
     "An implementation detail of rental. Should not be used directly.")
    (description
     "An implementation detail of rental.  Should not be used directly.")
    (license #f)))

(define-public rust-reqwest-0.10
  (package
    (name "rust-reqwest")
    (version "0.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "reqwest" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1r1z26di07vf09x8b8867d40x0mcx7ij4iih1b1zd2r5kvhriry0"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-async-compression" ,rust-async-compression-0.2)
        ("rust-base64" ,rust-base64-0.11)
        ("rust-bytes" ,rust-bytes-0.5)
        ("rust-cookie" ,rust-cookie-0.12)
        ("rust-cookie-store" ,rust-cookie-store-0.10)
        ("rust-encoding-rs" ,rust-encoding-rs-0.8)
        ("rust-futures-core" ,rust-futures-core-0.3)
        ("rust-futures-util" ,rust-futures-util-0.3)
        ("rust-http" ,rust-http-0.2)
        ("rust-http-body" ,rust-http-body-0.3)
        ("rust-hyper" ,rust-hyper-0.13)
        ("rust-hyper-rustls" ,rust-hyper-rustls-0.19)
        ("rust-hyper-tls" ,rust-hyper-tls-0.4)
        ("rust-js-sys" ,rust-js-sys-0.3)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-mime" ,rust-mime-0.3)
        ("rust-mime-guess" ,rust-mime-guess-2.0)
        ("rust-native-tls" ,rust-native-tls-0.2)
        ("rust-percent-encoding" ,rust-percent-encoding-2.1)
        ("rust-pin-project-lite" ,rust-pin-project-lite-0.1)
        ("rust-rustls" ,rust-rustls-0.16)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-serde-json" ,rust-serde-json-1.0)
        ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.6)
        ("rust-time" ,rust-time-0.1)
        ("rust-tokio" ,rust-tokio-0.2)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.12)
        ("rust-tokio-socks" ,rust-tokio-socks-0.2)
        ("rust-tokio-tls" ,rust-tokio-tls-0.3)
        ("rust-url" ,rust-url-2.1)
        ("rust-wasm-bindgen" ,rust-wasm-bindgen-0.2)
        ("rust-wasm-bindgen-futures" ,rust-wasm-bindgen-futures-0.4)
        ("rust-web-sys" ,rust-web-sys-0.3)
        ("rust-webpki-roots" ,rust-webpki-roots-0.17)
        ("rust-winreg" ,rust-winreg-0.6))
       #:cargo-development-inputs
       (("rust-doc-comment" ,rust-doc-comment-0.3)
        ("rust-env-logger" ,rust-env-logger-0.6)
        ("rust-hyper" ,rust-hyper-0.13)
        ("rust-libflate" ,rust-libflate-0.1)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-tokio" ,rust-tokio-0.2))))
    (home-page
     "https://github.com/seanmonstar/reqwest")
    (synopsis "higher level HTTP client library")
    (description "higher level HTTP client library")
    (license (list license:expat license:asl2.0))))

(define-public rust-reqwest-0.9
  (package
    (inherit rust-reqwest-0.10)
    (name "rust-reqwest")
    (version "0.9.20")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "reqwest" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0y4wvzl3pspd8drr2hf9kk107cjw455cb6p529sh90x58dhqjv8g"))))
    (arguments
     `(#:cargo-inputs
       (("rust-base64" ,rust-base64-0.10)
        ("rust-bytes" ,rust-bytes-0.4)
        ("rust-cookie" ,rust-cookie-0.12)
        ("rust-cookie-store" ,rust-cookie-store-0.7)
        ("rust-encoding-rs" ,rust-encoding-rs-0.8)
        ("rust-flate2" ,rust-flate2-1.0)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-http" ,rust-http-0.1)
        ("rust-hyper" ,rust-hyper-0.12)
        ("rust-log" ,rust-log-0.4)
        ("rust-mime" ,rust-mime-0.3)
        ("rust-mime-guess" ,rust-mime-guess-2.0)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-serde-json" ,rust-serde-json-1.0)
        ("rust-serde-urlencoded" ,rust-serde-urlencoded-0.5)
        ("rust-time" ,rust-time-0.1)
        ("rust-tokio" ,rust-tokio-0.1)
        ("rust-tokio-executor" ,rust-tokio-executor-0.1)
        ("rust-tokio-io" ,rust-tokio-io-0.1)
        ("rust-tokio-threadpool" ,rust-tokio-threadpool-0.1)
        ("rust-tokio-timer" ,rust-tokio-timer-0.2)
        ("rust-url" ,rust-url-1.2)
        ("rust-uuid" ,rust-uuid-0.7)
        ("rust-winreg" ,rust-winreg-0.6)
        ("rust-hyper-old-types" ,rust-hyper-old-types-0.11)
        ("rust-hyper-rustls" ,rust-hyper-rustls-0.17)
        ("rust-hyper-tls" ,rust-hyper-tls-0.3)
        ("rust-native-tls" ,rust-native-tls-0.2)
        ("rust-rustls" ,rust-rustls-0.16)
        ("rust-socks" ,rust-socks-0.3)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.10)
        ("rust-trust-dns-resolver" ,rust-trust-dns-resolver-0.11)
        ("rust-webpki-roots" ,rust-webpki-roots-0.17))
       #:cargo-development-inputs
       (("rust-bytes" ,rust-bytes-0.4)
        ("rust-doc-comment" ,rust-doc-comment-0.3)
        ("rust-env-logger" ,rust-env-logger-0.6)
        ("rust-libflate" ,rust-libflate-0.1)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-tokio" ,rust-tokio-0.1)
        ("rust-tokio-tcp" ,rust-tokio-tcp-0.1))))))

(define-public rust-rgb-0.8
  (package
    (name "rust-rgb")
    (version "0.8.14")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rgb" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1nzg8809n0p7g3giq3ca8wi77kcpzv1cihzq07i2kl8l281y9290"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-serde" ,rust-serde-1.0))
       #:cargo-development-inputs
       (("rust-serde-json" ,rust-serde-json-1.0))
       #:tests? #f)) ; internal::ops::test::test_add_overflow fails
    (home-page "https://lib.rs/crates/rgb")
    (synopsis
     "`struct RGB/RGBA/etc.` for sharing pixels between crates + convenience methods for color manipulation.
      Allows no-copy high-level interoperability. Also adds common convenience methods and implements standard Rust traits to make `RGB`/`RGBA` pixels and slices first-class Rust objects.")
    (description
     "`struct RGB/RGBA/etc.` for sharing pixels between crates + convenience methods for color manipulation.
            Allows no-copy high-level interoperability.  Also adds common convenience methods and implements standard Rust traits to make `RGB`/`RGBA` pixels and slices first-class Rust objects.")
    (license license:expat)))

(define-public rust-ring-0.16
  (package
    (name "rust-ring")
    (version "0.16.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ring" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0mzpk6zmvlvzy021fh8b3xs2zl6c8mqdqfwqn7zlvc07g8qyhskr"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cc" ,rust-cc)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-spin" ,rust-spin)
        ("rust-untrusted" ,rust-untrusted)
        ("rust-web-sys" ,rust-web-sys)
        ("rust-winapi" ,rust-winapi-0.3)
        ("rust-lazy-static" ,rust-lazy-static-1))
       #:cargo-development-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-wasm-bindgen-test" ,rust-wasm-bindgen-test-0.2))))
    (home-page "https://github.com/briansmith/ring")
    (synopsis
     "Safe, fast, small crypto using Rust")
    (description
     "Safe, fast, small crypto using Rust")
    (license (license:non-copyleft "file://LICENSE"))))

(define-public rust-ron-0.5
  (package
    (inherit rust-ron-0.4)
    (name "rust-ron")
    (version "0.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "ron" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1mb2bavvp8jg5wx0kx9n45anrsbjwhjzddim987bjaa11hg45kif"))))
    (arguments
     `(#:cargo-inputs
       (("rust-base64" ,rust-base64-0.10)
        ("rust-bitflags" ,rust-bitflags)
        ("rust-serde" ,rust-serde-1.0))
       #:cargo-development-inputs
       (("rust-serde-bytes" ,rust-serde-bytes-0.11)
        ("rust-serde-json" ,rust-serde-json-1.0))))))

(define-public rust-rspec-1.0
  (package
    (name "rust-rspec")
    (version "1.0.0-beta.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rspec" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1abfzwkbxlwahb243k8d3fp6i135lx1aqmbfl79w9zlpng182ndk"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-colored" ,rust-colored-1.8)
        ("rust-derive-new" ,rust-derive-new)
        ("rust-derive-builder" ,rust-derive-builder-0.5)
        ("rust-expectest" ,rust-expectest-0.9)
        ("rust-rayon" ,rust-rayon-0.8))
       #:cargo-development-inputs
       (("rust-clippy" ,rust-clippy-0.0))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'ddont-require-rust-clippy
           (lambda _
             (substitute* "Cargo.toml"
               (("0.0.153") "0.0"))
             #t)))))
    (home-page "https://mackwic.github.io/rspec")
    (synopsis
     "Write Rspec-like tests with stable rust")
    (description
     "Write Rspec-like tests with stable rust")
    (license license:mpl2.0)))

(define-public rust-rspec-1.0.0-beta3
  (package
    (inherit rust-rspec-1.0)
    (name "rust-rspec")
    (version "1.0.0-beta.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rspec" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1qkafvyg3r3h4ffhb7bhzq54mxxbirn2hk693wxdv5zhdjx68a99"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-clippy" ,rust-clippy-0.0))))))

(define-public rust-rustls-0.16
  (package
    (name "rust-rustls")
    (version "0.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "rustls" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "17n0fx3fpkg4fhpdplrdhkissnl003kj90vzbqag11vkpyqihnmj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-base64" ,rust-base64-0.10)
        ("rust-log" ,rust-log-0.4)
        ("rust-ring" ,rust-ring-0.16)
        ("rust-sct" ,rust-sct-0.6)
        ("rust-webpki" ,rust-webpki))
       #:cargo-development-inputs
       (("rust-criterion" ,rust-criterion-0.2)
        ("rust-env-logger" ,rust-env-logger-0.6)
        ("rust-log" ,rust-log-0.4)
        ("rust-tempfile" ,rust-tempfile-3.1)
        ("rust-webpki-roots" ,rust-webpki-roots))))
    (home-page "https://github.com/ctz/rustls")
    (synopsis
     "Rustls is a modern TLS library written in Rust.")
    (description
     "Rustls is a modern TLS library written in Rust.")
    (license (list license:asl2.0
                   license:isc
                   license:expat))))

(define-public rust-same-file-0.1
  (package
    (inherit rust-same-file-1.0)
    (name "rust-same-file")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "same-file" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "19qpl6j8s3ph9jm8rh1k0wp2nkyw5ah34xly00vqcfx4v97s8cfr"))))
    (arguments
     `(#:cargo-inputs
       (("rust-kernel32-sys" ,rust-kernel32-sys-0.2)
        ("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs
       (("rust-rand" ,rust-rand-0.3))))))

(define-public rust-sct-0.6
  (package
    (name "rust-sct")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sct" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0g4dz7las43kcpi9vqv9c6l1afjkdv3g3w3s7d2w7a7w77wjl173"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-ring" ,rust-ring-0.16)
        ("rust-untrusted" ,rust-untrusted))))
    (home-page "https://github.com/ctz/sct.rs")
    (synopsis
     "Certificate transparency SCT verification library")
    (description
     "Certificate transparency SCT verification library")
    (license (list license:asl2.0
                   license:isc
                   license:expat))))

(define-public rust-security-framework-0.3
  (package
    (name "rust-security-framework")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "security-framework" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1hmdsdj061wk76g3fajbfjnw74p0q45hy8hfngp7diwy987kvrpf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-core-foundation" ,rust-core-foundation-0.6)
        ("rust-core-foundation-sys"
         ,rust-core-foundation-sys)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-security-framework-sys"
         ,rust-security-framework-sys))
       #:cargo-development-inputs
       (("rust-hex" ,rust-hex-0.3)
        ("rust-tempdir" ,rust-tempdir-0.3))))
    (home-page
     "https://lib.rs/crates/security_framework")
    (synopsis
     "Security.framework bindings for macOS and iOS")
    (description
     "Security.framework bindings for macOS and iOS")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-select-rustc-0.1
  (package
    (name "rust-select-rustc")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "select-rustc" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0daqd56smi93g59nz43n4mh3d8whr6j5pa8dmwlf8bd76mdy3cpx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2)
        ("rust-quote" ,rust-quote)
        ("rust-syn" ,rust-syn-0.15))))
    (home-page
     "https://github.com/dtolnay/rustversion")
    (synopsis
     "Conditional compilation according to rustc compiler version")
    (description
     "Conditional compilation according to rustc compiler version")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-serde-0.9
  (package
    (inherit rust-serde-1.0)
    (name "rust-serde")
    (version "0.9.15")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1bsla8l5xr9pp5sirkal6mngxcq6q961km88jvf339j5ff8j7dil"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-serde-derive" ,rust-serde-derive-0.9))
       #:cargo-development-inputs
       (("rust-serde-derive" ,rust-serde-derive-0.9))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-Cargo-toml
           (lambda _
             (substitute* "Cargo.toml"
               ((", path =.*}") " }"))
             #t)))))))

(define-public rust-serde-codegen-internals-0.14
  (package
    (name "rust-serde-codegen-internals")
    (version "0.14.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde-codegen-internals" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0004s3wlc85vi6hq62hq84cv5b6qbbin1n6hdaqj095xhg98p25w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-syn" ,rust-syn-0.11))))
    (home-page "https://serde.rs")
    (synopsis
     "AST representation used by Serde codegen. Unstable.")
    (description
     "AST representation used by Serde codegen.  Unstable.")
    (license #f)))

(define-public rust-serde-derive-0.9
  (package
    (inherit rust-serde-derive-1.0)
    (name "rust-serde-derive")
    (version "0.9.15")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde-derive" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1fkldf0lnl6pwxs00qpyp79m30qmfpi3bk0wm22211ylyikdi3wp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-quote" ,rust-quote-0.3)
        ("rust-serde-codegen-internals"
         ,rust-serde-codegen-internals-0.14)
        ("rust-syn" ,rust-syn-0.15))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-Cargo-toml
           (lambda _
             (substitute* "Cargo.toml"
               ((", path =.*}") " }"))
             #t)))))))

(define-public rust-serde-json-0.9
  (package
    (inherit rust-serde-json-1.0)
    (name "rust-serde-json")
    (version "0.9.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde-json" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "188nbf56m7p6mnh3xd71rwqxd4g95lqh8gsl7mfy3lp7gd4cz2xd"))))
    (arguments
     `(#:cargo-inputs
       (("rust-dtoa" ,rust-dtoa)
        ("rust-itoa" ,rust-itoa-0.3)
        ("rust-linked-hash-map" ,rust-linked-hash-map-0.4)
        ("rust-num-traits" ,rust-num-traits-0.1)
        ("rust-serde" ,rust-serde-0.9))
       #:cargo-development-inputs
       (("rust-serde-derive" ,rust-serde-derive-0.9))))))

(define-public rust-serde-json-0.8
  (package
    (inherit rust-serde-json-1.0)
    (name "rust-serde-json")
    (version "0.8.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_json" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0k3bclzbvzhiscjydqzym887i8mkh726xkf8isf3lln3xplx5xv7"))))
    (arguments
     `(#:cargo-inputs
       (("rust-dtoa" ,rust-dtoa-0.2)
        ("rust-itoa" ,rust-itoa-0.1)
        ("rust-num-traits" ,rust-num-traits-0.1)
        ("rust-serde" ,rust-serde-0.8)
        ("rust-clippy" ,rust-clippy-0.0)
        ("rust-linked-hash-map" ,rust-linked-hash-map-0.3))))))

(define-public rust-serde-stacker-0.1
  (package
    (name "rust-serde-stacker")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_stacker" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1jn54i5m1mlc6nm47f96k85fgjs9mhpbbqa4dvd5xjbivkdw55ic"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-serde" ,rust-serde-1.0)
        ("rust-stacker" ,rust-stacker))
       #:cargo-development-inputs
       (("rust-serde-json" ,rust-serde-json-1.0))))
    (home-page
     "https://github.com/dtolnay/serde-stacker")
    (synopsis
     "Serde adapter that avoids stack overflow by dynamically growing the stack")
    (description
     "Serde adapter that avoids stack overflow by dynamically growing the stack")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-serde-test-0.9
  (package
    (inherit rust-serde-test-1.0)
    (name "rust-serde-test")
    (version "0.9.15")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde-test" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "193mf0qkhvjywd06x6hhmkixlqcyfbpfwfmr75dp2b8xwzpsvxwf"))))
    (arguments
     `(#:cargo-inputs
       (("rust-serde" ,rust-serde-0.9))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-Cargo-toml
           (lambda _
             (substitute* "Cargo.toml"
               ((", path =.*}") " }"))
             #t)))))))

(define-public rust-serde-urlencoded-0.6
  (package
    (name "rust-serde-urlencoded")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_urlencoded" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "15rcwfkff0md5i231m2ym5756ksw1mkh5b5g2rw72wsc5mzdgicy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-dtoa" ,rust-dtoa)
        ("rust-itoa" ,rust-itoa)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-url" ,rust-url-2.1))
       #:cargo-development-inputs
       (("rust-serde-derive" ,rust-serde-derive-1.0))))
    (home-page "https://serde.rs")
    (synopsis
     "`x-www-form-urlencoded` meets Serde")
    (description
     "`x-www-form-urlencoded` meets Serde")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-serde-urlencoded-0.5
  (package
    (inherit rust-serde-urlencoded-0.6)
    (name "rust-serde-urlencoded")
    (version "0.5.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "serde_urlencoded" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0nhnzllx5xrij4x17g351n14md691r95mxr7sbpz4sl80n8xcbb4"))))
    (arguments
     `(#:cargo-inputs
       (("rust-dtoa" ,rust-dtoa)
        ("rust-itoa" ,rust-itoa)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-url" ,rust-url-1.7))
       #:cargo-development-inputs
       (("rust-serde-derive" ,rust-serde-derive-1.0))))))

(define-public rust-simd-0.2
  (package
    (name "rust-simd")
    (version "0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "simd" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1dgpmfzd4favsckd5m0p6bna1dcgw19hjigkqcgwfhc4d05hxczj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-serde" ,rust-serde-1.0)
        ("rust-serde-derive" ,rust-serde-derive-1.0))
       #:cargo-development-inputs
       (("rust-cfg-if" ,rust-cfg-if))
       #:skip-build? #t)) ; Unsupported as of rust-1.33 nightly
    (home-page "https://github.com/hsivonen/simd")
    (synopsis
     "`simd` offers limited cross-platform access to SIMD instructions on
      CPUs, as well as raw interfaces to platform-specific instructions.
      (To be obsoleted by the `std::simd` implementation RFC 2366.)
      ")
    (description
     "`simd` offers limited cross-platform access to SIMD instructions on
        CPUs, as well as raw interfaces to platform-specific instructions.
        (To be obsoleted by the `std::simd` implementation RFC 2366.)
        ")
    (license #f)))

(define-public rust-siphasher-0.3
  (package
    (inherit rust-siphasher-0.2)
    (name "rust-siphasher")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "siphasher" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "17cj2ynbv5zs7fa8ylrw7a6pb260q53ccj091mj9xa6ix0745nl3"))))))

(define-public rust-skeptic
  (package
    (name "rust-skeptic")
    (version "0.13.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "skeptic" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0rai61hbs65nbvbhqlk1nap5hlav5qx3zmjjjzh9rhgxagc8xyyn"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bytecount" ,rust-bytecount-0.4)
        ("rust-cargo-metadata" ,rust-cargo-metadata-0.6)
        ("rust-error-chain" ,rust-error-chain-0.12)
        ("rust-glob" ,rust-glob)
        ("rust-pulldown-cmark" ,rust-pulldown-cmark-0.2)
        ("rust-serde-json" ,rust-serde-json-1.0)
        ("rust-tempdir" ,rust-tempdir-0.3)
        ("rust-walkdir" ,rust-walkdir))
       #:cargo-development-inputs
       (("rust-unindent" ,rust-unindent))))
    (home-page
     "https://github.com/budziq/rust-skeptic")
    (synopsis
     "Test your Rust markdown documentation via Cargo")
    (description
     "Test your Rust markdown documentation via Cargo")
    (license #f)))

(define-public rust-socks-0.3
  (package
    (name "rust-socks")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "socks" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1hnbw4c4j7dn9n3bd1v7ddkdzlxlzkfw3z29da1nxlj6jgx4r9p6"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-byteorder" ,rust-byteorder-1.3)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-winapi" ,rust-winapi-0.3)
        ("rust-ws2-32-sys" ,rust-ws2-32-sys))))
    (home-page "https://github.com/sfackler/rust-socks")
    (synopsis
     "SOCKS proxy support for Rust.")
    (description
     "SOCKS proxy support for Rust.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-spmc-0.3
  (package
    (name "rust-spmc")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "spmc" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1rgcqgj6b3d0cshi7277akr2xk0cx11rkmviaahy7a3pla6l5a02"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-development-inputs
       (("rust-loom" ,rust-loom))))
    (home-page
     "https://github.com/seanmonstar/spmc")
    (synopsis
     "Simple SPMC channel")
    (description
     "Simple SPMC channel")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-starship-module-config-derive-0.1
  (package
    (name "rust-starship-module-config-derive")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri
             "starship-module-config-derive"
             version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0knhif0b71f7akql1ndr87ndw9vb4q8015xbi381blbs8vwcs74x"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2-1.0)
        ("rust-quote" ,rust-quote-1.0)
        ("rust-syn" ,rust-syn-1.0))))
    (home-page "https://starship.rs")
    (synopsis
     "The cross-shell prompt for astronauts. â\x98\x84ð\x9f\x8c\x8cï¸\x8f")
    (description
     "The cross-shell prompt for astronauts.  â\x98\x84ð\x9f\x8c\x8cï¸\x8f")
    (license license:isc)))

(define-public rust-string-0.2
  (package
    (name "rust-string")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "string" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0vaxz85ja52fn66akgvggb29wqa5bpj3y38syykpr1pbrjzi8hfj"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-bytes" ,rust-bytes-0.4))))
    (home-page
     "https://github.com/carllerche/string")
    (synopsis
     "A UTF-8 encoded string with configurable byte storage.")
    (description
     "This package provides a UTF-8 encoded string with configurable byte storage.")
    (license license:expat)))

(define-public rust-syn-0.11
  (package
    (inherit rust-syn-0.15)
    (name "rust-syn")
    (version "0.11.11")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "syn" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1b8x8jdsmj6r9ck7n1pg371526n1q90kx6rv6ivcb22w06wr3f6k"))))
    (arguments
     `(#:cargo-inputs
       (("rust-quote" ,rust-quote-0.3)
        ("rust-synom" ,rust-synom-0.11)
        ("rust-unicode-xid" ,rust-unicode-xid-0.0))
       #:cargo-development-inputs
       (("rust-syntex-pos" ,rust-syntex-pos)
        ("rust-syntex-syntax" ,rust-syntex-syntax)
        ("rust-tempdir" ,rust-tempdir-0.3)
        ("rust-walkdir" ,rust-walkdir-1))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-Cargo-toml
           (lambda _
             (substitute* "Cargo.toml"
               ((", path =.*,") ","))
             #t)))))))

(define-public rust-synom-0.11
  (package
    (name "rust-synom")
    (version "0.11.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "synom" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1dj536sh5xxhan2h0znxhv0sl6sb7lvzmsmrc3nvl3h1v5p0d4x3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-unicode-xid" ,rust-unicode-xid-0.0))
       #:cargo-development-inputs
       (("rust-syn" ,rust-syn-0.11))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-Cargo-toml
           (lambda _
             (substitute* "Cargo.toml"
               (("path =.*") ""))
             #t)))
       #:tests? #f)) ; TODO: Fix tests
    (home-page "https://github.com/dtolnay/syn")
    (synopsis "Stripped-down Nom parser used by Syn")
    (description
     "Stripped-down Nom parser used by Syn")
    (license #f)))

(define-public rust-synstructure-0.11
  (package
    (inherit rust-synstructure-0.10)
    (name "rust-synstructure")
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "synstructure" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1spqy31qcss57mciklc4nky4v778fvqs9qwdjgvnmf0hr5ichcca"))))
    (arguments
     `(#:cargo-inputs
       (("rust-proc-macro2" ,rust-proc-macro2)
        ("rust-quote" ,rust-quote)
        ("rust-syn" ,rust-syn-0.15)
        ("rust-unicode-xid" ,rust-unicode-xid-0.1))
       #:cargo-development-inputs
       (("rust-synstructure-test-traits"
         ,rust-synstructure-test-traits))))))

(define-public rust-syntex-errors-0.58
  (package
    (name "rust-syntex-errors")
    (version "0.58.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "syntex-errors" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "176vma7sjv6li17q7dsilryac66b76zyis9ampmff2hlsz1caz46"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc-0.2)
        ("rust-rustc-serialize" ,rust-rustc-serialize-0.3)
        ("rust-syntex-pos" ,rust-syntex-pos)
        ("rust-term" ,rust-term)
        ("rust-unicode-xid" ,rust-unicode-xid-0.0))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-Cargo-toml
           (lambda _
             (substitute* "Cargo.toml"
               ((", path =.*}") " }"))
             #t)))))
    (home-page "https://github.com/serde-rs/syntex")
    (synopsis "Backport of librustc_errors")
    (description "Backport of librustc_errors")
    (license #f)))

(define-public rust-syntex-syntax-0.58
  (package
    (name "rust-syntex-syntax")
    (version "0.58.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "syntex-syntax" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "14f74l7yzwl6fr9i23k4j23k66qn0gakvhk4jjc9ipb3w6x4s3kf"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bitflags" ,rust-bitflags-0.8)
        ("rust-log" ,rust-log-0.3)
        ("rust-rustc-serialize" ,rust-rustc-serialize-0.3)
        ("rust-syntex-errors" ,rust-syntex-errors)
        ("rust-syntex-pos" ,rust-syntex-pos)
        ("rust-unicode-xid" ,rust-unicode-xid-0.0))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-Cargo-toml
           (lambda _
             (substitute* "Cargo.toml"
               ((", path =.*}") " }"))
             #t)))))
    (home-page "https://github.com/serde-rs/syntex")
    (synopsis "Backport of libsyntax")
    (description "Backport of libsyntax")
    (license #f)))

(define-public rust-syntex-pos-0.58
  (package
    (name "rust-syntex-pos")
    (version "0.58.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "syntex-pos" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0iqhircpr723da1g97xrrj8smqqz3gxw91cf03sckasjzri4gb8k"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-rustc-serialize" ,rust-rustc-serialize-0.3))))
    (home-page "https://github.com/serde-rs/syntex")
    (synopsis "Backport of libsyntax_pos")
    (description "Backport of libsyntax_pos")
    (license #f)))

(define-public rust-sysinfo-0.10
  (package
    (name "rust-sysinfo")
    (version "0.10.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "sysinfo" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "17jdmwjmnwvkn2gi6fjd7lyg7irpk2vgn5mj71ilzbk1pr14x0xq"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if-0.1)
        ("rust-doc-comment" ,rust-doc-comment-0.3)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-ntapi" ,rust-ntapi-0.3)
        ("rust-once-cell" ,rust-once-cell-1.0)
        ("rust-rayon" ,rust-rayon-1.0)
        ("rust-winapi" ,rust-winapi-0.3))))
    (home-page "https://github.com/GuillaumeGomez/sysinfo")
    (synopsis "Library to handle processes")
    (description "Library to handle processes")
    (license license:expat)))

(define-public rust-takeable-option-0.5
  (package
    (inherit rust-takeable-option-0.4)
    (name "rust-takeable-option")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "takeable-option" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "182axkm8pq7cynsfn65ar817mmdhayrjmbl371yqp8zyzhr8kbin"))))))

(define-public rust-target-build-utils-0.3
  (package
    (name "rust-target-build-utils")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "target-build-utils" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0p7713x4bpbwi11l196z1mi8ym8qj1cdnab1mm2ffpm2wi516g81"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-phf" ,rust-phf-0.7)
        ("rust-serde-json" ,rust-serde-json-0.9)
        ("rust-phf-codegen" ,rust-phf-codegen-0.7))
       #:cargo-development-inputs
       (("rust-phf-codegen" ,rust-phf-codegen-0.7))))
    (home-page
     "https://github.com/nagisa/target_build_utils.rs")
    (synopsis
     "DEPRECATED: Use Cargo environment variables `CARGO_CFG_TARGET_*`")
    (description
     "DEPRECATED: Use Cargo environment variables `CARGO_CFG_TARGET_*`")
    (license #f)))

(define-public rust-target-lexicon-0.4
  (package
    (name "rust-target-lexicon")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "target-lexicon" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1jrdch22pvm8r9fwx6d051l4yhac16lq6sn4q5fc6ic95fcb82hv"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-failure" ,rust-failure-0.1)
        ("rust-failure-derive" ,rust-failure-derive-0.1)
        ("rust-serde-json" ,rust-serde-json-1.0))))
    (home-page "https://github.com/CraneStation/target-lexicon")
    (synopsis
     "This is a library for managing targets for compilers and related tools.")
    (description
     "This is a library for managing targets for compilers and related tools.")
    (license license:asl2.0))) ; with LLVM exception

(define-public rust-tokio-buf-0.2
  (package
    (name "rust-tokio-buf")
    (version "0.2.0-alpha.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tokio-buf" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1bz2yb77kxq4006j6cjdkl14n21pi0c0mdw20ywj9yd70y7lap2z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bytes" ,rust-bytes-0.4)
        ("rust-either" ,rust-either-1.5))
       #:cargo-development-inputs
       (("rust-tokio-mock-task" ,rust-tokio-mock-task))))
    (home-page "https://tokio.rs")
    (synopsis
     "Asynchronous stream of byte buffers")
    (description
     "Asynchronous stream of byte buffers")
    (license license:expat)))

(define-public rust-tokio-mockstream-1.1
  (package
    (name "rust-tokio-mockstream")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tokio-mockstream" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0mg1i39cl8x32wxwbn74hlirks8a6f3g0gfzkb0n0zwbxwvc9gs1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-futures" ,rust-futures-0.1)
        ("rust-tokio-io" ,rust-tokio-io-0.1))
       #:cargo-development-inputs
       (("rust-bytes" ,rust-bytes-0.4))))
    (home-page
     "https://github.com/aatxe/tokio-mockstream")
    (synopsis
     "A fake stream for testing network applications backed by buffers.")
    (description
     "A fake stream for testing network applications backed by buffers.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-tokio-openssl-0.4
  (package
    (name "rust-tokio-openssl")
    (version "0.4.0-alpha.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tokio-openssl" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0zzhb720bmjkcg5q53yp9mimx8frnbrk9il6rya16zc6pwmzbfw8"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-openssl" ,rust-openssl-0.10)
        ("rust-tokio-io" ,rust-tokio-io-0.1))
       #:cargo-development-inputs
       (("rust-futures-preview" ,rust-futures-preview-0.3)
        ("rust-tokio" ,rust-tokio-0.1))))
    (home-page
     "https://github.com/alexcrichton/tokio-openssl")
    (synopsis
     "An implementation of SSL streams for Tokio backed by OpenSSL")
    (description
     "An implementation of SSL streams for Tokio backed by OpenSSL")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-tokio-rustls-0.12
  (package
    (name "rust-tokio-rustls")
    (version "0.12.0-alpha.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tokio-rustls" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0vmjqdpvvwi5xga8lrp9pr29i7jd77zzlbbv4vi2mnsqxjafdcji"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-futures-core-preview"
         ,rust-futures-core-preview)
        ("rust-rustls" ,rust-rustls-0.16)
        ("rust-smallvec" ,rust-smallvec-0.6)
        ("rust-tokio-io" ,rust-tokio-io-0.1)
        ("rust-webpki" ,rust-webpki))
       #:cargo-development-inputs
       (("rust-futures-util-preview"
         ,rust-futures-util-preview-0.3)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-tokio" ,rust-tokio-0.1)
        ("rust-webpki-roots" ,rust-webpki-roots))))
    (home-page
     "https://github.com/quininer/tokio-rustls")
    (synopsis
     "Asynchronous TLS/SSL streams for Tokio using Rustls.")
    (description
     "Asynchronous TLS/SSL streams for Tokio using Rustls.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-tokio-tls-0.3
  (package
    (name "rust-tokio-tls")
    (version "0.3.0-alpha.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "tokio-tls" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1z9bbxkd646lsn1fr1a5znxdz8afbpy31iq1knxd424v57lxf29p"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-native-tls" ,rust-native-tls)
        ("rust-tokio-io" ,rust-tokio-io-0.1))))
    (home-page "https://tokio.rs")
    (synopsis
     "An implementation of TLS/SSL streams for Tokio giving an implementation of TLS for nonblocking I/O streams.")
    (description
     "An implementation of TLS/SSL streams for Tokio giving an implementation of TLS for nonblocking I/O streams.")
    (license license:expat)))

(define-public rust-toml-0.4
  (package
    (inherit rust-toml-0.5)
    (name "rust-toml")
    (version "0.4.10")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "toml" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "07qilkzinn8z13vq2sss65n2lza7wrmqpvkbclw919m3f7y691km"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-serde" ,rust-serde-1.0))
       #:cargo-development-inputs
       (("rust-serde-derive" ,rust-serde-derive-1.0)
        ("rust-serde-json" ,rust-serde-json-1.0))))))

(define-public rust-trust-dns-https-0.4
  (package
    (name "rust-trust-dns-https")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-https" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1ypkbgm5p7smjfkca3gaszhvknbr2ykf8skw8pyvpn0sq95lv5ia"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bytes" ,rust-bytes-0.4)
        ("rust-data-encoding" ,rust-data-encoding)
        ("rust-failure" ,rust-failure-0.1)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-h2" ,rust-h2-0.1)
        ("rust-http" ,rust-http-0.1)
        ("rust-log" ,rust-log-0.4)
        ("rust-rustls" ,rust-rustls-0.16)
        ("rust-tokio-executor" ,rust-tokio-executor-0.1)
        ("rust-tokio-reactor" ,rust-tokio-reactor-0.1)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.12)
        ("rust-tokio-tcp" ,rust-tokio-tcp-0.1)
        ("rust-trust-dns-proto" ,rust-trust-dns-proto)
        ("rust-trust-dns-rustls" ,rust-trust-dns-rustls)
        ("rust-tokio-executor" ,rust-tokio-executor-0.1)
        ("rust-tokio-reactor" ,rust-tokio-reactor-0.1)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.12)
        ("rust-tokio-tcp" ,rust-tokio-tcp-0.1)
        ("rust-trust-dns-proto" ,rust-trust-dns-proto)
        ("rust-trust-dns-rustls" ,rust-trust-dns-rustls)
        ("rust-typed-headers" ,rust-typed-headers)
        ("rust-webpki" ,rust-webpki)
        ("rust-webpki-roots" ,rust-webpki-roots))
       #:cargo-development-inputs
       (("rust-env-logger" ,rust-env-logger-0.6)
        ("rust-tokio" ,rust-tokio-0.1))))
    (home-page "http://trust-dns.org/index.html")
    (synopsis
     "Trust-DNS is a safe and secure DNS library. This is an extension for the Trust-DNS client to use DNS over HTTPS.")
    (description
     "Trust-DNS is a safe and secure DNS library.  This is an extension for the Trust-DNS client to use DNS over HTTPS.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-trust-dns-native-tls
  (package
    (name "rust-trust-dns-native-tls")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-native-tls" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0dkwfqxjjmbikm3mav71zjymgy8wmqr4mca64x49qzknvc4qwy6z"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-futures" ,rust-futures-0.1)
        ("rust-native-tls" ,rust-native-tls)
        ("rust-tokio-tcp" ,rust-tokio-tcp-0.1)
        ("rust-tokio-tls" ,rust-tokio-tls-0.3)
        ("rust-trust-dns-proto" ,rust-trust-dns-proto))
       #:cargo-development-inputs
       (("rust-tokio" ,rust-tokio-0.1))))
    (home-page "http://trust-dns.org/index.html")
    (synopsis
     "Trust-DNS is a safe and secure DNS library. This is an extension for the Trust-DNS client to use native-tls for TLS.")
    (description
     "Trust-DNS is a safe and secure DNS library.  This is an extension for the Trust-DNS client to use native-tls for TLS.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-trust-dns-openssl
  (package
    (name "rust-trust-dns-openssl")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-openssl" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "19qxi4y33wd2g55r4v9d6b06d20bdhqhvsrsmbpz5ir3i7l5psp7"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-futures" ,rust-futures-0.1)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-tokio-openssl" ,rust-tokio-openssl-0.4)
        ("rust-tokio-tcp" ,rust-tokio-tcp-0.1)
        ("rust-trust-dns-proto" ,rust-trust-dns-proto))
       #:cargo-development-inputs
       (("rust-openssl" ,rust-openssl-0.10)
        ("rust-tokio" ,rust-tokio-0.1))))
    (home-page "http://trust-dns.org/index.html")
    (synopsis
     "Trust-DNS is a safe and secure DNS library. This is an extension for the Trust-DNS client to use tokio-openssl for TLS.")
    (description
     "Trust-DNS is a safe and secure DNS library.  This is an extension for the Trust-DNS client to use tokio-openssl for TLS.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-trust-dns-proto
  (package
    (name "rust-trust-dns-proto")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-proto" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1f9xjyz7fsa83dj00zif7lmljd4x420c0vmniinhb7c35777wi85"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-data-encoding" ,rust-data-encoding)
        ("rust-enum-as-inner" ,rust-enum-as-inner-0.2)
        ("rust-failure" ,rust-failure-0.1)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-idna" ,rust-idna-0.2)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-openssl" ,rust-openssl-0.10)
        ("rust-rand" ,rust-rand-0.6)
        ("rust-ring" ,rust-ring-0.16)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-smallvec" ,rust-smallvec-0.6)
        ("rust-socket2" ,rust-socket2)
        ("rust-tokio-executor" ,rust-tokio-executor-0.1)
        ("rust-tokio-io" ,rust-tokio-io-0.1)
        ("rust-tokio-reactor" ,rust-tokio-reactor-0.1)
        ("rust-tokio-tcp" ,rust-tokio-tcp-0.1)
        ("rust-tokio-timer" ,rust-tokio-timer-0.1)
        ("rust-tokio-udp" ,rust-tokio-udp-0.1)
        ("rust-url" ,rust-url-2.1))
       #:cargo-development-inputs
       (("rust-env-logger" ,rust-env-logger-0.6)
        ("rust-tokio" ,rust-tokio-0.1))))
    (home-page "http://trust-dns.org/index.html")
    (synopsis
     "Trust-DNS is a safe and secure DNS library. This is the foundational DNS protocol library for all Trust-DNS projects.")
    (description
     "Trust-DNS is a safe and secure DNS library.  This is the foundational DNS protocol library for all Trust-DNS projects.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-trust-dns-resolver
  (package
    (name "rust-trust-dns-resolver")
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-resolver" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0cjkz3rcisk7v354l5hqb3j5x9x389pjqd6da6h8skvqxr0kl6yb"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cfg-if" ,rust-cfg-if)
        ("rust-failure" ,rust-failure-0.1)
        ("rust-futures" ,rust-futures-0.1)
        ("rust-ipconfig" ,rust-ipconfig)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-log" ,rust-log-0.4)
        ("rust-lru-cache" ,rust-lru-cache)
        ("rust-resolv-conf" ,rust-resolv-conf)
        ("rust-rustls" ,rust-rustls-0.16)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-smallvec" ,rust-smallvec-0.6)
        ("rust-tokio" ,rust-tokio-0.1)
        ("rust-tokio-executor" ,rust-tokio-executor-0.1)
        ("rust-tokio-tcp" ,rust-tokio-tcp-0.1)
        ("rust-tokio-udp" ,rust-tokio-udp-0.1)
        ("rust-trust-dns-https" ,rust-trust-dns-https-0.4)
        ("rust-trust-dns-native-tls"
         ,rust-trust-dns-native-tls)
        ("rust-trust-dns-openssl"
         ,rust-trust-dns-openssl)
        ("rust-trust-dns-proto" ,rust-trust-dns-proto)
        ("rust-trust-dns-rustls" ,rust-trust-dns-rustls)
        ("rust-webpki-roots" ,rust-webpki-roots))
       #:cargo-development-inputs
       (("rust-env-logger" ,rust-env-logger-0.6)
        ("rust-tokio" ,rust-tokio-0.1)
        ("rust-tokio-io" ,rust-tokio-io-0.1))))
    (home-page "http://trust-dns.org/index.html")
    (synopsis
     "Trust-DNS is a safe and secure DNS library. This Resolver library  uses the Client library to perform all DNS queries. The Resolver is intended to be a high-level library for any DNS record resolution see Resolver and AsyncResolver for supported resolution types. The Client can be used for other queries.
      ")
    (description
     "Trust-DNS is a safe and secure DNS library.  This Resolver library  uses the Client library to perform all DNS queries.  The Resolver is intended to be a high-level library for any DNS record resolution see Resolver and AsyncResolver for supported resolution types.  The Client can be used for other queries.
           ")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-trust-dns-rustls
  (package
    (name "rust-trust-dns-rustls")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "trust-dns-rustls" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0glpggq31764q7lp19h5l6implsr7ik015qkm5rg7pqwy93krsb3"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-futures" ,rust-futures-0.1)
        ("rust-log" ,rust-log-0.4)
        ("rust-rustls" ,rust-rustls-0.16)
        ("rust-tokio-rustls" ,rust-tokio-rustls-0.12)
        ("rust-tokio-tcp" ,rust-tokio-tcp-0.1)
        ("rust-trust-dns-proto" ,rust-trust-dns-proto)
        ("rust-webpki" ,rust-webpki))
       #:cargo-development-inputs
       (("rust-openssl" ,rust-openssl-0.10)
        ("rust-tokio" ,rust-tokio-0.1))))
    (home-page "http://trust-dns.org/index.html")
    (synopsis
     "Trust-DNS is a safe and secure DNS library. This is an extension for the Trust-DNS client to use rustls for TLS.")
    (description
     "Trust-DNS is a safe and secure DNS library.  This is an extension for the Trust-DNS client to use rustls for TLS.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-typed-headers
  (package
    (name "rust-typed-headers")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "typed-headers" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0g40nlq5iw0zxhwb7nfmfbr9m86abgwwhxwhzrm10nfq6bsmlvxx"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-base64" ,rust-base64-0.10)
        ("rust-bytes" ,rust-bytes-0.4)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-http" ,rust-http-0.1)
        ("rust-mime" ,rust-mime))))
    (home-page "https://github.com/sfackler/typed-headers")
    (synopsis
     "Typed HTTP header serialization and deserialization.")
    (description
     "Typed HTTP header serialization and deserialization.")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-unicase-1.4
  (package
    (inherit rust-unicase-2.4)
    (name "rust-unicase")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "unicase" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0cwazh4qsmm9msckjk86zc1z35xg7hjxjykrgjalzdv367w6aivz"))))
    (arguments
     `(#:cargo-inputs
       (("rust-heapsize" ,rust-heapsize-0.3)
        ("rust-heapsize-plugin" ,rust-heapsize-plugin)
        ("rust-version-check" ,rust-version-check-0.1))
       #:cargo-development-inputs
       (("rust-version-check" ,rust-version-check-0.1))))))

(define-public rust-unicode-segmentation-1.6
  (package
    (inherit rust-unicode-segmentation-1.3)
    (name "rust-unicode-segmentation")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "unicode-segmentation" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1h7d48mzpi8hwf5cvnq07warkv86pvapzzzf32hvbjsk20yiagp8"))))
    (arguments
     `(#:cargo-development-inputs
       (("rust-quickcheck" ,rust-quickcheck-0.7))))))

(define-public rust-uom-0.26
  (package
    (name "rust-uom")
    (version "0.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "uom" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1i4dggf4nxji7z1slq0wql2n41c6h8b7k43h65v5bb7pqmp7kv2c"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-num-bigint" ,rust-num-bigint-0.2)
        ("rust-num-rational" ,rust-num-rational-0.2)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-typenum" ,rust-typenum-1.9))
       #:cargo-development-inputs
       (("rust-approx" ,rust-approx-0.3)
        ("rust-quickcheck" ,rust-quickcheck-0.8)
        ("rust-serde-json" ,rust-serde-json-1.0)
        ("rust-static-assertions" ,rust-static-assertions-0.3))))
    (home-page "https://github.com/iliekturtles/uom")
    (synopsis "Units of measurement")
    (description "Units of measurement")
    (license (list license:asl2.0 license:expat))))

(define-public rust-urlencoded
  (package
    (name "rust-urlencoded")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "urlencoded" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1qrkblcj3gpz256d5fci9g9ig20mxlavy25gj6p612qi740zalha"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-bodyparser" ,rust-bodyparser-0.8)
        ("rust-iron" ,rust-iron)
        ("rust-plugin" ,rust-plugin)
        ("rust-url" ,rust-url-2.1))))
    (home-page "https://github.com/iron/urlencoded")
    (synopsis
     "URL Encoded middleware for the Iron web framework. Decode URL Encoded data from GET request queries and POST request bodies.")
    (description
     "URL Encoded middleware for the Iron web framework. Decode URL Encoded data from GET request queries and POST request bodies.")
    (license license:expat)))

(define-public rust-urlencoding-1.0
  (package
    (name "rust-urlencoding")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "urlencoding" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1v9piz5dwrkiyk0w9cfr55y9rqnr90rw9r52wmblrfx854b5dwrx"))))
    (build-system cargo-build-system)
    (home-page "https://github.com/bt/rust_urlencoding")
    (synopsis
     "A Rust library for doing URL percentage encoding.")
    (description
     "This package provides a Rust library for doing URL percentage encoding.")
    (license license:expat)))

(define-public rust-utf8-ranges-0.1
  (package
    (inherit rust-utf8-ranges-1.0)
    (name "rust-utf8-ranges")
    (version "0.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "utf8-ranges" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "03xf604b2v51ag3jgzw92l97xnb10kw9zv948bhc7ja1ik017jm1"))))
    (arguments
     `(#:cargo-development-inputs
       (("rust-quickcheck" ,rust-quickcheck-0.2))
       #:tests? #f)))) ; Tests require 'unicode' create.

(define-public rust-walkdir-1
  (package
    (inherit rust-walkdir-2.2)
    (name "rust-walkdir")
    (version "1.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "walkdir" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1zw8safzqpsrvfn0256cngq2fr9d4lmwv5qb8ycn1f7sf3kgj25v"))))
    (arguments
     `(#:cargo-inputs
       (("rust-kernel32-sys" ,rust-kernel32-sys-0.2)
        ("rust-same-file" ,rust-same-file-0.1)
        ("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs
       (("rust-docopt" ,rust-docopt-0.7)
        ("rust-quickcheck" ,rust-quickcheck-0.4)
        ("rust-rand" ,rust-rand-0.6)
        ("rust-rustc-serialize" ,rust-rustc-serialize-0.3))))))

(define-public rust-want
  (package
    (name "rust-want")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "want" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "181b2zmwfq389x9n2g1n37cvcvvdand832zz6v8i1l8wrdlaks0w"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-log" ,rust-bytes-0.4)
        ("rust-try-lock" ,rust-try-lock-0.3))
       #:cargo-development-inputs
       (("rust-tokio-executor" ,rust-tokio-executor-0.1)
        ("rust-tokio-sync" ,rust-tokio-sync-0.1))))
    (home-page "https://github.com/seanmonstar/want")
    (synopsis
     "Detect when another Future wants a result.")
    (description
     "Detect when another Future wants a result.")
    (license license:expat)))

(define-public rust-wasm-bindgen-webidl
  (package
    (name "rust-wasm-bindgen-webidl")
    (version "0.2.50")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "wasm-bindgen-webidl" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "057zak44nyrawipgi37m451fjkxz6ix5rzcw11d699rgpy4x4lxy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-failure" ,rust-failure-0.1)
        ("rust-heck" ,rust-heck-0.3)
        ("rust-log" ,rust-log-0.4)
        ("rust-proc-macro2" ,rust-proc-macro2)
        ("rust-quote" ,rust-quote)
        ("rust-syn" ,rust-syn-0.15)
        ("rust-wasm-bindgen-backend" ,rust-wasm-bindgen-backend-0.2)
        ("rust-weedle" ,rust-weedle))))
    (home-page
     "https://rustwasm.github.io/wasm-bindgen/")
    (synopsis
     "Support for parsing WebIDL specific to wasm-bindgen")
    (description
     "Support for parsing WebIDL specific to wasm-bindgen")
    (license (list license:asl2.0
                   license:expat))))

(define-public rust-webpki
  (package
    (name "rust-webpki")
    (version "0.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "webpki" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1d41gfai89q7drm92mgmh6fk57nikv2vqsa773i100dcf3kn9rnp"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-ring" ,rust-ring-0.16)
        ("rust-untrusted" ,rust-untrusted))
       #:cargo-development-inputs
       (("rust-base64" ,rust-base64-0.10))))
    (home-page "https://github.com/briansmith/webpki")
    (synopsis
     "webpki is a library that validates Web PKI (TLS/SSL) certificates. webpki is designed to provide a full implementation of the client side of the Web PKI to a diverse range of applications and devices, including embedded (IoT) applications, mobile apps, desktop applications, and server infrastructure. webpki is intended to not only be the best implementation of the Web PKI, but to also precisely define what the Web PKI is.")
    (description
     "webpki is a library that validates Web PKI (TLS/SSL) certificates. webpki is designed to provide a full implementation of the client side of the Web PKI to a diverse range of applications and devices, including embedded (IoT) applications, mobile apps, desktop applications, and server infrastructure. webpki is intended to not only be the best implementation of the Web PKI, but to also precisely define what the Web PKI is.")
    (license license:isc))) ; I think

(define-public rust-webpki-roots
  (package
    (name "rust-webpki-roots")
    (version "0.17.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "webpki-roots" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "12vi8dh0yik0h4f0b9dnlw5i3gxyky7iblbksh6zcq4xvlvswqm2"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-webpki" ,rust-webpki))))
    (home-page "https://github.com/ctz/webpki-roots")
    (synopsis
     "Mozilla's CA root certificates for use with webpki")
    (description
     "Mozilla's CA root certificates for use with webpki")
    (license license:mpl2.0)))

(define-public rust-which-1
  (package
    (inherit rust-which-2.0)
    (name "rust-which")
    (version "1.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "which" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1cjwa57kzfgzs681a27m5pjmq580pv3hkcg23smf270bgqz60jp8"))))
    (arguments
     `(#:cargo-inputs
       (("rust-libc" ,rust-libc-0.2))
       #:cargo-development-inputs
       (("rust-tempdir" ,rust-tempdir-0.3))
       #:tests? #f)))) ; TODO: Fix tests

(define-public rust-winconsole
  (package
    (name "rust-winconsole")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "winconsole" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0cjhrzs6djbi7sv37yl27plkqrp7y7bncrh5h3cjvdqds6b4py1y"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-cgmath" ,rust-cgmath-0.16)
        ("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-rgb" ,rust-rgb-0.8)
        ("rust-serde" ,rust-serde-1.0)
        ("rust-winapi" ,rust-winapi-0.3))
       #:cargo-development-inputs
       (("rust-serde-cbor" ,rust-serde-cbor-0.10)
        ("rust-serde-json" ,rust-serde-json-1.0))))
    (home-page
     "https://github.com/omarkmu/winconsole")
    (synopsis
     "A wrapper for console-related functions in the Windows API.")
    (description
     "This package provides a wrapper for console-related functions in the Windows API.")
    (license (list license:expat license:asl2.0))))

(define-public rust-winreg
  (package
    (name "rust-winreg")
    (version "0.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "winreg" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1jdcqr6zmvwyrp87h48miasfdvv16gjsb60rc8dy2kqwb3mnv65j"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-winapi" ,rust-winapi-0.3)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-serde" ,rust-serde-1.0))
       #:cargo-development-inputs
       (("rust-rand" ,rust-rand-0.3)
        ("rust-serde-derive" ,rust-serde-derive-1.0))))
    (home-page "https://github.com/gentoo90/winreg-rs")
    (synopsis
     "Rust bindings to MS Windows Registry API.")
    (description
     "Rust bindings to MS Windows Registry API.")
    (license license:expat)))

(define-public rust-x11-dl
  (package
    (name "rust-x11-dl")
    (version "2.18.4")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "x11-dl" version))
       (file-name
        (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0n1w837xagxqgwx2880d7c9ks6l3g1kk00yd75afdaiv58sf2rdy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-lazy-static" ,rust-lazy-static-1)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-maybe-uninit" ,rust-maybe-uninit)
        ("rust-pkg-config" ,rust-pkg-config-0.3))
       #:cargo-development-inputs
       (("rust-pkg-config" ,rust-pkg-config-0.3))))
    (inputs
     `(("pkg-config" ,rust-pkg-config-0.3)))
    (home-page
     "https://github.com/erlepereira/x11-rs.git")
    (synopsis "X11 library bindings for Rust")
    (description "X11 library bindings for Rust")
    (license license:cc0)))

(define-public rust-xz2-0.1
  (package
    (name "rust-xz2")
    (version "0.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (crate-uri "xz2" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0v4jb0193gx8s1kvd2ajsgh0ffmwhqhfmrrw1n1h2z7w6jgqcyf1"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs
       (("rust-futures" ,rust-futures-0.1)
        ("rust-lzma-sys" ,rust-lzma-sys-0.1)
        ("rust-tokio-io" ,rust-tokio-io-0.1))
       #:cargo-development-inputs
       (("rust-quickcheck" ,rust-quickcheck-0.7)
        ("rust-rand" ,rust-rand-0.5)
        ("rust-tokio-core" ,rust-tokio-core-0.1))))
    (inputs
     `(("pkg-config" ,pkg-config)
       ("xz" ,xz)))
    (home-page "https://github.com/alexcrichton/xz2-rs")
    (synopsis "Rust bindings to liblzma providing Read/Write streams")
    (description
     "Rust bindings to liblzma providing Read/Write streams as well as low-level
in-memory encoding/decoding.")
    (license (list license:asl2.0
                   license:expat))))
