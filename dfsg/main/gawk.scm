;;; Copyright © 2024 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main gawk)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages regex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml))

#;
(define-public gawk-full
  (package/inherit gawk
    (source
      (origin (inherit (package-source gawk))
              (snippet
               `(begin (delete-file "awkgram.c")
                       (delete-file "command.c")))))
    (native-inputs
     (modify-inputs (package-native-inputs gawk)
                    (prepend bison)))
    (inputs
     (modify-inputs (package-inputs gawk)
                    (prepend mpfr
                             readline)))
    (native-search-paths
      (list (search-path-specification
              (variable "AWKLIBPATH")
              (files '("lib/gawk")))
            (search-path-specification
              (variable "AWKPATH")
              (files '("share/awk")))))))

(define gawkextlib-commit "e59d9f0da83033ae6ebea8fc9e7da634fcd1bc68")
(define gawkextlib-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://git.code.sf.net/p/gawkextlib/code")
          (commit gawkextlib-commit)))
    (file-name "gawkextlib-source")
    (sha256
     (base32 "15b1f20v6cid4vq0xlqghrwai1ap08g3lhavl754zj04ns4c5bqk"))))

(define-public gawkextlib
  (package
    (name "gawkextlib")
    (version (git-version "1.0.4" "1" gawkextlib-commit))
    (source gawkextlib-source)
    (build-system gnu-build-system)
    (arguments
     (list
       #:configure-flags
       #~(list (string-append "--with-gawk=" #$(this-package-input "gawk")))
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda _
               (chdir "lib"))))))
    (native-inputs
     (list autoconf automake libtool))
    (inputs
     (list gawk))
    (home-page "https://sourceforge.net/projects/gawkextlib/")
    (synopsis "Helper functions for gawk extensions")
    (description "The libgawkextlib library contains the strhash API used by
various extensions, and the gawk_varinit API to make it easy to initialize
variables.")
    (license license:gpl3+)))

(define-public gawk-abort
  (package
    (name "gawk-abort")
    (version (git-version "1.0.1" "1" gawkextlib-commit))
    (source gawkextlib-source)
    (build-system gnu-build-system)
    (arguments
     (list
       #:configure-flags
       #~(list (string-append "--with-gawk=" #$(this-package-native-input "gawk")))
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda _
               (chdir "abort"))))))
    (native-inputs
     (list autoconf automake gawk gettext-minimal libtool))
    (home-page "https://sourceforge.net/projects/gawkextlib/")
    (synopsis "Abort function for gawk scripts")
    (description "This package provides a gawk extension library implementing
the abort function.")
    (license license:gpl3+)))

(define-public gawk-aregex
  (package
    (name "gawk-aregex")
    (version (git-version "1.1.0" "1" gawkextlib-commit))
    (source gawkextlib-source)
    (build-system gnu-build-system)
    (arguments
     (list
       #:configure-flags
       #~(list (string-append "--with-gawk="
                              #$(this-package-native-input "gawk"))
               (string-append "--with-gawkextlib="
                              #$(this-package-input "gawkextlib")))
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda _
               (chdir "aregex"))))))
    (native-inputs
     (list autoconf automake gawk gettext-minimal libtool))
    (inputs
     (list gawkextlib tre))
    (home-page "https://sourceforge.net/projects/gawkextlib/")
    (synopsis "Aregex API gawk scripts")
    (description "This package provides a gawk extension library implementing
the aregex API.")
    (license license:gpl3+)))

(define-public gawk-csv
  (package
    (name "gawk-csv")
    (version (git-version "1.0.0" "1" gawkextlib-commit))
    (source gawkextlib-source)
    (build-system gnu-build-system)
    (arguments
     (list
       #:configure-flags
       #~(list (string-append "--with-gawk="
                              #$(this-package-native-input "gawk"))
               (string-append "--with-gawkextlib="
                              #$(this-package-input "gawkextlib")))
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda _
               (chdir "csv")))
           (add-after 'chdir 'fix-typo
             (lambda _
               (substitute* "doc/Makefile.am"
                 (("/dev/nul ") "/dev/null ")))))))
    (native-inputs
     (list autoconf automake gawk gettext-minimal libtool texinfo))
    (inputs
     (list gawkextlib))
    (home-page "https://sourceforge.net/projects/gawkextlib/")
    (synopsis "CSV library for gawk scripts")
    (description "This package extends gawk with facilities to read, parse,
compose and write CSV records and files.")
    (license license:gpl3+)))

(define-public gawk-errno
  (package
    (name "gawk-errno")
    (version (git-version "1.1.1" "1" gawkextlib-commit))
    (source gawkextlib-source)
    (build-system gnu-build-system)
    (arguments
     (list
       #:configure-flags
       #~(list (string-append "--with-gawk="
                              #$(this-package-native-input "gawk")))
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda _
               (chdir "errno"))))))
    (native-inputs
     (list autoconf automake gawk gettext-minimal libtool))
    (home-page "https://sourceforge.net/projects/gawkextlib/")
    (synopsis "ERRNO library for gawk scripts")
    (description "This package provides a gawk extension library containing
some useful functions for working with ERRNO values.")
    (license license:gpl3+)))

(define-public gawk-gd
  (package
    (name "gawk-gd")
    (version (git-version "1.0.3" "1" gawkextlib-commit))
    (source gawkextlib-source)
    (build-system gnu-build-system)
    (arguments
     (list
       #:configure-flags
       #~(list (string-append "--with-gawk="
                              #$(this-package-native-input "gawk"))
               (string-append "--with-gawkextlib="
                              #$(this-package-input "gawkextlib")))
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda _
               (chdir "gd"))))))
    (native-inputs
     (list autoconf automake gawk gettext-minimal libtool texinfo))
    (inputs
     (list gawkextlib gd))
    (home-page "https://sourceforge.net/projects/gawkextlib/")
    (synopsis "Gd library for gawk scripts")
    (description "This package provides a gawk extension library for using the
gd graphics library.")
    (license license:gpl3+)))

(define-public gawk-haru
  (package
    (name "gawk-haru")
    (version (git-version "1.0.2" "1" gawkextlib-commit))
    (source gawkextlib-source)
    (build-system gnu-build-system)
    (arguments
     (list
       #:configure-flags
       #~(list (string-append "--with-gawk="
                              #$(this-package-native-input "gawk"))
               (string-append "--with-gawkextlib="
                              #$(this-package-input "gawkextlib")))
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda _
               (chdir "haru")))
           (add-after 'chdir 'adjust-source
             (lambda _
               ;; There seem to be some typos in some of the functions found by
               ;; the test suite.
               (substitute* '("pdf.h" "pdf.c")
                 (("HPDF_PROJECTING_SCUARE_END") "HPDF_PROJECTING_SQUARE_END")
                 (("HPDF_BYTE_TYPE_TRIAL") "HPDF_BYTE_TYPE_TRAIL")))))))
    (native-inputs
     (list autoconf automake gawk gettext-minimal libtool))
    (inputs
     (list gawkextlib libharu))
    (home-page "https://sourceforge.net/projects/gawkextlib/")
    (synopsis "Libharu library for gawk scripts")
    (description "This package provides a gawk extension library for using the
libharu pdf library.")
    (license license:gpl3+)))

(define-public gawk-json
  (package
    (name "gawk-json")
    (version (git-version "2.0.1" "1" gawkextlib-commit))
    (source gawkextlib-source)
    (build-system gnu-build-system)
    (arguments
     (list
       #:configure-flags
       #~(list (string-append "--with-gawk="
                              #$(this-package-native-input "gawk"))
               (string-append "--with-gawkextlib="
                              #$(this-package-input "gawkextlib")))
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda _
               (chdir "json"))))))
    (native-inputs
     (list autoconf automake gawk gettext-minimal libtool))
    (inputs
     (list gawkextlib rapidjson))
    (home-page "https://sourceforge.net/projects/gawkextlib/")
    (synopsis "Json library for gawk scripts")
    (description "This package provides a gawk extension library for using json.")
    (license license:gpl3+)))

(define-public gawk-lmdb
  (package
    (name "gawk-lmdb")
    (version (git-version "1.1.3" "1" gawkextlib-commit))
    (source gawkextlib-source)
    (build-system gnu-build-system)
    (arguments
     (list
       #:configure-flags
       #~(list (string-append "--with-gawk="
                              #$(this-package-native-input "gawk"))
               (string-append "--with-gawkextlib="
                              #$(this-package-input "gawkextlib")))
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda _
               (chdir "lmdb"))))))
    (native-inputs
     (list autoconf automake gawk gettext-minimal libtool))
    (inputs
     (list gawkextlib lmdb))
    (home-page "https://sourceforge.net/projects/gawkextlib/")
    (synopsis "Lmdb library for gawk scripts")
    (description "This package provides a gawk extension library for using the
@acronym{Lightning Memory-Mapped Database, LMDB} API.")
    (license license:gpl3+)))

(define-public gawk-mbs
  (package
    (name "gawk-mbs")
    (version (git-version "1.0.0" "1" gawkextlib-commit))
    (source gawkextlib-source)
    (build-system gnu-build-system)
    (arguments
     (list
       #:configure-flags
       #~(list (string-append "--with-gawk="
                              #$(this-package-native-input "gawk")))
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda _
               (chdir "mbs"))))))
    (native-inputs
     (list autoconf automake gawk gettext-minimal libtool))
    (home-page "https://sourceforge.net/projects/gawkextlib/")
    (synopsis "Multibyte string library for gawk scripts")
    (description "This package provides a gawk extension library implementing
the multibyte string API.")
    (license license:gpl3+)))

(define-public gawk-mpfr
  (package
    (name "gawk-mpfr")
    (version (git-version "1.1.0" "1" gawkextlib-commit))
    (source gawkextlib-source)
    (build-system gnu-build-system)
    (arguments
     (list
       #:configure-flags
       #~(list (string-append "--with-gawk="
                              #$(this-package-native-input "gawk"))
               (string-append "--with-gawkextlib="
                              #$(this-package-input "gawkextlib")))
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda _
               (chdir "mpfr"))))))
    (native-inputs
     (list autoconf automake gawk gettext-minimal libtool texinfo))
    (inputs
     (list gawkextlib gmp mpfr))
    (home-page "https://sourceforge.net/projects/gawkextlib/")
    (synopsis "Mpfr library for gawk scripts")
    (description "This package contains the gawk MPFR shared library extension
for accessing the MPFR library.")
    (license license:gpl3+)))

(define-public gawk-nl-langinfo
  (package
    (name "gawk-nl-langinfo")
    (version (git-version "1.1.0" "1" gawkextlib-commit))
    (source gawkextlib-source)
    (build-system gnu-build-system)
    (arguments
     (list
       #:configure-flags
       #~(list (string-append "--with-gawk="
                              #$(this-package-native-input "gawk")))
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda _
               (chdir "nl_langinfo"))))))
    (native-inputs
     (list autoconf automake gawk gettext-minimal libtool))
    (home-page "https://sourceforge.net/projects/gawkextlib/")
    (synopsis "Nl_langinfo library for gawk scripts")
    (description
     "This package contains the gawk nl_langinfo shared library extension.  It
provides access to the nl_langinfo(3) C library function.")
    (license license:gpl3+)))

(define-public gawk-pgsql
  (package
    (name "gawk-pgsql")
    (version "1.1.2")
    (source gawkextlib-source)
    (build-system gnu-build-system)
    (arguments
     (list
       #:configure-flags
       #~(list (string-append "--with-gawk="
                              #$(this-package-native-input "gawk"))
               (string-append "--with-gawkextlib="
                              #$(this-package-input "gawkextlib")))
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda _
               (chdir "pgsql"))))))
    (native-inputs
     (list autoconf automake gawk gettext-minimal libtool texinfo))
    (inputs
     (list gawkextlib postgresql))
    (home-page "https://sourceforge.net/projects/gawkextlib/")
    (synopsis "Postgresql library for gawk scripts")
    (description
     "This package provides a gawk extension library for accessing PostgreSQL
database servers using the libpq C library API.")
    (license license:gpl3+)))

(define-public gawk-reclen
  (package
    (name "gawk-reclen")
    (version "1.0.1")
    (source gawkextlib-source)
    (build-system gnu-build-system)
    (arguments
     (list
       #:configure-flags
       #~(list (string-append "--with-gawk="
                              #$(this-package-native-input "gawk")))
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda _
               (chdir "reclen"))))))
    (native-inputs
     (list autoconf automake gawk gettext-minimal libtool))
    (home-page "https://sourceforge.net/projects/gawkextlib/")
    (synopsis "Fixed-length record input parser for gawk")
    (description
     "This package provides a fixed-length record input parser for gawk.")
    (license license:gpl3+)))

(define-public gawk-redis
  (package
    (name "gawk-redis")
    (version (git-version "1.7.8" "1" gawkextlib-commit))
    (source gawkextlib-source)
    (build-system gnu-build-system)
    (arguments
     (list
       #:configure-flags
       #~(list (string-append "--with-gawk="
                              #$(this-package-native-input "gawk")))
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda _
               (chdir "redis"))))))
    (native-inputs
     (list autoconf automake gawk gettext-minimal libtool))
    (inputs
     (list hiredis))
    (home-page "https://sourceforge.net/projects/gawkextlib/")
    (synopsis "Redis library for gawk scripts")
    (description
     "This package contains an extension library for accessing Redis database
servers using the hiredis C library API.")
    (license license:gpl3+)))

(define-public gawk-select
  (package
    (name "gawk-select")
    (version (git-version "1.1.4" "1" gawkextlib-commit))
    (source gawkextlib-source)
    (build-system gnu-build-system)
    (arguments
     (list
       #:configure-flags
       #~(list (string-append "--with-gawk="
                              #$(this-package-native-input "gawk"))
               (string-append "--with-gawkextlib="
                              #$(this-package-input "gawkextlib")))
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda _
               (chdir "select"))))))
    (native-inputs
     (list autoconf automake gawk gettext-minimal libtool))
    (inputs
     (list gawkextlib))
    (home-page "https://sourceforge.net/projects/gawkextlib/")
    (synopsis "I/O multiplexing, non-blocking I/O, and signal trapping for gawk")
    (description
     "This package contains a gawk extension library supporting I/O
multiplexing and signal trapping.")
    (license license:gpl3+)))

(define-public gawk-xml
  (package
    (name "gawk-xml")
    (version (git-version "1.1.1" "1" gawkextlib-commit))
    (source gawkextlib-source)
    (build-system gnu-build-system)
    (arguments
     (list
       #:configure-flags
       #~(list (string-append "--with-gawk="
                              #$(this-package-native-input "gawk"))
               (string-append "--with-gawkextlib="
                              #$(this-package-input "gawkextlib")))
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda _
               (chdir "xml"))))))
    (native-inputs
     (list autoconf automake gawk gettext-minimal libtool texinfo))
    (inputs
     (list expat gawkextlib))
    (home-page "https://sourceforge.net/projects/gawkextlib/")
    (synopsis "XML library for gawk")
    (description
     "This package provides the gawk XML extension module, as well as the
xmlgawk script and some gawk include libraries for enhanced XML processing.")
    (license license:gpl3+)))
