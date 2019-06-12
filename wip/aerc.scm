;;; Copyright © 2019 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (wip aerc)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system go)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages check)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages man)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages syncthing)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages w3m))

(define-public aerc
  (package
    (name "aerc")
    (version "0.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://git.sr.ht/~sircmpwn/aerc")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1izfkh7wixbkyhw4l3hv4w6vfz2x0fz9s6cpq0y95ip15aca2sdh"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f ; no tests
       #:import-path "git.sr.ht/~sircmpwn/aerc"
       #:install-source? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'change-directory
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((getopt (assoc-ref inputs "git-sr-ht-sircmpwn-getopt"))
                   (pty    (assoc-ref inputs "git-sr-ht-sircmpwn-pty")))
               (copy-recursively getopt ".")
               (copy-recursively pty ".")
               (chdir "src/git.sr.ht/~sircmpwn/aerc")
               #t)))
         (replace 'build
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (invoke "make"
                       (string-append "PREFIX=" out)))))
         )
       ))
    (native-inputs
     `(
       ("python" ,python)
       ("scdoc" ,scdoc)
       ))
    (inputs
     `(
       ("dante" ,dante)
       ("git-sr-ht-sircmpwn-getopt" ,git-sr-ht-sircmpwn-getopt)
       ("git-sr-ht-sircmpwn-pty" ,git-sr-ht-sircmpwn-pty)
       ("go-github-com-ddevault-go-libvterm" ,go-github-com-ddevault-go-libvterm)
       ("go-github-com-danwakefield-fnmatch" ,go-github-com-danwakefield-fnmatch)
       ("go-github-com-emersion-go-imap" ,go-github-com-emersion-go-imap)
       ("go-github-com-emersion-go-imap-idle" ,go-github-com-emersion-go-imap-idle)
       ("go-github-com-emersion-go-message" ,go-github-com-emersion-go-message)
       ("go-github-com-emersion-go-sasl" ,go-github-com-emersion-go-sasl)
       ("go-github-com-emersion-go-smtp" ,go-github-com-emersion-go-smtp)
       ("go-github-com-emersion-go-textwrapper" ,go-github-com-emersion-go-textwrapper)
       ("go-github-com-gdamore-encoding" ,go-github-com-gdamore-encoding)
       ("go-github-com-gdamore-tcell" ,go-github-com-gdamore-tcell)
       ("go-github-com-go-ini-ini" ,go-github-com-go-ini-ini)
       ("go-github-com-google-shlex" ,go-github-com-google-shlex)
       ("go-github-com-kyoh-xdg" ,go-github-com-kyoh-xdg)
       ("go-github-com-lucasb-eyer-go-colorful" ,go-github-com-lucasb-eyer-go-colorful)
       ("go-github-com-martinlindhe-base36" ,go-github-com-martinlindhe-base36)
       ("go-github-com-mattn-go-isatty" ,go-github-com-mattn-go-isatty)
       ("go-github.com-mattn-go-runewidth" ,go-github.com-mattn-go-runewidth)
       ("go-github-com-miolini-datacounter" ,go-github-com-miolini-datacounter)
       ("go-github-com-mitchellh-go-homedir" ,go-github-com-mitchellh-go-homedir)
       ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
       ("go-github-com-riywo-loginshell" ,go-github-com-riywo-loginshell)
       ("python-colorama" ,python-colorama)
       ("w3m" ,w3m)
       ))
    (home-page "https://git.sr.ht/~sircmpwn/aerc")
    (synopsis "Email client for your terminal")
    (description "Aerc is an email client for your terminal.")
    (license license:expat)))

(define-public go-github-com-riywo-loginshell
  (let ((commit "c2f4167b23039c6289b5a488080176426ec5dad9")
        (revision "0"))
    (package
      (name "go-github-com-riywo-loginshell")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/riywo/loginshell")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "172igc6pavcbj8prjk72h4dyky1839m4i604h9ljy0sdfrikv6yi"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/riywo/loginshell"
         #:tests? #f)) ; SHELL cannot be determined in build environment
      (home-page "https://github.com/riywo/loginshell")
      (synopsis "Golang library to get the login shell of the current user")
      (description "Loginshell is a golang library to get the login shell of
the current user.")
      (license license:expat))))

(define-public go-github-com-miolini-datacounter
  (let ((commit "fd4e42a1d5e0d2714f16caf92f9c64215bf957ce")
        (revision "0"))
    (package
      (name "go-github-com-miolini-datacounter")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/miolini/datacounter")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "1456vxf625ih7vhwshzp8al48jh7zhffp0jxk7f40qy34dnbli56"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/miolini/datacounter"))
      (home-page "https://github.com/miolini/datacounter")
      (synopsis "Golang counters for readers/writers")
      (description "Datacounter provides golang counters for readers/writers.")
      (license license:expat))))

(define-public go-github-com-martinlindhe-base36
  (let ((commit "7c6542dfbb41505111dde1f321eb5d5002f5fb88")
        (revision "0"))
    (package
      (name "go-github-com-martinlindhe-base36")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/martinlindhe/base36")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0hib6cnx543fxbcy7fm3kcfz8ajhd90kxzbld49qi7w930pcz26c"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/martinlindhe/base36"))
      (inputs
       `(("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))
      (home-page "https://github.com/martinlindhe/base36")
      (synopsis "Base36 implementation in golang")
      (description "Base36 implements Base36 encoding and decoding, which is
useful to represent large integers in a case-insensitive alphanumeric way.")
      (license license:expat))))

(define-public go-github-com-kyoh-xdg
  (package
    (name "go-github-com-kyoh-xdg")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/kyoh86/xdg")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "02wrm9rl6qqv3nr2n5yariwg1rgr7w76l1sp0ak1kxin1y184ivf"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/kyoh86/xdg"))
    (home-page "https://github.com/kyoh86/xdg")
    (synopsis "Golang xdg helper functions")
    (description "Xdg is a collection of light weight helper functions in
golang to get config, data and cache files according to the XDG Base Directory
Specification.")
    (license license:expat)))

(define-public go-github-com-google-shlex
  (let ((commit "c34317bd91bf98fab745d77b03933cf8769299fe")
        (revision "0"))
    (package
      (name "go-github-com-google-shlex")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/google/shlex")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "00qivza4hkllfkar2vpqmyxybi0fwqipgijv61dgapq4xddxdq0r"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/google/shlex"))
      (synopsis "Go lexer")
      (description "Shlex is a simple lexer for go that supports shell-style
quoting, commenting, and escaping.")
      (home-page "https://github.com/google/shlex")
      (license license:asl2.0))))

(define-public go-github-com-go-ini-ini
  (package
    (name "go-github-com-go-ini-ini")
    (version "1.42.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/go-ini/ini")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "18ywm8zyv091j1pp5mvx8szl7928chk8lw02br6jy568d7rk4xal"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "gopkg.in/ini.v1"
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'copy-destination
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (src  (string-append out "/src/gopkg.in/ini.v1"))
                    (dest (string-append out "/src/github.com/go-ini/ini")))
               (copy-recursively src dest))
             #t)))))
    (inputs
     `(("go-github.com-smartystreets-goconvey" ,go-github.com-smartystreets-goconvey)))
    (home-page "https://ini.unknwon.io/")
    (synopsis "INI file read and write functionality in Go")
    (description "Package ini provides INI file read and write functionality in Go.
Features
@enumerate
@item Load from multiple data sources([]byte, file and io.ReadCloser) with overwrites.
@item Read with recursion values.
@item Read with parent-child sections.
@item Read with auto-increment key names.
@item Read with multiple-line values.
@item Read with tons of helper methods.
@item Read and convert values to Go types.
@item Read and WRITE comments of sections and keys.
@item Manipulate sections, keys and comments with ease.
@item Keep sections and keys in order as you parse and save.
@end enumerate")
    (license license:asl2.0)))

(define-public go-github-com-gdamore-tcell
  (package
    (name "go-github-com-gdamore-tcell")
    (version "1.1.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/gdamore/tcell")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0g2zfbgyk3djlk0qpmrgcyy0ba9ad932yswpaacswi21qyf9gwag"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/gdamore/tcell"
       #:phases
       (modify-phases %standard-phases
         (add-before 'reset-gzip-timestamps 'make-files-writable
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Make sure .gz files are writable so that the
             ;; 'reset-gzip-timestamps' phase can do its work.
             (let ((out (assoc-ref outputs "out")))
               (for-each make-file-writable
                         (find-files out "\\.gz$"))
               #t))))))
    (inputs
     `(("go-github-com-gdamore-encoding" ,go-github-com-gdamore-encoding)
       ("go-github-com-lucasb-eyer-go-colorful" ,go-github-com-lucasb-eyer-go-colorful)
       ("go-github.com-mattn-go-runewidth" ,go-github.com-mattn-go-runewidth)))
    (home-page "https://github.com/gdamore/tcell")
    (synopsis "Golang terminal package")
    (description "Tcell is a Go package that provides a cell based view for
text terminals, like xterm.  It was inspired by termbox, but includes many
additional improvements.")
    (license license:asl2.0)))

(define-public go-github-com-gdamore-encoding
  (package
    (name "go-github-com-gdamore-encoding")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/gdamore/encoding")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1vmm5zll92i2fm4ajqx0gyx0p9j36496x5nabi3y0x7h0inv0pk9"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/gdamore/encoding"))
    (propagated-inputs
     `(("go-golang-org-x-text-encoding" ,go-golang-org-x-text-encoding)
       ("go-golang-org-x-text-transform" ,go-golang-org-x-text-transform)))
    (home-page "https://github.com/gdamore/encoding")
    (synopsis "Various character map encodings")
    (description "Encoding provides a number of encodings that are missing from
the standard Go encoding package.")
    (license license:asl2.0)))

(define-public go-golang-org-x-text-encoding
  (let ((commit "e19ae1496984b1c655b8044a65c0300a3c878dd3")
        (revision "1"))
    (package
      (name "go-golang-org-x-text-encoding")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://go.googlesource.com/text")
                      (commit commit)))
                (file-name (string-append "go.googlesource.com-text-"
                                          version "-checkout"))
                (sha256
                 (base32
                  "1cvnnx8nwx5c7gr6ajs7sldhbqh52n7h6fsa3i21l2lhx6xrsh4w"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "golang.org/x/text/encoding"
         #:unpack-path "golang.org/x/text"))
      (synopsis "Interface for character encodings")
      (description "This package defines an interface for character encodings,
such as Shift JIS and Windows 1252, that can convert to and from UTF-8.")
      (home-page "https://go.googlesource.com/text")
      (license license:bsd-3))))

(define-public go-github-com-lucasb-eyer-go-colorful
  (package
    (name "go-github-com-lucasb-eyer-go-colorful")
    (version "1.0.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/lucasb-eyer/go-colorful")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0fig06880bvk1l92j4127v4x9sar4ds7ga8959gxxghb2w70b7l2"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/lucasb-eyer/go-colorful"))
    (inputs
     `(("go-github-com-data-dog-go-sqlmock" ,go-github-com-data-dog-go-sqlmock)))
    (home-page "https://github.com/lucasb-eyer/go-colorful")
    (synopsis "Library for using colors in Go")
    (description "Go-Colorful stores colors in RGB and provides methods from
converting these to various color-spaces.")
    (license license:expat)))

(define-public go-github-com-data-dog-go-sqlmock
  (package
    (name "go-github-com-data-dog-go-sqlmock")
    (version "1.3.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/DATA-DOG/go-sqlmock")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1xrly2vmy1mgj9dbkmivhh8gvq6v9f9xy2yp2dw54i1895zzs928"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/DATA-DOG/go-sqlmock"))
    (home-page "https://github.com/DATA-DOG/go-sqlmock")
    (synopsis "Sql mock driver for golang to test database interactions")
    (description "sqlmock is a mock library implementing sql/driver.  Which has
one and only purpose - to simulate any sql driver behavior in tests, without
needing a real database connection.  It helps to maintain correct TDD workflow.")
    (license license:bsd-3)))

(define-public go-github-com-emersion-go-smtp
  (package
    (name "go-github-com-emersion-go-smtp")
    (version "0.11.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/emersion/go-smtp/releases/"
                            "download/v" version "/go-smtp-" version ".tar.gz"))
        (sha256
         (base32
          "1jcrfljzpakiavcwdwffd312wmivw06b6n61zdfifv1cp94ca50n"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/emersion/go-smtp"))
    (inputs
     `(("go-github-com-emersion-go-sasl" ,go-github-com-emersion-go-sasl)))
    (home-page "https://github.com/emersion/go-smtp")
    (synopsis "SMTP client & server library written in Go")
    (description "Go-smtp is an ESMTP client and server library written in Go.
Features
@enumerate
@item ESMTP client & server implementing RFC 5321
@item Support for SMTP AUTH and PIPELINING
@item UTF-8 support for subject and message
@item LMTP support
@end enumerate")
    (license license:expat)))

(define-public go-github-com-emersion-go-sasl
  (let ((commit "47d4276003177822a0a7b78773d7a6ed51c6474d")
        (revision "1"))
    (package
      (name "go-github-com-emersion-go-sasl")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/emersion/go-sasl")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "071k3pjxaq331a26r7l1l609xd3jsxs53bb1805ba592brqwjlis"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/emersion/go-sasl"))
      (home-page "https://github.com/emersion/go-sasl")
      (synopsis "SASL library written in Go")
      (description "A SASL library written in Go.")
      (license license:expat))))

(define-public go-github-com-emersion-go-message
  (package
    (name "go-github-com-emersion-go-message")
    (version "0.10.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/emersion/go-message/releases/"
                            "download/v" version "/go-message-"
                            version ".tar.gz"))
        (sha256
         (base32
          "081w5rzhkm26w2rcyngixc114n1yga1m7qk11wf2wr792idcyszs"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/emersion/go-message"))
    (inputs
     `(("go-github-com-emersion-go-textwrapper" ,go-github-com-emersion-go-textwrapper)))
    (home-page "https://github.com/emersion/go-message")
    (synopsis "streaming Go library for the Internet Message Format and mail messages")
    (description "Go-message is a Go library for the Internet Message Format. It implements:
@enumerate
@item RFC 5322: Internet Message Format
@item RFC 2045, RFC 2046 and RFC 2047: Multipurpose Internet Mail Extensions
@item RFC 2183: Content-Disposition Header Field
@end eumerate
Features
@enumerate
@item Streaming API
@item Automatic encoding and charset handling
@item A mail subpackage to read and write mail messages
@item DKIM-friendly
@item A textproto subpackage that just implements the wire format
@end enumerate")
    (license license:expat)))

(define-public go-github-com-emersion-go-textwrapper
  (let ((commit "d0e65e56babe3f687ff94c1d764ca0e6aa7723ee")
        (revision "1"))
    (package
      (name "go-github-com-emersion-go-textwrapper")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/emersion/go-textwrapper")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "1nw8qpjjbpkz49wd19yg2qsln1dmdfxi83wp2aa819cv6xxf2y7l"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/emersion/go-textwrapper"))
      (home-page "https://github.com/emersion/go-textwrapper")
      (synopsis "Writer that wraps long text lines to a specified length")
      (description "A writer that wraps long text lines to a specified length")
      (license license:expat))))

(define-public go-github-com-emersion-go-imap
  (package
    (name "go-github-com-emersion-go-imap")
    (version "1.0.0-beta.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/emersion/go-imap")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "138405jzs35fz9j2iyczz744n46fzhpi2hwanal5jyjqraf1alhi"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/emersion/go-imap"))
    (propagated-inputs
     `(("go-golang-org-x-text-encoding" ,go-golang-org-x-text-encoding)
       ("go-golang-org-x-text-transform" ,go-golang-org-x-text-transform)))
    (home-page "https://github.com/emersion/go-imap")
    (synopsis "IMAP library for clients and servers")
    (description "An IMAP4rev1 library written in Go. It can be used to build a
client and/or a server.")
    (license license:expat)))

(define-public go-github-com-emersion-go-imap-idle
  (let ((commit "2704abd7050ed7f2143753554ee23affdf847bd9")
        (revision "1"))
    (package
      (name "go-github-com-emersion-go-imap-idle")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/emersion/go-imap-idle")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0blwcadmxgqsdwgr9m4jqfbpfa2viw5ah19xbybpa1z1z4aj5cbc"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/emersion/go-imap-idle"))
      (inputs
       `(("go-github-com-emersion-go-imap" ,go-github-com-emersion-go-imap)
         ("go-github-com-emersion-go-sasl" ,go-github-com-emersion-go-sasl)))
      (home-page "https://github.com/emersion/go-imap-idle")
      (synopsis "IDLE extension for go-imap")
      (description "go-imap-idle is an IDLE extension for go-imap.")
      (license license:expat))))

(define-public go-github-com-danwakefield-fnmatch
  (let ((commit "cbb64ac3d964b81592e64f957ad53df015803288")
        (revision "1"))
    (package
      (name "go-github-com-danwakefield-fnmatch")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/danwakefield/fnmatch")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0cbf511ppsa6hf59mdl7nbyn2b2n71y0bpkzbmfkdqjhanqh1lqz"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/danwakefield/fnmatch"))
      (home-page "https://github.com/danwakefield/fnmatch")
      (synopsis "Function match library in Go")
      (description "Fnmatch is a function match library written in Go.")
      (license license:bsd-2))))

(define-public go-github-com-ddevault-go-libvterm
  (let ((commit "b7d861da381071e5d3701e428528d1bfe276e78f")
        (revision "1"))
    (package
      (name "go-github-com-ddevault-go-libvterm")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/ddevault/go-libvterm")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "06vv4pgx0i6hjdjcar4ch18hp9g6q6687mbgkvs8ymmbacyhp7s6"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/ddevault/go-libvterm"))
      (propagated-inputs
       `(("go-github-com-mattn-go-pointer" ,go-github-com-mattn-go-pointer)))
      (home-page "https://github.com/ddevault/go-libvterm")
      (synopsis "Aerc fork of go-libvterm")
      (description "Go binding to libvterm.")
      (license license:expat))))

(define-public go-github-com-mattn-go-pointer
  (let ((commit "49522c3f37914a12a6813caf41f4a9e84d39ca0a")
        (revision "1"))
    (package
      (name "go-github-com-mattn-go-pointer")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/mattn/go-pointer")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "05xld3ykmj1fqsaqw3ixkjbkrp7alciyr041nm3y0y3k18cc41fm"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/mattn/go-pointer"))
      (home-page "https://github.com/mattn/go-pointer")
      (synopsis "CGo augmentor for go-1.6")
      (description "Go-pointer is a CGo augmentor for Go-1.6.")
      (license license:expat))))

(define-public git-sr-ht-sircmpwn-pty
  (let ((commit "3a43678975a9cb13dbcf05ea7a1611ebce3eadb2")
        (revision "1"))
    (package
      (name "git-sr-ht-sircmpwn-pty")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://git.sr.ht/~sircmpwn/pty")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "1spa779vb2j1vgwm1cpbmf7f8wj6zwyyy49ihz134izvah7vncsw"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "git.sr.ht/~sircmpwn/pty"))
      (home-page "https://git.sr.ht/~sircmpwn/pty")
      (synopsis "Go package for using unix pseudo-terminals")
      (description "Pty is a Go package for using unix pseudo-terminals.")
      (license license:expat))))

(define-public git-sr-ht-sircmpwn-getopt
  (let ((commit "fd226983a752f84b6a4a6c68f6bee62546d78a9b")
        (revision "1"))
    (package
      (name "git-sr-ht-sircmpwn-getopt")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://git.sr.ht/~sircmpwn/getopt")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "136fykmi48dddi2na1qf6p4ln1m0mn8069bf44z5hrks6m813d87"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "git.sr.ht/~sircmpwn/getopt"))
      (inputs
       `(("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))
      (home-page "https://git.sr.ht/~sircmpwn/getopt")
      (synopsis "POSIX-compatible getopt implementation for Go")
      (description "A POSIX-compatible getopt implementation for Go, because
POSIX getopt is The Correct Way to interpret arguments to command line
utilities.")
      (license license:expat))))
