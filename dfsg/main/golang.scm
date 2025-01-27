;;; Copyright © 2020-2023 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main golang)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix build-system go)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-compression)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-maths)
  #:use-module (gnu packages golang-vcs)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages node)
  #:use-module (gnu packages prometheus)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages syncthing)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages version-control))

;; These packages are sorted alphabetically for lack of a better option.

(define-public go-camlistore-org-pkg-buildinfo
  (let ((commit "c55c8602d3cea4511081630e17bca7ed601abc44")
        (revision "1"))
    (package
      (name "go-camlistore-org-pkg-buildinfo")
      (version (git-version "0.9" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/perkeep/perkeep")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1h9f6kj6ifwgx9hymwkn5w0ri291js1951mlc8fa5lagpbhnqd1g"))
         (modules '((guix build utils)))
         (snippet
          '(begin
             (delete-file-recursively "vendor")
             (delete-file-recursively "website")
             (delete-file-recursively "server/camlistored")
             (for-each make-file-writable (find-files "."))))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "camlistore.org/pkg/buildinfo"
         #:unpack-path "camlistore.org"))
      (home-page "https://perkeep.org/pkg/")
      (synopsis "Go library for personal storage system")
      (description "Camlistore is your personal storage system for life: a way
of storing, syncing, sharing, modelling and backing up content.")
      (license license:asl2.0))))

(define-public go-camlistore-org-pkg-images
  (package
    (inherit go-camlistore-org-pkg-buildinfo)
    (name "go-camlistore-org-pkg-images")
    (arguments
     '(#:import-path "camlistore.org/pkg/images"
       #:unpack-path "camlistore.org"))
    (propagated-inputs
     `(("go-github-com-nf-cr2" ,go-github-com-nf-cr2)
       ("go-github-com-rwcarlsen-goexif" ,go-github-com-rwcarlsen-goexif)
       ("go-go4-org-readerutil" ,go-go4-org-readerutil)
       ("go-golang-org-x-image" ,go-golang-org-x-image)))))

(define-public go-cloud-google-com-go
  (package
    (name "go-cloud-google-com-go")
    (version "0.99.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/googleapis/google-cloud-go")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1h5w5rnfaifg6frgyh7pz6604zhdacy0jmha0i0vvmb8n2vadx2n"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "cloud.google.com/go"))
    (propagated-inputs
      `(;("go-google-golang-org-protobuf" ,go-google-golang-org-protobuf)
        ;("go-google-golang-org-grpc" ,go-google-golang-org-grpc)
        ;("go-google-golang-org-genproto" ,go-google-golang-org-genproto)
        ;("go-google-golang-org-api" ,go-google-golang-org-api)
        ;("go-golang-org-x-xerrors" ,go-golang-org-x-xerrors)
        ;("go-golang-org-x-oauth2" ,go-golang-org-x-oauth2)
        ;("go-go-opencensus-io" ,go-go-opencensus-io)
        ;("go-github-com-googleapis-gax-go-v2" ,go-github-com-googleapis-gax-go-v2)
        ;("go-github-com-google-martian-v3" ,go-github-com-google-martian-v3)
        ;("go-github-com-google-go-cmp" ,go-github-com-google-go-cmp)
        ;("go-github-com-golang-protobuf" ,go-github-com-golang-protobuf)
        ;;("go-cloud-google-com-go-storage" ,go-cloud-google-com-go-storage)     ; TODO: import
        ))
    (home-page "https://cloud.google.com/go")
    (synopsis "Google Cloud Client Libraries for Go")
    (description
      "Package cloud is the root of the packages used to access Google Cloud Services.
See
@url{https://godoc.org/cloud.google.com/go,https://godoc.org/cloud.google.com/go}
for a full list of sub-packages.")
    (license license:asl2.0)))

(define-public go-code-gitea-io-gitea-vet
  (package
    (name "go-code-gitea-io-gitea-vet")
    (version "0.2.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gitea.com/gitea/gitea-vet")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1f5b3l79jwp6nifj9pz12ddjz7z1r85pfrrdkix6hv0f7fb6vnbg"))))
    (build-system go-build-system)
    (arguments '(#:import-path "code.gitea.io/gitea-vet"))
    (propagated-inputs
     (list go-golang-org-x-tools))
    (home-page "https://code.gitea.io/gitea-vet")
    (synopsis "Golang vet tool for gitea development")
    (description "@code{go vet} tool for Gitea.")
    (license license:expat)))

(define-public go-code-gitea-io-sdk-gitea
  (package
    (name "go-code-gitea-io-sdk-gitea")
    (version "0.14.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gitea.com/gitea/go-sdk")
               (commit (string-append "gitea/v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0a5dp0wlinnjacv7a6qkkg9ninqqbf8qrdfjr7is0kxvlkr0ih7f"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f      ; Requires a running gitea instance.
       #:unpack-path "code.gitea.io/sdk"
       #:import-path "code.gitea.io/sdk/gitea"))
    (propagated-inputs
     (list go-github-com-hashicorp-go-version))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://code.gitea.io/sdk")
    (synopsis "Gitea SDK for Go")
    (description
     "This project acts as a client SDK implementation written in Go to interact
with the Gitea API implementation.  For further information take a look at the
current @url{https://godoc.org/code.gitea.io/sdk/gitea,documentation}.")
    (license license:expat)))

(define-public go-filippo-io-mkcert
  (package
    (name "go-filippo-io-mkcert")
    (version "1.4.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/FiloSottile/mkcert")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ms9mjspiwlsgsnir0ccj3w8vhvrphf5i6k9q3hrz47y2a6igh0l"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "filippo.io/mkcert"))
    (propagated-inputs
     `(("go-golang-org-x-text" ,go-golang-org-x-text)
       ("go-golang-org-x-crypto" ,go-golang-org-x-crypto)
       ("go-software-sslmate-com-src-go-pkcs12" ,go-software-sslmate-com-src-go-pkcs12)
       ("go-howett-net-plist" ,go-howett-net-plist)
       ("go-golang-org-x-net" ,go-golang-org-x-net)))
    (home-page "https://filippo.io/mkcert")
    (synopsis "mkcert")
    (description
     "Command mkcert is a simple zero-config tool to make development certificates.")
    (license license:bsd-3)))

(define-public go-gitea-com-go-chi-binding
  (package
    (name "go-gitea-com-go-chi-binding")
    (version "0.0.0-20211013065440-d16dc407c2be")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gitea.com/go-chi/binding.git")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0hkgnyr8z0l4xrb3d159dhz85kabvs6g0wpy9g9nh467z3z3v6mm"))))
    (build-system go-build-system)
    (arguments '(#:import-path "gitea.com/go-chi/binding"))
    (propagated-inputs
     (list go-github-com-go-chi-chi-v5
           go-github-com-goccy-go-json
           go-github-com-unknwon-com))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://gitea.com/go-chi/binding")
    (synopsis "Middleware binding providing request data binding and validation")
    (description
     "Package binding is a middleware that provides request data binding and
validation for @code{net/http}.")
    (license license:asl2.0)))

(define-public go-gitea-com-go-chi-cache
  (package
    (name "go-gitea-com-go-chi-cache")
    (version "0.0.0-20211201020628-dcb774c4ffea")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gitea.com/go-chi/cache.git")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0qb3anqfvm5lm0n6anb48rn3f597wq3vkpg90g3gb8qblkyy9h24"))))
    (build-system go-build-system)
    (arguments '(#:import-path "gitea.com/go-chi/cache"))
    (propagated-inputs
     (list go-github-com-bradfitz-gomemcache
           go-github-com-go-redis-redis
           go-github-com-go-sql-driver-mysql
           go-github-com-lib-pq
           go-github-com-lunny-nodb
           go-github-com-siddontang-ledisdb
           go-github-com-smartystreets-goconvey
           go-github-com-unknwon-com
           go-gopkg-in-ini-v1))
    (home-page "https://gitea.com/go-chi/cache")
    (synopsis "Cache management for different databases")
    (description
     "Package cache is a middleware that provides the cache management of Macaron.")
    (license license:asl2.0)))

(define-public go-gitea-com-go-chi-captcha
  (package
    (name "go-gitea-com-go-chi-captcha")
    (version "0.0.0-20211013065431-70641c1a35d5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gitea.com/go-chi/captcha.git")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1k7pz1zmnrbgd8vlvpija3wm7qa14dmqywvqrqxx6l0n1dxzd4fi"))))
    (build-system go-build-system)
    (arguments '(#:import-path "gitea.com/go-chi/captcha"))
    (propagated-inputs
     (list go-gitea-com-go-chi-cache
           go-github-com-go-chi-chi-v5
           go-github-com-smartystreets-goconvey
           go-github-com-unknwon-com))
    (home-page "https://gitea.com/go-chi/captcha")
    (synopsis "Captcha service for chi")
    (description
     "Package captcha a middleware that provides captcha service for chi.")
    (license license:asl2.0)))

(define-public go-gitea-com-go-chi-session
  (package
    (name "go-gitea-com-go-chi-session")
    (version "0.0.0-20211218221615-e3605d8b28b8")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gitea.com/go-chi/session.git")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1m5dmjlb85z6518fpvcyl9ljz6zivq9is4mxzbfylkssmavrqmb7"))))
    (build-system go-build-system)
    (arguments '(#:import-path "gitea.com/go-chi/session"))
    (propagated-inputs
     (list go-github-com-bradfitz-gomemcache
           go-github-com-couchbase-go-couchbase
           go-github-com-go-chi-chi-v5
           go-github-com-go-redis-redis-v8
           go-github-com-go-sql-driver-mysql
           go-github-com-lib-pq
           go-github-com-siddontang-ledisdb
           go-github-com-smartystreets-goconvey
           go-github-com-unknwon-com
           go-gopkg-in-ini-v1))
    (home-page "https://gitea.com/go-chi/session")
    (synopsis "Session management for go-chi")
    (description "This package provides middleware that provides session
management for @code{go-chi}.")
    (license license:asl2.0)))

(define-public go-gitea-com-lunny-levelqueue
  (package
    (name "go-gitea-com-lunny-levelqueue")
    (version "0.4.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gitea.com/lunny/levelqueue")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0ppgr841qp7z2gp93ymkw7xy56jdb6h2r9zy8nn4v3d3lxr0gl0n"))))
    (build-system go-build-system)
    (arguments '(#:import-path "gitea.com/lunny/levelqueue"))
    (propagated-inputs
     (list go-github-com-syndtr-goleveldb))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://gitea.com/lunny/levelqueue")
    (synopsis "Golang queue library")
    (description
     "Level queue is a simple queue golang library based on go-leveldb.")
    (license license:expat)))

(define-public go-gitea-com-xorm-sqlfiddle
  (package
    (name "go-gitea-com-xorm-sqlfiddle")
    (version "0.0.0-20180821085327-62ce714f951a")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gitea.com/xorm/sqlfiddle.git")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "03g79q02djhk7rjp4k6jh9aaq6z3sqwjcyz4zzml1klv5gwyb1ij"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f      ; Tests try to connect to the Internet.
       #:import-path "gitea.com/xorm/sqlfiddle"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://gitea.com/xorm/sqlfiddle")
    (synopsis "SQL Fiddle API (UnOfficial)")
    (description
      "This Go library is aimed to provide an API to operate
@url{http://sqlfiddle.com/,http://sqlfiddle.com/}")
    (license license:expat)))

(define-public go-github-com-6543-go-version
  (package
    (name "go-github-com-6543-go-version")
    (version "1.4.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/6543/go-version")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0qlgwxhw9r2r88ap1m9q1hknn4g3xvcdpjgq14gswcqzd34pyg2v"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/6543/go-version"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/6543/go-version")
    (synopsis "Versioning Library for Go")
    (description
      "go-version is a library for parsing versions and version constraints, and
verifying versions against a set of constraints.  go-version can sort a
collection of versions properly, handles prerelease/beta versions, can increment
versions, etc.")
    (license license:mpl2.0)))

(define-public go-github-com-acomagu-bufpipe
  (package
    (name "go-github-com-acomagu-bufpipe")
    (version "1.0.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/acomagu/bufpipe")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1a74sh8g3wmigxx2i0jcpysd46509ll4fw626wfzwrlrbbd1z144"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/acomagu/bufpipe"))
    (propagated-inputs
     (list go-github-com-matryer-is))
    (home-page "https://github.com/acomagu/bufpipe")
    (synopsis #f)
    (description #f)
    (license #f)))

(define-public go-github-com-ajg-form
  (package
    (name "go-github-com-ajg-form")
    (version "1.5.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ajg/form")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1d6sxzzf9yycdf8jm5877y0khmhkmhxfw3sc4xpdcsrdlc7gqh5a"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/ajg/form"
       #:tests? #f))
    (home-page "https://github.com/ajg/form")
    (synopsis "form")
    (description #f)
    (license license:bsd-3)))

(define-public go-github-com-akutz-memconn
  (package
    (name "go-github-com-akutz-memconn")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/akutz/memconn")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1mhghzcx2zxr4bpyf4wx3j7ph9srw38wxg78svwbjh930r2kzssq"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f  ; Tests include certificates which expire on 2018-05-24T14:45:08Z
       #:import-path "github.com/akutz/memconn"))
    (home-page "https://github.com/akutz/memconn")
    (synopsis "MemConn")
    (description
     "MemConn provides named, in-memory network connections for Go.")
    (license license:asl2.0)))

(define-public go-github-com-alexbrainman-sspi
  (package
    (name "go-github-com-alexbrainman-sspi")
    (version "0.0.0-20210105120005-909beea2cc74")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/alexbrainman/sspi")
                     (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00md46ywma7v0czi9zlbwvsqwlhpsivf064ws22a4nli4y0kvimh"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f      ; Tests only work on Windows?
       #:import-path "github.com/alexbrainman/sspi"))
    (home-page "https://github.com/alexbrainman/sspi")
    (synopsis #f)
    (description
     "This repository holds Go packages for accessing Security Support Provider
Interface on Windows.")
    (license license:bsd-3)))

(define-public go-github-com-alicebob-gopher-json
  (package
    (name "go-github-com-alicebob-gopher-json")
    (version "0.0.0-20200520072559-a9ecdc9d1d3a")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/alicebob/gopher-json")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1fblh4anp85qc77zv7465a7a5yn2l5i2lrwsrnbhlmhpzfa6lmdl"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f      ; bad argument #1 to decode
       #:import-path "github.com/alicebob/gopher-json"))
    (propagated-inputs
     (list go-github-com-yuin-gopher-lua))
    (home-page "https://github.com/alicebob/gopher-json")
    (synopsis "JSON encoder/decoder")
    (description
     "Package json is a simple JSON encoder/decoder for gopher-lua.")
    (license license:unlicense)))

(define-public go-github-com-alicebob-miniredis
  (package
    (name "go-github-com-alicebob-miniredis")
    (version "2.5.0+incompatible")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/alicebob/miniredis")
               (commit (string-append "v" (go-version->git-ref version)))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "193vjx8lrrklffjyi6x635hmmwa72gjvcq2dm09hyd3258fpdzrg"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f      ; cmd_scripting_test.go:444: no EVAL error
       #:import-path "github.com/alicebob/miniredis"))
    (propagated-inputs
     (list go-github-com-alicebob-gopher-json
           go-github-com-gomodule-redigo
           go-github-com-yuin-gopher-lua))
    (home-page "https://github.com/alicebob/miniredis")
    (synopsis "Miniredis")
    (description
     "Package miniredis is a pure Go Redis test server, for use in Go unittests.
There are no dependencies on system binaries, and every server you start will be
empty.")
    (license license:expat)))

(define-public go-github-com-antchfx-htmlquery
  (package
    (name "go-github-com-antchfx-htmlquery")
    (version "1.2.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/antchfx/htmlquery")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "19avv51fsda357vzrn5bz1lbfw9bpl5vcs4q3f1zw6kb37wa728z"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/antchfx/htmlquery"))
    (propagated-inputs
     (list go-github-com-antchfx-xpath
           go-github-com-golang-groupcache
           go-golang-org-x-net
           go-golang-org-x-text))
    (home-page "https://github.com/antchfx/htmlquery")
    (synopsis "htmlquery")
    (description #f)
    (license license:expat)))

(define-public go-github-com-antchfx-xmlquery
  (package
    (name "go-github-com-antchfx-xmlquery")
    (version "1.3.10")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/antchfx/xmlquery")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1ghdvbf9nd9pq0ny6xr1az93hcq5r3qy940dz9mr8mcrrsm4m1rv"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/antchfx/xmlquery"))
    (propagated-inputs
     `(("go-github-com-antchfx-xpath" ,go-github-com-antchfx-xpath)
       ("go-github-com-golang-groupcache" ,go-github-com-golang-groupcache)
       ("go-golang-org-x-net" ,go-golang-org-x-net)
       ("go-golang-org-x-text" ,go-golang-org-x-text)))
    (home-page "https://github.com/antchfx/xmlquery")
    (synopsis "xmlquery")
    (description #f)
    (license license:expat)))

(define-public go-github-com-antchfx-xpath
  (package
    (name "go-github-com-antchfx-xpath")
    (version "1.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/antchfx/xpath")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1qidxka248aaqf8n3m04ig25skgaxzh2ahm75swwxsk3s198lg08"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/antchfx/xpath"))
    (home-page "https://github.com/antchfx/xpath")
    (synopsis "XPath")
    (description #f)
    (license license:expat)))

(define-public go-github-com-araddon-dateparse
  (package
    (name "go-github-com-araddon-dateparse")
    (version "0.0.0-20210429162001-6b43995a97de")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/araddon/dateparse.git")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0p60rdbfk7d97hb1kk225lvnqvhw04d822782hn66i4yfvigrraj"))))
    (build-system go-build-system)
    (arguments
     (list
       #:import-path "github.com/araddon/dateparse"
       #:phases
       #~(modify-phases %standard-phases
           (replace 'check
             (lambda* (#:key inputs #:allow-other-keys #:rest args)
               (unless
                 ;; The tests fail when run with gccgo.
                 ;; Unable to load timezones.
                 (false-if-exception (search-input-file inputs "/bin/gccgo"))
                 (apply (assoc-ref %standard-phases 'check) args)))))))
    (native-inputs
     (list go-github-com-scylladb-termtables
           go-github-com-stretchr-testify))
    (home-page "https://github.com/araddon/dateparse")
    (synopsis "Go Date Parser")
    (description
     "Package dateparse parses date-strings without knowing the format in advance,
using a fast lex based approach to eliminate shotgun attempts.  It leans towards
US style dates when there is a conflict.")
    (license license:expat)))

(define-public go-github-com-aws-aws-sdk-go-v2-service-ssm
  (package
    (inherit go-github-com-aws-aws-sdk-go-v2)
    (name "go-github-com-aws-aws-sdk-go-v2-service-ssm")
    (arguments
     '(#:import-path "github.com/aws/aws-sdk-go-v2/service/ssm"
       #:unpack-path "github.com/aws/aws-sdk-go-v2"))))

(define-public go-github-com-aws-smithy-go
  (package
    (name "go-github-com-aws-smithy-go")
    (version "1.13.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/aws/smithy-go")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0xw7a3iah92w1mbh646gn9i3079a7ddbr72yjklq4rsngfakx08k"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/aws/smithy-go"))
    (propagated-inputs
     (list go-github-com-jmespath-go-jmespath
           go-github-com-google-go-cmp))
    (home-page "https://github.com/aws/smithy-go")
    (synopsis "Smithy Go")
    (description
     "Package smithy provides the core components for a Smithy SDK.")
    (license license:asl2.0)))

(define-public go-github-com-blevesearch-bleve
  (package
    (name "go-github-com-blevesearch-bleve")
    (version "0.8.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/blevesearch/bleve")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "12cwzxzizqf30dxds6yvl8bvb8i58f9jpln3608c6i79kl048xc1"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/blevesearch/bleve"))
    (propagated-inputs
     `(("go-github-com-blevesearch-go-porterstemmer" ,go-github-com-blevesearch-go-porterstemmer)
       ("go-github-com-blevesearch-segment" ,go-github-com-blevesearch-segment)
       ("go-github-com-blevesearch-snowballstem" ,go-github-com-blevesearch-snowballstem)
       ("go-github-com-couchbase-vellum" ,go-github-com-couchbase-vellum)
       ("go-github-com-edsrzf-mmap-go" ,go-github-com-edsrzf-mmap-go)
       ("go-github-com-golang-protobuf" ,go-github-com-golang-protobuf)
       ("go-github-com-golang-snappy" ,go-github-com-golang-snappy)
       ("go-github-com-roaringbitmap-roaring" ,go-github-com-roaringbitmap-roaring)
       ("go-github-com-steveyen-gtreap" ,go-github-com-steveyen-gtreap)
       ("go-go-etcd-io-bbolt" ,go-go-etcd-io-bbolt)))
    (home-page "https://github.com/blevesearch/bleve")
    (synopsis "bleve")
    (description #f)
    (license license:asl2.0)))

(define-public go-github-com-blevesearch-bleve-index-api
  (package
    (name "go-github-com-blevesearch-bleve-index-api")
    (version "1.0.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/blevesearch/bleve_index_api")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1fnfy1jj1qxk2gjvq4rrqj6yw8mmlibvhfm5qxv239sr23g42114"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/blevesearch/bleve_index_api"))
    (home-page "https://github.com/blevesearch/bleve_index_api")
    (synopsis "Bleve Index API")
    (description "Bleve supports a pluggable Index interface.")
    (license license:asl2.0)))

(define-public go-github-com-blevesearch-bleve-v2
  (package
    (name "go-github-com-blevesearch-bleve-v2")
    (version "2.0.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/blevesearch/bleve")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1xpcckqk9c2jbz64r72w9nbpwvjhsc0rbiays523rgvgja3h4zpw"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (delete-file-recursively "cmd/bleve/vendor")))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/blevesearch/bleve/v2"))
    (propagated-inputs
     (list go-golang-org-x-text
           go-go-etcd-io-bbolt
           go-github-com-syndtr-goleveldb
           go-github-com-steveyen-gtreap
           go-github-com-spf13-cobra
           go-github-com-rcrowley-go-metrics
           go-github-com-kljensen-snowball
           go-github-com-golang-protobuf
           go-github-com-couchbase-moss
           go-github-com-blevesearch-zapx-v15
           go-github-com-blevesearch-zapx-v14
           go-github-com-blevesearch-zapx-v13
           go-github-com-blevesearch-zapx-v12
           go-github-com-blevesearch-zapx-v11
           go-github-com-blevesearch-vellum
           go-github-com-blevesearch-upsidedown-store-api
           go-github-com-blevesearch-snowballstem
           go-github-com-blevesearch-segment
           go-github-com-blevesearch-scorch-segment-api-v2
           go-github-com-blevesearch-go-porterstemmer
           go-github-com-blevesearch-bleve-index-api
           go-github-com-bits-and-blooms-bitset
           go-github-com-roaringbitmap-roaring))
    (home-page "https://github.com/blevesearch/bleve")
    (synopsis "bleve")
    (description "Package bleve is a library for indexing and searching text.")
    (license license:asl2.0)))

(define-public go-github-com-blevesearch-go-porterstemmer
  (package
    (name "go-github-com-blevesearch-go-porterstemmer")
    (version "1.0.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/blevesearch/go-porterstemmer.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0nj448j7kj31vg76xa7nh2i6iz4b4fnvarh0dgsl11ay1pmfhj45"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/blevesearch/go-porterstemmer"
       ;; Tests require network access.
       #:tests? #f))
    (home-page "https://github.com/blevesearch/go-porterstemmer")
    (synopsis "This fork...")
    (description
     "I'm maintaining this fork because the original author was not replying to issues
or pull requests.  For now I plan on maintaining this fork as necessary.")
    (license license:expat)))

(define-public go-github-com-blevesearch-mmap-go
  (package
    (name "go-github-com-blevesearch-mmap-go")
    (version "1.0.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/blevesearch/mmap-go")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0lmrz1cqy2qc83rsgvcjpwl458pkpa7ksncjy4iy484493szvbxz"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/blevesearch/mmap-go"))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://github.com/blevesearch/mmap-go")
    (synopsis "mmap-go")
    (description
     "Package mmap allows mapping files into memory.  It tries to provide a simple,
reasonably portable interface, but doesn't go out of its way to abstract away
every little platform detail.  This specifically means:")
    (license license:bsd-3)))

(define-public go-github-com-blevesearch-scorch-segment-api-v2
  (package
    (name "go-github-com-blevesearch-scorch-segment-api-v2")
    (version "2.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/blevesearch/scorch_segment_api")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0fyp83cvlrivw33hsgjfbc7jr6vlnkdaf4brdcn84ln3f9p5888j"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/blevesearch/scorch_segment_api/v2"))
    (propagated-inputs
     (list go-github-com-davecgh-go-spew
           go-github-com-blevesearch-bleve-index-api
           go-github-com-roaringbitmap-roaring))
    (home-page "https://github.com/blevesearch/scorch_segment_api")
    (synopsis "Scorch Segment API")
    (description "Scorch supports a pluggable Segment interface.")
    (license license:asl2.0)))

(define-public go-github-com-blevesearch-segment
  (package
    (name "go-github-com-blevesearch-segment")
    (version "0.9.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/blevesearch/segment")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1p8n2p047x3lyahfhly92qm8b90yc6p1yksns73amlrn6fwdhcyx"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/blevesearch/segment"))
    (home-page "https://github.com/blevesearch/segment")
    (synopsis "segment")
    (description
     "Package segment is a library for performing Unicode Text Segmentation as
described in Unicode Standard Annex #29
@url{http://www.unicode.org/reports/tr29/,http://www.unicode.org/reports/tr29/}")
    (license license:asl2.0)))

(define-public go-github-com-blevesearch-snowballstem
  (package
    (name "go-github-com-blevesearch-snowballstem")
    (version "0.9.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/blevesearch/snowballstem")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1yzglihjjn588xmmkaawqhc95pkk1cyc4bq7ipw7jqfw2np1f2rm"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/blevesearch/snowballstem"))
    (home-page "https://github.com/blevesearch/snowballstem")
    (synopsis "snowballstem")
    (description
     "This repository contains the Go stemmers generated by the
@url{https://github.com/snowballstem/snowball,Snowball} project.  They are
maintained outside of the core bleve package so that they may be more easily be
reused in other contexts.")
    (license license:bsd-3)))

(define-public go-github-com-blevesearch-upsidedown-store-api
  (package
    (name "go-github-com-blevesearch-upsidedown-store-api")
    (version "1.0.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/blevesearch/upsidedown_store_api")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1vj9dqwaixpd090rsqdp82ikp9hdvfs6868dbgi7fniyr0j0gn27"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/blevesearch/upsidedown_store_api"))
    (home-page "https://github.com/blevesearch/upsidedown_store_api")
    (synopsis "Upsidedown Store API")
    (description
     "Upsidedown supports a pluggable Key/Value storage interface.")
    (license license:asl2.0)))

(define-public go-github-com-blevesearch-vellum
  (package
    (name "go-github-com-blevesearch-vellum")
    (version "1.0.7")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/blevesearch/vellum")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1jw4w9wp02rzcrp6h33271ravm1xjrldvadb4chylbzlji1a504m"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (delete-file-recursively "vendor")))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/blevesearch/vellum"))
    (propagated-inputs
     (list go-github-com-spf13-cobra
           go-github-com-blevesearch-mmap-go
           go-github-com-bits-and-blooms-bitset))
    (home-page "https://github.com/blevesearch/vellum")
    (synopsis "vellum")
    (description
     "Package vellum is a library for building, serializing and executing an FST
(finite state transducer).")
    (license license:asl2.0)))

(define-public go-github-com-blevesearch-zapx-v11
  (package
    (name "go-github-com-blevesearch-zapx-v11")
    (version "11.3.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/blevesearch/zapx")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0aci580kl9zi2q8jpjq03ipnl07qlldzsa6rybva4svxkfrc96nd"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/blevesearch/zapx/v11"))
    (propagated-inputs
     (list go-github-com-spf13-cobra
           go-github-com-golang-snappy
           go-github-com-blevesearch-vellum
           go-github-com-blevesearch-scorch-segment-api-v2
           go-github-com-blevesearch-mmap-go
           go-github-com-blevesearch-bleve-index-api
           go-github-com-roaringbitmap-roaring))
    (home-page "https://github.com/blevesearch/zapx")
    (synopsis "zapx file format")
    (description
     "The zapx module is fork of @url{https://github.com/blevesearch/zap,zap} module
which maintains file format compatibility, but removes dependency on bleve, and
instead depends only on the independent interface modules:")
    (license license:asl2.0)))

(define-public go-github-com-blevesearch-zapx-v12
  (package
    (name "go-github-com-blevesearch-zapx-v12")
    (version "12.3.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/blevesearch/zapx")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1qiz7xd0xl4q34xfsr13cpdm3gr1xdj2kw7mllasmkv7apzvh56r"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/blevesearch/zapx/v12"))
    (propagated-inputs
     (list go-github-com-spf13-cobra
           go-github-com-golang-snappy
           go-github-com-blevesearch-vellum
           go-github-com-blevesearch-scorch-segment-api-v2
           go-github-com-blevesearch-mmap-go
           go-github-com-blevesearch-bleve-index-api
           go-github-com-roaringbitmap-roaring))
    (home-page "https://github.com/blevesearch/zapx")
    (synopsis "zapx file format")
    (description
     "The zapx module is fork of @url{https://github.com/blevesearch/zap,zap} module
which maintains file format compatibility, but removes dependency on bleve, and
instead depends only on the independent interface modules:")
    (license license:asl2.0)))

(define-public go-github-com-blevesearch-zapx-v13
  (package
    (name "go-github-com-blevesearch-zapx-v13")
    (version "13.3.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/blevesearch/zapx")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "19dgsgny5w38zdblyhbp6bcn2gwb9zd3bsc4x1pc365blpfxag05"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/blevesearch/zapx/v13"))
    (propagated-inputs
     (list go-github-com-spf13-cobra
           go-github-com-golang-snappy
           go-github-com-blevesearch-vellum
           go-github-com-blevesearch-scorch-segment-api-v2
           go-github-com-blevesearch-mmap-go
           go-github-com-blevesearch-bleve-index-api
           go-github-com-roaringbitmap-roaring))
    (home-page "https://github.com/blevesearch/zapx")
    (synopsis "zapx file format")
    (description
     "The zapx module is fork of @url{https://github.com/blevesearch/zap,zap} module
which maintains file format compatibility, but removes dependency on bleve, and
instead depends only on the independent interface modules:")
    (license license:asl2.0)))

(define-public go-github-com-blevesearch-zapx-v14
  (package
    (name "go-github-com-blevesearch-zapx-v14")
    (version "14.3.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/blevesearch/zapx")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0ygrm5ga91kmkpr0vh2s5h08q9afrm5j5v042qrrqcvm3sn7gy0n"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/blevesearch/zapx/v14"))
    (propagated-inputs
     (list go-github-com-spf13-cobra
           go-github-com-golang-snappy
           go-github-com-blevesearch-vellum
           go-github-com-blevesearch-scorch-segment-api-v2
           go-github-com-blevesearch-mmap-go
           go-github-com-blevesearch-bleve-index-api
           go-github-com-roaringbitmap-roaring))
    (home-page "https://github.com/blevesearch/zapx")
    (synopsis "zapx file format")
    (description
     "The zapx module is fork of @url{https://github.com/blevesearch/zap,zap} module
which maintains file format compatibility, but removes dependency on bleve, and
instead depends only on the independent interface modules:")
    (license license:asl2.0)))

(define-public go-github-com-blevesearch-zapx-v15
  (package
    (name "go-github-com-blevesearch-zapx-v15")
    (version "15.3.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/blevesearch/zapx")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0r6yvfa2pq7b43afc8a0sja80g1aq5by38x5mv0c0lk6w3wkj5hy"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/blevesearch/zapx/v15"))
    (propagated-inputs
     (list go-github-com-spf13-cobra
           go-github-com-golang-snappy
           go-github-com-blevesearch-vellum
           go-github-com-blevesearch-scorch-segment-api-v2
           go-github-com-blevesearch-mmap-go
           go-github-com-blevesearch-bleve-index-api
           go-github-com-roaringbitmap-roaring))
    (home-page "https://github.com/blevesearch/zapx")
    (synopsis "zapx file format")
    (description
     "The zapx module is fork of @url{https://github.com/blevesearch/zap,zap} module
which maintains file format compatibility, but removes dependency on bleve, and
instead depends only on the independent interface modules:")
    (license license:asl2.0)))

(define-public go-github-com-boombuler-barcode
  (package
    (name "go-github-com-boombuler-barcode")
    (version "1.0.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/boombuler/barcode")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0v4ypgh3xarzfpgys838mgkfabqacbjklhf4kfqnycs0v0anvnlr"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/boombuler/barcode"))
    (home-page "https://github.com/boombuler/barcode")
    (synopsis "Introduction")
    (description
      "This is a package for GO which can be used to create different types of
barcodes.")
    (license license:expat)))

(define-public go-github-com-btcsuite-btcd-chaincfg
  (package
    (inherit go-github-com-btcsuite-btcd-btcec)
    (name "go-github-com-btcsuite-btcd-chaincfg")
    (arguments
     '(#:unpack-path "github.com/btcsuite/btcd"
       #:import-path "github.com/btcsuite/btcd/chaincfg"))
    (synopsis "btcd")
    (description #f)))

(define-public go-github-com-btcsuite-btcd-wire
  (package
    (inherit go-github-com-btcsuite-btcd-btcec)
    (name "go-github-com-btcsuite-btcd-wire")
    (arguments
     '(#:unpack-path "github.com/btcsuite/btcd"
       #:import-path "github.com/btcsuite/btcd/wire"
       #:tests? #f))
    (synopsis "btcd")
    (description #f)))

(define-public go-github-com-btcsuite-btcutil
  (package
    (name "go-github-com-btcsuite-btcutil")
    (version "1.0.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/btcsuite/btcutil")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0wwykb4cbq8xj2mls2mxma5vaahdgdy3vqw1r2fi4wyj0yr4kyw9"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/btcsuite/btcutil"
       #:tests? #f))
    (propagated-inputs
     `(("go-github-com-btcsuite-btcd-btcec" ,go-github-com-btcsuite-btcd-btcec)
       ("go-github-com-btcsuite-btcd-chaincfg" ,go-github-com-btcsuite-btcd-chaincfg)
       ("go-github-com-btcsuite-btcd-wire" ,go-github-com-btcsuite-btcd-wire)
       ("go-golang-org-x-crypto" ,go-golang-org-x-crypto)))
    (native-inputs
     `(("go-github-com-davecgh-go-spew" ,go-github-com-davecgh-go-spew)))
    (home-page "https://github.com/btcsuite/btcutil")
    (synopsis "btcutil")
    (description #f)
    (license license:isc)))

(define-public go-github-com-buger-jsonparser
  (package
    (name "go-github-com-buger-jsonparser")
    (version "1.1.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/buger/jsonparser")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0qv2lsh2biwxn927941gqiv5pqg7n4v58j0i536pjp7pr17pq7dp"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/buger/jsonparser"))
    (home-page "https://github.com/buger/jsonparser")
    (synopsis #f)
    (description #f)
    (license license:expat)))

(define-public go-github-com-census-instrumentation-opencensus-proto
  (package
    (name "go-github-com-census-instrumentation-opencensus-proto")
    (version "0.3.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/census-instrumentation/opencensus-proto")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1ngp6jb345xahsijjpwwlcy2giymyzsy7kdhkrvgjafqssk6aw6f"))))
    (build-system go-build-system)
    (arguments
      '(#:import-path "github.com/census-instrumentation/opencensus-proto"))
    (home-page "https://github.com/census-instrumentation/opencensus-proto")
    (synopsis
      "OpenCensus Proto - Language Independent Interface Types For OpenCensus")
    (description
      "Census provides a framework to define and collect stats against metrics and to
break those stats down across user-defined dimensions.")
    (license license:asl2.0)))

;; This package is deprecated upstream
(define-public go-github-com-chaseadamsio-goorgeous
  (package
    (name "go-github-com-chaseadamsio-goorgeous")
    (version "2.0.0+incompatible")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/chaseadamsio/goorgeous")
               (commit (string-append "v" (go-version->git-ref version)))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "179pazd942609b4ndhirb2sanz88s16d9csbdj7w4dnyqgqiyylk"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/chaseadamsio/goorgeous"))
    (propagated-inputs
     (list go-github-com-russross-blackfriday
           go-github-com-shurcool-sanitized-anchor-name))
    (home-page "https://github.com/chaseadamsio/goorgeous")
    (synopsis #f)
    (description #f)
    (license license:expat)))

(define-public go-github-com-chavacava-garif
  (package
    (name "go-github-com-chavacava-garif")
    (version "0.0.0-20210405164556-e8a0a408d6af")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/chavacava/garif")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "07cbl8x9cpai8k4086qmdq9r2wy97khvi0pkwl7bh8r9m3q4msiv"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/chavacava/garif"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/chavacava/garif")
    (synopsis "garif")
    (description
      "Package garif defines all the GO structures required to model a SARIF log file.
These structures were created using the JSON-schema sarif-schema-2.1.0.json of
SARIF logfiles available at
@url{https://github.com/oasis-tcs/sarif-spec/tree/master/Schemata,https://github.com/oasis-tcs/sarif-spec/tree/master/Schemata}.")
    (license license:expat)))

(define-public go-github-com-chi-middleware-proxy
  (package
    (name "go-github-com-chi-middleware-proxy")
    (version "1.1.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/chi-middleware/proxy")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0pslb4x8jhblgg7sfahrsiv7r4ay5aizgrqkrpfpwzsnhw88fl6h"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/chi-middleware/proxy"))
    (propagated-inputs
     (list go-github-com-go-chi-chi-v5))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/chi-middleware/proxy")
    (synopsis "proxy middleware")
    (description
     "Forwarded headers middleware to use if application is run behind reverse proxy.")
    (license license:expat)))

(define-public go-github-com-cncf-udpa-go
  (package
    (name "go-github-com-cncf-udpa-go")
    (version "0.0.0-20210930031921-04548b0d99d4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/cncf/udpa")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "16z9iqs7g6c084fh6y9v3skdbxnpyqw3d1y19v42llyl9hzx361v"))))
    (build-system go-build-system)
    (arguments
      '(#:import-path
        "github.com/cncf/udpa/go"
        #:unpack-path
        "github.com/cncf/udpa"))
    (propagated-inputs
     (list go-github-com-cncf-xds-go))
    (home-page "https://github.com/cncf/udpa")
    (synopsis "Description")
    (description
      "This library has been deprecated in favor of @code{github.com/cncf/xds/go}.  All
users are recommended to switch their imports.")
    (license license:asl2.0)))

(define-public go-github-com-cncf-xds-go
  (package
    (name "go-github-com-cncf-xds-go")
    (version "0.0.0-20211216145620-d92e9ce0af51")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/cncf/xds")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "14as79iiwwp5r6cxr5f0hl2dkb7qlqla4a8d8grjdrqldf2z1345"))))
    (build-system go-build-system)
    (arguments
      '(#:import-path
        "github.com/cncf/xds/go"
        #:unpack-path
        "github.com/cncf/xds"))
    (propagated-inputs
      `(;("go-google-golang-org-grpc" ,go-google-golang-org-grpc)
        ;("go-github-com-golang-protobuf" ,go-github-com-golang-protobuf)
        ;("go-github-com-envoyproxy-protoc-gen-validate" ,go-github-com-envoyproxy-protoc-gen-validate)
        ))
    (home-page "https://github.com/cncf/xds")
    (synopsis #f)
    (description #f)
    (license license:asl2.0)))

(define-public go-github-com-gocolly-colly
  (package
    (name "go-github-com-gocolly-colly")
    (version "1.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/gocolly/colly")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1a6brjy0a4pwq2ml3fvz6p7wjmg37rh006i00zxgv9v4vmv7b84d"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/gocolly/colly"))
    (propagated-inputs
     (list go-github-com-puerkitobio-goquery
           go-github-com-antchfx-htmlquery
           go-github-com-antchfx-xmlquery
           go-github-com-gobwas-glob
           go-github-com-kennygrant-sanitize
           go-github-com-saintfish-chardet
           go-github-com-temoto-robotstxt
           go-golang-org-x-net
           go-google-golang-org-appengine-internal
           go-google-golang-org-appengine-urlfetch))
    (home-page "https://github.com/gocolly/colly")
    (synopsis "Colly")
    (description "Package colly implements a HTTP scraping framework")
    (license license:asl2.0)))

(define-public go-github-com-coreos-go-iptables
  (package
    (name "go-github-com-coreos-go-iptables")
    (version "0.6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/coreos/go-iptables")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "14q1k7fsaq5vd613cbzr2fby9yvxnx3j6bwf3a4h1bk013nmappl"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f      ; Can't modify iptables from inside the container.
       #:import-path "github.com/coreos/go-iptables/iptables"
       #:unpack-path "github.com/coreos/go-iptables"))
    (native-inputs
     (list (specification->package "iptables")))
    (home-page "https://github.com/coreos/go-iptables")
    (synopsis "go-iptables")
    (description "Go bindings for iptables utility.")
    (license license:asl2.0)))

(define-public go-github-com-coreos-go-systemd
  (package
    (name "go-github-com-coreos-go-systemd")
    (version "0.0.0-20191104093116-d3cd4ed1dbcf")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/coreos/go-systemd")
                     (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "193mgqn7n4gbb8jb5kyn6ml4lbvh4xs55qpjnisaz7j945ik3kd8"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/coreos/go-systemd"
       #:phases
       (modify-phases %standard-phases
         (delete 'build)
         (delete 'check))))
    (home-page "https://github.com/coreos/go-systemd")
    (synopsis "go-systemd")
    (description "Go bindings to systemd.  The project has several packages:")
    (license license:asl2.0)))

(define-public go-github-com-coreos-go-systemd-util
  (package
    (inherit go-github-com-coreos-go-systemd)
    (name "go-github-com-coreos-go-systemd-util")
    (arguments
     '(#:unpack-path "github.com/coreos/go-systemd"
       #:import-path "github.com/coreos/go-systemd/util"))
    (propagated-inputs
     `(("go-github-com-coreos-pkg-dlopen" ,go-github-com-coreos-pkg-dlopen)))
    (synopsis "go-systemd")
    (description #f)))

(define-public go-github-com-coreos-pkg-dlopen
  (package
    (name "go-github-com-coreos-pkg-dlopen")
    (version "0.0.0-20170901145554-459346e834d8")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/coreos/pkg")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0j8qqw9wwjzidb8nm29dprj320z7wil11cn9wp53awkpw1y4bgq1"))))
    (build-system go-build-system)
    (arguments
     '(#:unpack-path "github.com/coreos/pkg"
       #:import-path "github.com/coreos/pkg/dlopen"))
    (home-page "https://github.com/coreos/pkg")
    (synopsis #f)
    (description #f)
    (license license:asl2.0)))

;; There are only a few Free Software commits after this tag.
(define-public go-github-com-couchbase-ghistogram
  (package
    (name "go-github-com-couchbase-ghistogram")
    (version "0.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/couchbase/ghistogram")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "05nhcp5i8l9ndcf18bn58qgm6vh10d59xnxz6qikk0sajyy4r2s1"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/couchbase/ghistogram"))
    (home-page "https://github.com/couchbase/ghistogram")
    (synopsis "ghistogram")
    (description
     "Simple histogram for golang that avoids runtime memory allocations.")
    (license license:asl2.0)))

(define-public go-github-com-couchbase-go-couchbase
  (package
    (name "go-github-com-couchbase-go-couchbase")
    (version "0.1.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/couchbase/go-couchbase")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0dpy8148a1fz47yibn8mzl29savqg4jkfvyp8vvgsi0zp7jmwj89"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/couchbase/go-couchbase"))
    (propagated-inputs
     (list go-github-com-couchbase-gomemcached
           go-github-com-couchbase-goutils))
    (home-page "https://github.com/couchbase/go-couchbase")
    (synopsis "A smart client for couchbase in go")
    (description "Package couchbase provides a smart client for go.")
    (license license:expat)))

;; DO NOT UPGRADE! Future versions are under the nonfree Business Source License.
(define-public go-github-com-couchbase-goutils
  (package
    (name "go-github-com-couchbase-goutils")
    (version "0.1.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/couchbase/goutils")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1nq3lz1isbbcgql7rq249m53gansvzd3xbc8kqchcp5mm60a1rq5"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/couchbase/goutils"
       ;; This package has circular dependencies with the other couchbase packages.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'build))))
    (propagated-inputs
     (list go-github-com-pkg-errors
           go-golang-org-x-crypto))
    (home-page "https://github.com/couchbase/goutils")
    (synopsis "goutils")
    (description "Common utility libraries for Couchbase Go code.")
    (license license:asl2.0)))

;; This is the last Free Software release.
(define-public go-github-com-couchbase-moss
  (package
    (name "go-github-com-couchbase-moss")
    (version "0.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/couchbase/moss")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "13ijal8f01rw8ivimz1f82q1l55m518jg0nq407dxj76lcrycqck"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f      ; TODO: Fix
       #:import-path "github.com/couchbase/moss"))
    (propagated-inputs
     (list go-github-com-mschoch-smat
           go-github-com-couchbase-ghistogram
           go-github-com-blevesearch-mmap-go))
    (home-page "https://github.com/couchbase/moss")
    (synopsis #f)
    (description #f)
    (license license:asl2.0)))

(define-public go-github-com-couchbase-vellum
  (package
    (name "go-github-com-couchbase-vellum")
    (version "1.0.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/couchbase/vellum")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1an304lm3q6y5f18c17rs8nv7kg9fd13mj6p7h2qf6ndjr7hzypg"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (delete-file-recursively "vendor")))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/couchbase/vellum"))
    (propagated-inputs
     `(("go-github-com-blevesearch-mmap-go" ,go-github-com-blevesearch-mmap-go)
       ("go-github-com-spf13-cobra" ,go-github-com-spf13-cobra)
       ("go-github-com-bits-and-blooms-bitset" ,go-github-com-bits-and-blooms-bitset)
       ("go-golang-org-x-sys" ,go-golang-org-x-sys)))
    (home-page "https://github.com/couchbase/vellum")
    (synopsis "vellum")
    (description #f)
    (license license:asl2.0)))

(define-public go-github-com-cpuguy83-go-md2man-v2
  (package
    (name "go-github-com-cpuguy83-go-md2man-v2")
    (version "2.0.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/cpuguy83/go-md2man")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "051ljpzf1f5nh631lvn53ziclkzmx5lza8545mkk6wxdfnfdcx8f"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (delete-file-recursively "vendor")))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/cpuguy83/go-md2man/v2"))
    (propagated-inputs
     (list go-github-com-russross-blackfriday-v2))
    (home-page "https://github.com/cpuguy83/go-md2man")
    (synopsis "go-md2man")
    (description "Converts markdown into roff (man pages).")
    (license license:expat)))

(define-public go-github-com-dave-astrid
  (package
    (name "go-github-com-dave-astrid")
    (version "0.0.0-20170323122508-8c2895878b14")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/dave/astrid")
                     (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0fg6szwsh3cq32g1kzsdqhg6zgi5klsra9ncy5cl5qi1m6aahkfr"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f      ; Tests have bitrotted against newer x-tools
       #:import-path "github.com/dave/astrid"))
    (native-inputs
     (list go-github-com-dave-patsy
           go-golang-org-x-tools))
    (home-page "https://github.com/dave/astrid")
    (synopsis "Astrid")
    (description "Package astrid is a collection of AST utilities")
    (license license:expat)))

(define-public go-github-com-dave-brenda
  (package
    (name "go-github-com-dave-brenda")
    (version "1.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/dave/brenda")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1bcc06ifb9hg0g4qqfrqai1m2r4n1gwix3jaydjzq38c7zwcyd1v"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/dave/brenda"))
    (propagated-inputs
     (list go-github-com-dave-astrid
           go-github-com-pkg-errors))
    (home-page "https://github.com/dave/brenda")
    (synopsis "Brenda")
    (description "Package brenda is a boolean expression solver for Go AST")
    (license license:expat)))

(define-public go-github-com-dave-courtney
  (package
    (name "go-github-com-dave-courtney")
    (version "0.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/dave/courtney")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1x0a9yyfksmwysaf43crkzv4lmjj6nz147ldkbh496x6jfr7csfs"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f      ; Probably needs an older x-tools
       #:import-path "github.com/dave/courtney"))
    (propagated-inputs
     (list go-golang-org-x-tools
           go-github-com-pkg-errors
           go-github-com-dave-patsy
           go-github-com-dave-brenda
           go-github-com-dave-astrid))
    (home-page "https://github.com/dave/courtney")
    (synopsis "Courtney")
    (description
     "Courtney makes your code coverage more meaningful, by excluding some of the less
important parts.")
    (license license:expat)))

(define-public go-github-com-dave-gopackages
  (package
    (name "go-github-com-dave-gopackages")
    (version "0.0.0-20170318123100-46e7023ec56e")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/dave/gopackages")
                     (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "14g8w2vq5jb2w3f14vw5xw1zsn96wiz6iyi3ibfygl4y32zi1pfr"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/dave/gopackages"))
    (propagated-inputs
     (list go-github-com-dave-kerr))
    (native-inputs
     (list go-github-com-dave-ktest))
    (home-page "https://github.com/dave/gopackages")
    (synopsis #f)
    (description #f)
    (license license:expat)))

(define-public go-github-com-dave-kerr
  (package
    (name "go-github-com-dave-kerr")
    (version "0.0.0-20170318121727-bc25dd6abe8e")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/dave/kerr")
                     (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "14s5pc9anmvyzmpd8kmlf7wnaqp2kd5ip7iqyks3wqikaqj32pqn"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f      ; No clue
       #:import-path "github.com/dave/kerr"))
    (native-inputs
     (list go-github-com-dave-ktest))
    (home-page "https://github.com/dave/kerr")
    (synopsis #f)
    (description #f)
    (license license:expat)))

(define-public go-github-com-dave-ktest
  (package
    (name "go-github-com-dave-ktest")
    (version "1.1.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/dave/ktest")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1l3z0ggdcjspfmm6k9glmh52a9x50806k6yldxql73p4bpynsd9g"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/dave/ktest"))
    (propagated-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/dave/ktest")
    (synopsis "Testify - Thou Shalt Write Tests")
    (description
     "Package testify is a set of packages that provide many tools for testifying that
your code will behave as you intend.")
    (license (list license:expat license:expat))))

(define-public go-github-com-dave-patsy
  (package
    (name "go-github-com-dave-patsy")
    (version "0.0.0-20210517141501-957256f50cba")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/dave/patsy")
                     (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1f4xc95yr2bjrpnncj1ffiwghmxkx5bjd10082xai8npqiayy30w"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f      ; fails to import test library
       #:import-path "github.com/dave/patsy"))
    (propagated-inputs
     (list go-github-com-pkg-errors))
    (home-page "https://github.com/dave/patsy")
    (synopsis "Patsy")
    (description
     "package patsy is a package helper utility.  It allows the conversion of go
package paths to filesystem directories and vice versa.")
    (license license:expat)))

(define-public go-github-com-dave-rebecca
  (package
    (name "go-github-com-dave-rebecca")
    (version "0.9.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/dave/rebecca")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1k3y825z02wixxjyamkl63syi2qydsnv49wpgidf0jycykg6ndcv"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/dave/rebecca"))
    (home-page "https://github.com/dave/rebecca")
    (synopsis "Rebecca")
    (description "Package rebecca is a readme generator.")
    (license license:expat)))

(define-public go-github-com-denisenkom-go-mssqldb
  (package
    (name "go-github-com-denisenkom-go-mssqldb")
    (version "0.11.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/denisenkom/go-mssqldb")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0gh1k0sp4ka1cnqqmhhs5d5c0gp4xsv4pfy6azky5djpmsrwnb57"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f      ; Tests want network access
       #:import-path "github.com/denisenkom/go-mssqldb"))
    (propagated-inputs
     (list go-golang-org-x-crypto
           go-github-com-golang-sql-civil))
    (home-page "https://github.com/denisenkom/go-mssqldb")
    (synopsis "A pure Go MSSQL driver for Go's database/sql package")
    (description
      "package mssql implements the TDS protocol used to connect to MS SQL Server
(sqlserver) database servers.")
    (license license:bsd-3)))

(define-public go-github-com-dgrijalva-jwt-go
  (package
    (name "go-github-com-dgrijalva-jwt-go")
    (version "3.2.0+incompatible")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/dgrijalva/jwt-go")
               (commit (string-append "v" (go-version->git-ref version)))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "08m27vlms74pfy5z79w67f9lk9zkx6a9jd68k3c4msxy75ry36mp"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/dgrijalva/jwt-go"))
    (home-page "https://github.com/dgrijalva/jwt-go")
    (synopsis "jwt-go")
    (description
      "Package jwt is a Go implementation of JSON Web Tokens:
@url{http://self-issued.info/docs/draft-jones-json-web-token.html,http://self-issued.info/docs/draft-jones-json-web-token.html}")
    (license license:expat)))

(define-public go-github-com-djherbis-buffer
  (package
    (name "go-github-com-djherbis-buffer")
    (version "1.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/djherbis/buffer")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "17m6la583p9yskcj3bmhnazj8j4v8bmfjjp0kkv8i0zhqmcm0wmq"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/djherbis/buffer"))
    (home-page "https://github.com/djherbis/buffer")
    (synopsis "Buffer")
    (description
      "Package buffer implements a series of Buffers which can be composed to implement
complicated buffering strategies")
    (license license:expat)))

(define-public go-github-com-djherbis-nio-v3
  (package
    (name "go-github-com-djherbis-nio-v3")
    (version "3.0.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/djherbis/nio")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "06zd92m0p4hd6mkrp3ya043p4f9f1hhqwvcl69hxmdr1an39b699"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/djherbis/nio/v3"))
    (propagated-inputs
     (list go-github-com-djherbis-buffer))
    (home-page "https://github.com/djherbis/nio")
    (synopsis "nio")
    (description "Package nio provides a few buffered io primitives.")
    (license license:expat)))

;; Keybase needs this older version
(define-public go-github-com-dustin-go-humanize-20150824
  (package
    (inherit go-github-com-dustin-go-humanize)
    (name "go-github-com-dustin-go-humanize")
    (version "0.0.0-20150824013810-c20a8bde38c8")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/dustin/go-humanize")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0543bdyvg6x45wbiz6v13vl48wh0l61mk65ac0ha8p8sfvajglx3"))))))

(define-public go-github-com-eapache-channels
  (package
    (name "go-github-com-eapache-channels")
    (version "1.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/eapache/channels")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "179ryd2rqsccnv5nk35f8j4nfbqr8cgb2bjm0j8isvf5nzks8s9y"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/eapache/channels"))
    (propagated-inputs
     `(("go-github-com-eapache-queue" ,go-github-com-eapache-queue)))
    (home-page "https://github.com/eapache/channels")
    (synopsis "channels")
    (description #f)
    (license license:expat)))

(define-public go-github-com-eapache-queue
  (package
    (name "go-github-com-eapache-queue")
    (version "1.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/eapache/queue")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "07dp54n94gn3gsvdcki56yqh7py7wqqigxbamhxwgbr05n61fqyg"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/eapache/queue"))
    (home-page "https://github.com/eapache/queue")
    (synopsis "Queue")
    (description #f)
    (license license:expat)))

(define-public go-github-com-editorconfig-editorconfig-core-go-v2
  (package
    (name "go-github-com-editorconfig-editorconfig-core-go-v2")
    (version "2.4.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/editorconfig/editorconfig-core-go")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "15j8gq2kxgccwkjaa1yyam1z4lghm32q638w43c6ghn6mhmvxsxf"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/editorconfig/editorconfig-core-go/v2"))
    (propagated-inputs
     (list go-gopkg-in-ini-v1
           go-golang-org-x-mod
           go-github-com-google-go-cmp))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/editorconfig/editorconfig-core-go")
    (synopsis "Editorconfig Core Go")
    (description
      "This package provides a @url{https://editorconfig.org/,Editorconfig} file parser
and manipulator for Go.")
    (license license:expat)))

(define-public go-github-com-envoyproxy-go-control-plane
  (package
    (name "go-github-com-envoyproxy-go-control-plane")
    (version "0.10.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/envoyproxy/go-control-plane")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0amjw4x1904r14ps07l3wi5vdph5v2m9c97kkrr567kxr5xpjsv3"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/envoyproxy/go-control-plane"))
    (propagated-inputs
     (list ;go-google-golang-org-protobuf
        ;go-google-golang-org-grpc
        ;go-google-golang-org-genproto
        ;go-golang-org-x-sys
        ;go-golang-org-x-net
        ;go-go-opentelemetry-io-proto-otlp
        ;go-github-com-stretchr-testify
        ;go-github-com-prometheus-client-model
        ;go-github-com-google-go-cmp
        ;go-github-com-golang-protobuf
        ;go-github-com-envoyproxy-protoc-gen-validate
        ;go-github-com-cncf-xds-go
        ;go-github-com-census-instrumentation-opencensus-proto
        ))
    (home-page "https://github.com/envoyproxy/go-control-plane")
    (synopsis "control-plane")
    (description
      "This repository contains a Go-based implementation of an API server that
implements the discovery service APIs defined in
@url{https://github.com/envoyproxy/data-plane-api,data-plane-api}.")
    (license license:asl2.0)))

(define-public go-github-com-envoyproxy-protoc-gen-validate
  (package
    (name "go-github-com-envoyproxy-protoc-gen-validate")
    (version "0.6.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/envoyproxy/protoc-gen-validate")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "15n0iimdvirxmd1kyysss8fcnlds316dzh3rfzmcz4k3ip26npw5"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/envoyproxy/protoc-gen-validate"))
    (propagated-inputs
      `(;("go-google-golang-org-protobuf" ,go-google-golang-org-protobuf)
        ;("go-golang-org-x-tools" ,go-golang-org-x-tools)
        ;("go-golang-org-x-text" ,go-golang-org-x-text)
        ;("go-golang-org-x-sys" ,go-golang-org-x-sys)
        ;("go-golang-org-x-net" ,go-golang-org-x-net)
        ;("go-golang-org-x-mod" ,go-golang-org-x-mod)
        ;("go-golang-org-x-lint" ,go-golang-org-x-lint)
        ;("go-github-com-spf13-afero" ,go-github-com-spf13-afero)
        ;("go-github-com-lyft-protoc-gen-star" ,go-github-com-lyft-protoc-gen-star)
        ;("go-github-com-iancoleman-strcase" ,go-github-com-iancoleman-strcase)
        ))
    (home-page "https://github.com/envoyproxy/protoc-gen-validate")
    (synopsis "protoc-gen-validate (PGV)")
    (description
      "PGV is a protoc plugin to generate polyglot message validators.  While protocol
buffers effectively guarantee the types of structured data, they cannot enforce
semantic rules for values.  This plugin adds support to protoc-generated code to
validate such constraints.")
    (license license:asl2.0)))

(define-public go-github-com-ethantkoenig-rupture
  (package
    (name "go-github-com-ethantkoenig-rupture")
    (version "1.0.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ethantkoenig/rupture")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1qc2rv4i1292f1fw3mfvf6zn9wy4nvbj6dla4lycdxdqvv066pd8"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/ethantkoenig/rupture"))
    (propagated-inputs
     (list go-github-com-blevesearch-bleve-index-api
           go-github-com-blevesearch-bleve-v2))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/ethantkoenig/rupture")
    (synopsis "rupture")
    (description
     "An explosive companion to the
@url{https://www.github.com/blevesearch/bleve,bleve indexing library}")
    (license license:expat)))

(define-public go-github-com-evanphx-json-patch-v4
  (package
    (name "go-github-com-evanphx-json-patch")
    (version "4.12.0+incompatible")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/evanphx/json-patch")
                    (commit (string-append "v" (go-version->git-ref version)))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1z0bmsvzm4nchfbi7h9pdvkfgrnf0fvhn39pgb0q2az8cql58q56"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/evanphx/json-patch"))
    (propagated-inputs
     (list go-github-com-pkg-errors))
    (home-page "https://github.com/evanphx/json-patch")
    (synopsis "JSON-Patch")
    (description
     "@@code{jsonpatch} is a library which provides functionality for both applying
@@url{http://tools.ietf.org/html/rfc6902,RFC6902 JSON patches} against
documents, as well as for calculating & applying
@@url{https://tools.ietf.org/html/rfc7396,RFC7396 JSON merge patches}.")
    (license license:bsd-3)))

(define-public go-github-com-evanphx-json-patch-v5
  (package
    (name "go-github-com-evanphx-json-patch-v5")
    (version "5.6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/evanphx/json-patch")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0xrqha67ps24wmzx4yv87pkl25hk4v7lvcga0f18928jzzk4wbvk"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/evanphx/json-patch/v5"))
    (propagated-inputs `(("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
                         ("go-github-com-jessevdk-go-flags" ,go-github-com-jessevdk-go-flags)))
    (home-page "https://github.com/evanphx/json-patch")
    (synopsis #f)
    (description #f)
    (license license:bsd-3)))

(define-public go-github-com-fanliao-go-promise
  (package
    (name "go-github-com-fanliao-go-promise")
    (version "0.0.0-20141029170127-1890db352a72")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/fanliao/go-promise")
                     (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0nlls86fx6sxsvwp5k769h5knwh96j8fahhivh6fagzjjyyqcijd"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/fanliao/go-promise"))
    (native-inputs
     (list go-github-com-smartystreets-goconvey))
    (home-page "https://github.com/fanliao/go-promise")
    (synopsis "Installation")
    (description
     "Package promise provides a complete promise and future implementation.  A quick
start sample:")
    (license license:expat)))

(define-public go-github-com-flynn-go-shlex
  (package
    (name "go-github-com-flynn-go-shlex")
    (version "0.0.0-20150515145356-3f9db97f8568")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/flynn-archive/go-shlex")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1j743lysygkpa2s2gii2xr32j7bxgc15zv4113b0q9jhn676ysia"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/flynn/go-shlex"))
    (home-page "https://github.com/flynn/go-shlex")
    (synopsis #f)
    (description
      "go-shlex is a simple lexer for go that supports shell-style quoting, commenting,
and escaping.")
    (license license:asl2.0)))

(define-public go-github-com-fortytw2-leaktest
  (package
    (name "go-github-com-fortytw2-leaktest")
    (version "1.3.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/fortytw2/leaktest")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0487zghyxqzk6zdbhd2j074pcc2l15l4sfg5clrjqwfbql7519wx"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/fortytw2/leaktest"))
    (home-page "https://github.com/fortytw2/leaktest")
    (synopsis "Leaktest")
    (description
      "Package leaktest provides tools to detect leaked goroutines in tests.  To use
it, call \"defer leaktest.Check(t)()\" at the beginning of each test that may use
goroutines.  copied out of the cockroachdb source tree with slight modifications
to be more re-useable")
    (license license:bsd-3)))

(define-public go-github-com-gammazero-deque
  (package
    (name "go-github-com-gammazero-deque")
    (version "0.0.0-20201010052221-3932da5530cc")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/gammazero/deque")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1wc4b8li1hsxggm5f9x7km9jmplnkchxp6wkhx1ljnxnrxzs5h09"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/gammazero/deque"))
    (home-page "https://github.com/gammazero/deque")
    (synopsis "deque")
    (description #f)
    (license license:expat)))

(define-public go-github-com-gammazero-workerpool
  (package
    (name "go-github-com-gammazero-workerpool")
    (version "1.1.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/gammazero/workerpool")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "121dwkcx1azpn5xqjfn277kg063hx4wfns3jn1vjbvykws2h0ma7"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/gammazero/workerpool"))
    (propagated-inputs
     `(("go-github-com-gammazero-deque" ,go-github-com-gammazero-deque)))
    (home-page "https://github.com/gammazero/workerpool")
    (synopsis "workerpool")
    (description #f)
    (license license:expat)))

(define-public go-github-com-gavv-monotime
  (package
    (name "go-github-com-gavv-monotime")
    (version "0.0.0-20190418164738-30dba4353424")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/gavv/monotime")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0w67yyc9y11dp7lp4b712dkcgbiln1qmgfx1nbbrw3mfkzr61d7g"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/gavv/monotime"))
    (home-page "https://github.com/gavv/monotime")
    (synopsis "monotime")
    (description #f)
    (license license:asl2.0)))

(define-public go-github-com-ghodss-yaml
  (package
    (name "go-github-com-ghodss-yaml")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ghodss/yaml")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0skwmimpy7hlh7pva2slpcplnm912rp3igs98xnqmn859kwa5v8g"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/ghodss/yaml"))
    (propagated-inputs
     (list go-gopkg-in-yaml-v2))
    (home-page "https://github.com/ghodss/yaml")
    (synopsis "YAML marshaling and unmarshaling support for Go")
    (description
      "Copyright 2013 The Go Authors.  All rights reserved.  Use of this source code is
governed by a BSD-style license that can be found in the LICENSE file.")
    (license license:expat)))

(define-public go-github-com-glendc-gopher-json
  (package
    (name "go-github-com-glendc-gopher-json")
    (version "0.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/GlenDC/gopher-json")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1hvam978ls0768smwfywwfg2dy816bfifch4hdwwbsx2d59zpphs"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f      ; cannot encode recursively nested tables to JSON
       #:import-path "github.com/glendc/gopher-json"))
    (propagated-inputs
     (list go-github-com-yuin-gopher-lua))
    (home-page "https://github.com/glendc/gopher-json")
    (synopsis "JSON encoder/decoder for gopher-lua")
    (description
     "Package json is a simple JSON encoder/decoder for gopher-lua.")
    (license license:unlicense)))

(define-public go-github-com-glycerine-goconvey
  (package
    (name "go-github-com-glycerine-goconvey")
    (version "0.0.0-20190410193231-58a59202ab31")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/glycerine/goconvey")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "014cj82z42gaw863nrfmw39ff1z103n0aj5bs1fpx2iawgw5i6qh"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/glycerine/goconvey"))
    (home-page "https://github.com/glycerine/goconvey")
    (synopsis "GoConvey is awesome Go testing")
    (description #f)
    (license license:expat)))

(define-public go-github-com-glycerine-go-unsnap-stream
  (package
    (name "go-github-com-glycerine-go-unsnap-stream")
    (version "0.0.0-20210130063903-47dfef350d96")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/glycerine/go-unsnap-stream")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1sqpjify17sjff92f8biwb9vnn443shk3zr9myzm8qscjpzkby3w"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/glycerine/go-unsnap-stream"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'patch-source
           (lambda* (#:key import-path #:allow-other-keys)
             (substitute* (string-append "src/" import-path "/unsnap_test.go")
               (("/usr/bin/diff") (which "diff"))))))))
    (propagated-inputs
     (list go-github-com-golang-snappy))
    (native-inputs
     (list go-github-com-glycerine-goconvey
           go-github-com-jtolds-gls))
    (home-page "https://github.com/glycerine/go-unsnap-stream")
    (synopsis "go-unsnap-stream")
    (description
     "This is a small golang library for decoding and encoding the snappy  format,
specified here:
@url{https://github.com/google/snappy/blob/master/framing_format.txt,https://github.com/google/snappy/blob/master/framing_format.txt}")
    (license license:expat)))

(define-public go-github-com-go-chi-chi
  (package
    (name "go-github-com-go-chi-chi")
    (version "1.5.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/go-chi/chi")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1jpa4r5h15gkpfmb6xq1hamv0q20i8bdpw3kh7dw4n1v7pshjsr8"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/go-chi/chi"))
    (home-page "https://github.com/go-chi/chi")
    (synopsis #f)
    (description
     "Package chi is a small, idiomatic and composable router for building HTTP
services.")
    (license license:expat)))

(define-public go-github-com-go-chi-cors
  (package
    (name "go-github-com-go-chi-cors")
    (version "1.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/go-chi/cors")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0p566jkmb88v8gqknphzqs5p1hznk5y6n0qc3zwcfc88vf584vh9"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/go-chi/cors"))
    (home-page "https://github.com/go-chi/cors")
    (synopsis "CORS net/http middleware")
    (description
     "cors package is net/http handler to handle CORS related requests as defined by
@url{http://www.w3.org/TR/cors/,http://www.w3.org/TR/cors/}")
    (license license:expat)))

(define-public go-github-com-go-ole-go-ole
  (package
    (name "go-github-com-go-ole-go-ole")
    (version "1.2.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/go-ole/go-ole")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0aswlz7dr6v0if6bdwj3ivawj8cql2hgp84yymsq3ic9nys6537s"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/go-ole/go-ole"))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://github.com/go-ole/go-ole")
    (synopsis "Go OLE")
    (description
     "Go bindings for Windows COM using shared libraries instead of cgo.")
    (license license:expat)))

(define-public go-github-com-gocolly-colly-debug
  (package
    (name "go-github-com-gocolly-colly-debug")
    (version "1.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/gocolly/colly")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1a6brjy0a4pwq2ml3fvz6p7wjmg37rh006i00zxgv9v4vmv7b84d"))))
    (build-system go-build-system)
    (arguments
     '(#:unpack-path "github.com/gocolly/colly"
       #:import-path "github.com/gocolly/colly/debug"))
    (home-page "https://github.com/gocolly/colly")
    (synopsis "Colly")
    (description #f)
    (license license:asl2.0)))

(define-public go-github-com-gocolly-colly-storage
  (package
    (inherit go-github-com-gocolly-colly-debug)
    (name "go-github-com-gocolly-colly-storage")
    (arguments
     '(#:unpack-path "github.com/gocolly/colly"
       #:import-path "github.com/gocolly/colly/storage"))
    (synopsis "Colly")
    (description #f)))

(define-public go-github-com-go-enry-go-enry-v2
  (package
    (name "go-github-com-go-enry-go-enry-v2")
    (version "2.8.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/go-enry/go-enry")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1dqmnay5z015192sl343wlx6nys0v91rn2lch728swxhp2qxrkx5"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f      ; TODO: Fix
       #:import-path "github.com/go-enry/go-enry/v2"))
    (propagated-inputs
     (list go-gopkg-in-yaml-v2
           go-github-com-go-enry-go-oniguruma))
    (native-inputs
     (list git-minimal
           go-github-com-stretchr-testify))
    (home-page "https://github.com/go-enry/go-enry")
    (synopsis "go-enry")
    (description
     "Package enry implements multiple strategies for programming language
identification.")
    (license license:asl2.0)))

(define-public go-github-com-go-enry-go-oniguruma
  (package
    (name "go-github-com-go-enry-go-oniguruma")
    (version "1.2.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/go-enry/go-oniguruma")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1wlgs5qms988f4q1h30c08f3w7jlnz76dlkp2shf02prgv4qv00f"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/go-enry/go-oniguruma"))
    (inputs
     (list oniguruma))
    (home-page "https://github.com/go-enry/go-oniguruma")
    (synopsis "go-oniguruma")
    (description
     "This repository is a fork of
@url{https://github.com/moovweb/rubex/tree/go1,moovweb/rubex} - a simple regular
expression library (based on @url{https://github.com/kkos/oniguruma,oniguruma})
that supports Ruby's regex syntax.")
    (license license:expat)))

(define-public go-github-com-go-fed-httpsig
  (package
    (name "go-github-com-go-fed-httpsig")
    (version "1.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/go-fed/httpsig")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1h2yk2ih8vrma8zrs1z8bd4r48hbqdwhgbqykrs4siyj9c80ykd2"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f              ; TODO: Fix
       #:import-path "github.com/go-fed/httpsig"))
    (propagated-inputs
     (list go-golang-org-x-crypto))
    (home-page "https://github.com/go-fed/httpsig")
    (synopsis "httpsig")
    (description
     "This package implements HTTP request and response signing and verification.
Supports the major MAC and asymmetric key signature algorithms.  It has several
safety restrictions: One, none of the widely known non-cryptographically safe
algorithms are permitted; Two, the RSA SHA256 algorithms must be available in
the binary (and it should, barring export restrictions); Finally, the library
assumes either the Authorizationn or Signature headers are to be set (but not
both).")
      (license license:bsd-3)))

(define-public go-github-com-gogs-cron
  (package
    (name "go-github-com-gogs-cron")
    (version "0.0.0-20171120032916-9f6c956d3e14")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/gogs/cron")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "06nxf8c5rxjjzprpdyiq2pyhckqhgn6ad22hmrxmzyd7z6y34xcj"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/gogs/cron"))
    (home-page "https://github.com/gogs/cron")
    (synopsis #f)
    (description "Package cron implements a cron spec parser and job runner.")
    (license license:expat)))

(define-public go-github-com-gogs-go-gogs-client
  (package
    (name "go-github-com-gogs-go-gogs-client")
    (version "0.0.0-20210131175652-1d7215cd8d85")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/gogs/go-gogs-client")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "11b8mwdly960z7jdx297jywxzljs0l0sl3i1qvii1fjzis7k608l"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/gogs/go-gogs-client"))
    (home-page "https://github.com/gogs/go-gogs-client")
    (synopsis "Gogs API client in Go")
    (description
      "This package is still in experiment, see
@url{https://github.com/gogits/go-gogs-client/wiki,Wiki} for documentation.")
    (license license:expat)))

(define-public go-github-com-golang-mock-gomock
  (package
    (inherit go-github-com-golang-mock)
    (name "go-github-com-golang-mock-gomock")
    (arguments
     '(#:unpack-path "github.com/golang/mock"
       #:import-path "github.com/golang/mock/gomock"))))

(define-public go-github-com-golang-sql-civil
  (package
    (name "go-github-com-golang-sql-civil")
    (version "0.0.0-20190719163853-cb61b32ac6fe")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/golang-sql/civil")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0yadfbvi0w06lg3sxw0daji02jxd3vv2in26yfmwpl4vd4vm9zay"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/golang-sql/civil"))
    (home-page "https://github.com/golang-sql/civil")
    (synopsis "Civil Date and Time")
    (description
      "Package civil implements types for civil time, a time-zone-independent
representation of time that follows the rules of the proleptic Gregorian
calendar with exactly 24-hour days, 60-minute hours, and 60-second minutes.")
    (license license:asl2.0)))

(define-public go-github-com-googleapis-gax-go-v2
  (package
    (name "go-github-com-googleapis-gax-go-v2")
    (version "2.1.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/googleapis/gax-go")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0y959pdc2yqajhk5wxdsvfjkz3pb5ppi6yrcmpy2dkviwhx2kz73"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/googleapis/gax-go/v2"))
    (propagated-inputs
      `(;("go-google-golang-org-protobuf" ,go-google-golang-org-protobuf)
        ;("go-google-golang-org-grpc" ,go-google-golang-org-grpc)
        ;("go-google-golang-org-genproto" ,go-google-golang-org-genproto)
        ;("go-google-golang-org-api" ,go-google-golang-org-api)
        ;("go-github-com-google-go-cmp" ,go-github-com-google-go-cmp)
        ))
    (home-page "https://github.com/googleapis/gax-go")
    (synopsis #f)
    (description
      "Package gax contains a set of modules which aid the development of APIs for
clients and servers based on gRPC and Google API conventions.")
    (license license:bsd-3)))

(define-public go-github-com-google-gnostic
  (package
    (name "go-github-com-google-gnostic")
    (version "0.5.7-v3refs")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/google/gnostic")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ma3mdlhbdalxlc0l82j4lsynqz19d7hp09rim3npbkj9y4wgq4i"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/google/gnostic"
       #:phases
       (modify-phases %standard-phases
         (delete 'build)
         (delete 'check))))
    (propagated-inputs `(("go-gopkg-in-yaml-v3" ,go-gopkg-in-yaml-v3)
                         ("go-gopkg-in-check-v1" ,go-gopkg-in-check-v1)
                         ("go-google-golang-org-protobuf" ,go-google-golang-org-protobuf)
                         ("go-google-golang-org-genproto" ,go-google-golang-org-genproto)
                         ("go-golang-org-x-xerrors" ,go-golang-org-x-xerrors)
                         ("go-golang-org-x-tools" ,go-golang-org-x-tools)
                         ;("go-github-com-stoewer-go-strcase" ,go-github-com-stoewer-go-strcase)
                         ("go-github-com-kr-pretty" ,go-github-com-kr-pretty)
                         ("go-github-com-golang-protobuf" ,go-github-com-golang-protobuf)
                         ("go-github-com-docopt-docopt-go" ,go-github-com-docopt-docopt-go)
                         ("go-github-com-davecgh-go-spew" ,go-github-com-davecgh-go-spew)))
    (home-page "https://github.com/google/gnostic")
    (synopsis "⨁ gnostic")
    (description
     "Gnostic is a tool for building better REST APIs through knowledge.")
    (license license:asl2.0)))

(define-public go-github-com-google-go-github-v32
  (package
    (name "go-github-com-google-go-github-v32")
    (version "32.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/google/go-github")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0qaxcm2p655r1jd59rv1hd58driadw5hxlfy7h53c7pzcsmf2546"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/google/go-github/v32"
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key import-path build-flags #:allow-other-keys)
             (for-each
               (lambda (directory)
                 ((assoc-ref %standard-phases 'build)
                  #:build-flags build-flags
                  #:import-path (string-append "github.com/google/go-github/v32/" directory)))
               (list "github"
                     ;"scrape"      ; Wants go-github-com-google-go-github-v28
                     "test/fields"
                     "test/integration"
                     "update-urls"))))
         (replace 'check
           (lambda* (#:key tests? import-path #:allow-other-keys)
             (for-each
               (lambda (directory)
                 ((assoc-ref %standard-phases 'check)
                  #:tests? tests?
                  #:import-path (string-append "github.com/google/go-github/v32/" directory)))
               (list "github"
                     ;"scrape"      ; Wants go-github-com-google-go-github-v28
                     "test/fields"
                     "test/integration"
                     "update-urls"))))
         (add-after 'unpack 'disable-network-tests
           (lambda _
             (substitute* "src/github.com/google/go-github/v32/github/repos_releases_test.go"
               (("TestRepositoriesService_UploadReleaseAsset" all)
                (string-append "Disabled" all))))))))
    (propagated-inputs
     (list go-google-golang-org-appengine
           go-golang-org-x-oauth2
           go-golang-org-x-net
           go-golang-org-x-crypto
           go-github-com-google-go-querystring
           go-github-com-golang-protobuf))
    (native-inputs
     (list go-github-com-pmezard-go-difflib))
    (home-page "https://github.com/google/go-github")
    (synopsis "go-github")
    (description
      "go-github is a Go client library for accessing the
@url{https://developer.github.com/v3/,GitHub API v3}.")
    (license license:bsd-3)))

(define-public go-github-com-google-martian-v3
  (package
    (name "go-github-com-google-martian-v3")
    (version "3.2.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/google/martian")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0ylsicpiaprq6yvgbl4qiclvj4xsnsmjsjmyi21rqgxhnvyjbfyf"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/google/martian/v3"))
    (propagated-inputs
      `(;("go-google-golang-org-protobuf" ,go-google-golang-org-protobuf)
        ;("go-google-golang-org-grpc-cmd-protoc-gen-go-grpc" ,go-google-golang-org-grpc-cmd-protoc-gen-go-grpc)
        ;("go-google-golang-org-grpc" ,go-google-golang-org-grpc)
        ;("go-golang-org-x-net" ,go-golang-org-x-net)
        ;("go-github-com-golang-snappy" ,go-github-com-golang-snappy)
        ;("go-github-com-golang-protobuf" ,go-github-com-golang-protobuf)
        ))
    (home-page "https://github.com/google/martian")
    (synopsis "Martian Proxy")
    (description
      "Package martian provides an HTTP/1.1 proxy with an API for configurable request
and response modifiers.")
    (license license:asl2.0)))

(define-public go-github-com-google-uuid-1.3
  (package
    (name "go-github-com-google-uuid")
    (version "1.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/google/uuid")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0imkw52m7fzrwsdj2rfrk3zbplqfbwncyv6hv89xw0vdw3jpk122"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/google/uuid"))
    (home-page "https://github.com/google/uuid")
    (synopsis "uuid")
    (description "Package uuid generates and inspects UUIDs.")
    (license license:bsd-3)))

(define-public go-github-com-go-openapi-inflect
  (package
    (name "go-github-com-go-openapi-inflect")
    (version "0.19.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/go-openapi/inflect")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1bsv7cb9ylkgglcn5nk99v417c1120523v2pgp5nqir8sgsplbwd"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/go-openapi/inflect"))
    (home-page "https://github.com/go-openapi/inflect")
    (synopsis #f)
    (description #f)
    (license license:expat)))

(define-public go-github-com-gopherjs-gopherjs
  (package
    (name "go-github-com-gopherjs-gopherjs")
    (version "0.0.0-20211228203721-be292294a697")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/gopherjs/gopherjs")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1qk6yv295kn2lf881js1q17r9fiixz9h0mvhs7bkpcanvy7vnqnx"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (delete-file-recursively "compiler/vendor")))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/gopherjs/gopherjs"))
    (propagated-inputs
     (list go-golang-org-x-xerrors
           go-golang-org-x-term
           go-github-com-shurcool-vfsgen
           go-github-com-inconshreveable-mousetrap
           go-golang-org-x-tools
           go-golang-org-x-sys
           go-golang-org-x-sync
           go-golang-org-x-crypto
           go-github-com-spf13-pflag
           go-github-com-spf13-cobra
           go-github-com-shurcool-httpfs
           go-github-com-shurcool-go
           go-github-com-neelance-sourcemap
           go-github-com-neelance-astrewrite
           go-github-com-google-go-cmp
           go-github-com-fsnotify-fsnotify))
    (home-page "https://github.com/gopherjs/gopherjs")
    (synopsis "GopherJS - A compiler from Go to JavaScript")
    (description
      "GopherJS compiles Go code (@url{https://golang.org/,golang.org}) to pure
JavaScript code.  Its main purpose is to give you the opportunity to write
front-end code in Go which will still run in all browsers.")
    (license license:bsd-2)))

(define-public go-github-com-go-redis-redis
  (package
    (name "go-github-com-go-redis-redis")
    (version "6.15.9+incompatible")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/go-redis/redis")
               (commit (string-append "v" (go-version->git-ref version)))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1y13zhv4isf28bq249pz9dp08rb8amyfp2gdbfah09zcmlhjsaki"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f      ; Tests need a running redis server.
       #:import-path "github.com/go-redis/redis"))
    (native-inputs
     (list go-github-com-onsi-ginkgo
           go-github-com-onsi-gomega))
    (home-page "https://github.com/go-redis/redis")
    (synopsis "Redis client for Golang")
    (description "This package implements a Redis client in Golang.")
    (license license:bsd-2)))

(define-public go-github-com-go-redis-redis-v8
  (package
    (name "go-github-com-go-redis-redis-v8")
    (version "8.11.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/go-redis/redis")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1frzg6zywab6nnys1hwphx39ipimpqi8czpjkxb7gcd2pvqs4pnr"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f      ; TODO: fix
       #:import-path "github.com/go-redis/redis/v8"))
    (propagated-inputs
     (list go-github-com-onsi-gomega
           go-github-com-onsi-ginkgo
           go-github-com-google-go-cmp
           go-github-com-dgryski-go-rendezvous
           go-github-com-cespare-xxhash))
    (home-page "https://github.com/go-redis/redis")
    (synopsis "Redis client for Golang")
    (description "Package redis implements a Redis client.")
    (license license:bsd-2)))

(define-public go-github-com-goreleaser-nfpm
  (package
    (name "go-github-com-goreleaser-nfpm")
    (version "1.10.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/goreleaser/nfpm")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0fsgnh9xl9jbakk9hj2gmh8sfkgd3ma01qngfgy067ilvqg5x1zb"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/goreleaser/nfpm"))
    (propagated-inputs
     (list go-mvdan-cc-gofumpt
           go-gopkg-in-yaml-v3
           go-gopkg-in-yaml-v2
           go-golang-org-x-tools
           go-golang-org-x-sys
           go-golang-org-x-net
           go-golang-org-x-mod
           go-golang-org-x-crypto
           go-github-com-xi2-xz
           ;go-github-com-tomarrell-wrapcheck
           ;go-github-com-timakin-bodyclose
           ;go-github-com-tetafro-godot
           ;go-github-com-tdakkota-asciicheck
           go-github-com-stretchr-testify
           go-github-com-stretchr-objx
           go-github-com-spf13-afero
           ;go-github-com-sassoftware-go-rpmutils
           ;go-github-com-quasilyte-regex-syntax
           ;go-github-com-quasilyte-go-ruleguard
           ;go-github-com-polyfloyd-go-errorlint
           go-github-com-op-go-logging
           go-github-com-mitchellh-mapstructure
           ;go-github-com-matoous-godox
           ;go-github-com-jirfag-go-printf-func-name
           ;go-github-com-imdario-mergo
           ;go-github-com-gostaticanalysis-analysisutil
           go-github-com-goreleaser-fileglob
           ;go-github-com-goreleaser-chglog
           ;go-github-com-google-rpmpack
           go-github-com-google-go-cmp
           ;go-github-com-golangci-revgrep
           ;go-github-com-golangci-misspell
           ;go-github-com-golangci-golangci-lint
           ;go-github-com-daixiang0-gci
           ;go-github-com-blakesmith-ar
           go-github-com-alecthomas-units
           go-github-com-alecthomas-template
           go-github-com-alecthomas-kingpin
           go-github-com-microsoft-go-winio
           go-github-com-masterminds-semver-v3
           ;go-github-com-djarvur-go-err113
           ))
    (home-page "https://github.com/goreleaser/nfpm")
    (synopsis "Why")
    (description
     "Package nfpm provides ways to package programs in some linux packaging formats.")
    (license license:expat)))

(define-public go-github-com-gorilla-feeds
  (package
    (name "go-github-com-gorilla-feeds")
    (version "1.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/gorilla/feeds")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1lwqibra4hyzx0jhaz12rfhfnw73bmdf8cn9r51nqidk8k7zf7sg"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/gorilla/feeds"))
    (native-inputs
     (list go-github-com-kr-pretty))
    (home-page "https://github.com/gorilla/feeds")
    (synopsis "gorilla/feeds")
    (description "Syndication (feed) generator library for golang.")
    (license license:bsd-2)))

(define-public go-github-com-gorilla-pat
  (package
    (name "go-github-com-gorilla-pat")
    (version "1.0.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/gorilla/pat")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1z9mk8ism43bc1zm484iaq1cipn64znmzma4sl2gq83nsh4gcw5i"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/gorilla/pat"))
    (propagated-inputs
     (list go-github-com-gorilla-context
           go-github-com-gorilla-mux))
    (home-page "https://github.com/gorilla/pat")
    (synopsis "pat")
    (description
      "Package gorilla/pat is a request router and dispatcher with a pat-like
interface.  It is an alternative to gorilla/mux that showcases how it can be
used as a base for different API flavors.  Package pat is documented at:")
    (license license:bsd-3)))

(define-public go-github-com-gorilla-sessions
  (package
    (name "go-github-com-gorilla-sessions")
    (version "1.2.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/gorilla/sessions")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1zjw2s37yggk9231db0vmgs67z8m3am8i8l4gpgz6fvlbv52baxp"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/gorilla/sessions"))
    (propagated-inputs
     (list go-github-com-gorilla-securecookie))
    (home-page "https://github.com/gorilla/sessions")
    (synopsis "sessions")
    (description
      "Package sessions provides cookie and filesystem sessions and infrastructure for
custom session backends.")
    (license license:bsd-3)))

(define-public go-github-com-go-swagger-go-swagger
  (package
    (name "go-github-com-go-swagger-go-swagger")
    (version "0.28.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/go-swagger/go-swagger")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1q37h3455bk8dwl6p85rjqlyl8sqwqap6da30h9cf8vb1hfkh3q7"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/go-swagger/go-swagger"))
    (propagated-inputs
     (list go-gotest-tools
           go-gopkg-in-yaml-v3
           go-gopkg-in-ini-v1
           go-google-golang-org-appengine
           go-golang-org-x-xerrors
           go-github-com-subosito-gotenv
           go-github-com-spf13-jwalterweatherman
           go-github-com-spf13-afero
           go-github-com-pmezard-go-difflib
           go-github-com-pelletier-go-toml
           go-github-com-opentracing-opentracing-go
           go-github-com-oklog-ulid
           go-github-com-magiconair-properties
           go-github-com-kr-text
           go-github-com-josharian-intern
           go-github-com-inconshreveable-mousetrap
           go-github-com-hashicorp-hcl
           go-github-com-google-go-cmp
           go-github-com-golang-protobuf
           go-github-com-go-openapi-jsonpointer
           go-github-com-docker-go-units
           go-github-com-puerkitobio-urlesc
           go-github-com-puerkitobio-purell
           go-gopkg-in-yaml-v2
           ;go-gopkg-in-square-go-jose-v2
           go-google-golang-org-protobuf
           go-golang-org-x-tools
           go-golang-org-x-text
           go-golang-org-x-sys
           go-golang-org-x-oauth2
           go-golang-org-x-net
           go-golang-org-x-mod
           go-golang-org-x-crypto
           go-go-mongodb-org-mongo-driver
           go-github-com-toqueteos-webbrowser
           go-github-com-spf13-viper
           go-github-com-spf13-pflag
           go-github-com-spf13-cobra
           go-github-com-spf13-cast
           go-github-com-rogpeppe-go-internal
           go-github-com-pquerna-cachecontrol
           go-github-com-pkg-errors
           go-github-com-mitchellh-mapstructure
           go-github-com-mitchellh-go-homedir
           go-github-com-mailru-easyjson
           go-github-com-kr-pretty
           go-github-com-jessevdk-go-flags
           go-github-com-gorilla-handlers
           go-github-com-go-swagger-scan-repo-boundary
           go-github-com-go-stack-stack
           go-github-com-go-openapi-validate
           go-github-com-go-openapi-swag
           go-github-com-go-openapi-strfmt
           go-github-com-go-openapi-spec
           go-github-com-go-openapi-runtime
           go-github-com-go-openapi-loads
           go-github-com-go-openapi-jsonreference
           go-github-com-go-openapi-inflect
           go-github-com-go-openapi-errors
           go-github-com-go-openapi-analysis
           go-github-com-fsnotify-fsnotify
           go-github-com-felixge-httpsnoop
           go-github-com-dgrijalva-jwt-go
           go-github-com-davecgh-go-spew
           go-github-com-coreos-go-oidc
           go-github-com-asaskevich-govalidator))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/go-swagger/go-swagger")
    (synopsis "Swagger 2.0")
    (description
      "Package swagger (2.0) provides a powerful interface to your API")
    (license license:asl2.0)))

(define-public go-github-com-go-swagger-scan-repo-boundary
  (package
    (name "go-github-com-go-swagger-scan-repo-boundary")
    (version "0.0.0-20180623220736-973b3573c013")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/go-swagger/scan-repo-boundary")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0ipqv53s9piq5v5nsjmg8v7pzz4zinv2xkif7h0na84i9pnsccyn"))))
    (build-system go-build-system)
    (arguments
     '(#:unpack-path "github.com/go-swagger/scan-repo-boundary"
       #:import-path "github.com/go-swagger/scan-repo-boundary/makeplans"))
    (home-page "https://github.com/go-swagger/scan-repo-boundary")
    (synopsis "TestRepo")
    (description
      "This is a repo that is used in the tests of the go-swagger project.  It's is
only here to test finding files across repository boundaries.")
    (license license:asl2.0)))

(define-public go-github-com-go-testfixtures-testfixtures-v3
  (package
    (name "go-github-com-go-testfixtures-testfixtures-v3")
    (version "3.6.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/go-testfixtures/testfixtures")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1mcxvr1lhcf8bkwcy5ngrc5l2cfan435vrnm1byy4ifkyw1g9l5k"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/go-testfixtures/testfixtures/v3"))
    (propagated-inputs
     (list go-gopkg-in-yaml-v2
           ;go-golang-org-x-crypto
           ;go-github-com-spf13-pflag
           ;go-github-com-mattn-go-sqlite3
           ;go-github-com-lib-pq
           ;go-github-com-jackc-pgx-v4
           ;go-github-com-go-sql-driver-mysql
           ;go-github-com-denisenkom-go-mssqldb
           ))
    (native-inputs
     (list go-github-com-joho-godotenv))
    (home-page "https://github.com/go-testfixtures/testfixtures")
    (synopsis "testfixtures")
    (description
      "Writing tests is hard, even more when you have to deal with an SQL database.
This package aims to make writing functional tests for web apps written in Go
easier.")
    (license license:expat)))

(define-public go-github-com-grpc-ecosystem-grpc-gateway
  (package
    (name "go-github-com-grpc-ecosystem-grpc-gateway")
    (version "1.16.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/grpc-ecosystem/grpc-gateway")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0dzq1qbgzz2c6vnh8anx0j3py34sd72p35x6s2wrl001q68am5cc"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/grpc-ecosystem/grpc-gateway"))
    (propagated-inputs
      `(;("go-gopkg-in-yaml-v2" ,go-gopkg-in-yaml-v2)
        ;("go-google-golang-org-grpc" ,go-google-golang-org-grpc)
        ;("go-google-golang-org-genproto" ,go-google-golang-org-genproto)
        ;("go-golang-org-x-xerrors" ,go-golang-org-x-xerrors)
        ;("go-golang-org-x-oauth2" ,go-golang-org-x-oauth2)
        ;("go-golang-org-x-net" ,go-golang-org-x-net)
        ("go-github-com-rogpeppe-fastuuid" ,go-github-com-rogpeppe-fastuuid)
        ;("go-github-com-golang-protobuf" ,go-github-com-golang-protobuf)
        ("go-github-com-golang-glog" ,go-github-com-golang-glog)
        ;("go-github-com-ghodss-yaml" ,go-github-com-ghodss-yaml)
        ;("go-github-com-antihax-optional" ,go-github-com-antihax-optional)
        ))
    (home-page "https://github.com/grpc-ecosystem/grpc-gateway")
    (synopsis "grpc-gateway")
    (description
      "The grpc-gateway is a plugin of the Google protocol buffers compiler
@url{https://github.com/protocolbuffers/protobuf,protoc}.  It reads protobuf
service definitions and generates a reverse-proxy server which translates a
RESTful HTTP API into gRPC.  This server is generated according to the
@url{https://github.com/googleapis/googleapis/raw/master/google/api/http.proto#L46,(code
google.api.http)} annotations in your service definitions.")
    (license license:bsd-3)))

(define-public go-github-com-hdevalence-ed25519consensus
  (package
    (name "go-github-com-hdevalence-ed25519consensus")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/hdevalence/ed25519consensus")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0xkp5zpqy7siy4p1l8r45csbmif5jv2m0l528mvsxdwpgid8ajij"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/hdevalence/ed25519consensus"))
    (propagated-inputs
     (list go-filippo-io-edwards25519))
    (home-page "https://github.com/hdevalence/ed25519consensus")
    (synopsis "Ed25519 for consensus-critical contexts")
    (description
     "Package ed25519consensus implements Ed25519 verification according to ZIP215.")
    (license license:bsd-3)))

(define-public go-github-com-hugelgupf-socketpair
  (package
    (name "go-github-com-hugelgupf-socketpair")
    (version "0.0.0-20190730060125-05d35a94e714")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/hugelgupf/socketpair")
                     (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1kc243zdrh8n8wmcylyzam3gcqd6zbqnn9a5liaa4jhvn8rwym3j"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/hugelgupf/socketpair"))
    (home-page "https://github.com/hugelgupf/socketpair")
    (synopsis "socketpair")
    (description
     "Package socketpair provides bidirectionally connected net.Conns.")
    (license license:bsd-3)))

(define-public go-github-com-imkira-go-interpol
  (package
    (name "go-github-com-imkira-go-interpol")
    (version "1.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/imkira/go-interpol")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "180h3pf2p0pch6hmqf45wk7wd87md83d3p122f8ll43x5nja5mph"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/imkira/go-interpol"))
    (home-page "https://github.com/imkira/go-interpol")
    (synopsis "interpol")
    (description #f)
    (license license:expat)))

(define-public go-github-com-inconshreveable-mousetrap
  (package
    (name "go-github-com-inconshreveable-mousetrap")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/inconshreveable/mousetrap")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1mn0kg48xkd74brf48qf5hzp0bc6g8cf5a77w895rl3qnlpfw152"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/inconshreveable/mousetrap"))
    (home-page "https://github.com/inconshreveable/mousetrap")
    (synopsis "mousetrap")
    (description "mousetrap is a tiny library that answers a single question.")
    (license license:asl2.0)))

(define-public go-github-com-insomniacslk-dhcp
  (package
    (name "go-github-com-insomniacslk-dhcp")
    (version "0.0.0-20221215072855-de60144f33f8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/insomniacslk/dhcp")
                     (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1203aiphxvwa1jx6p1d1d6x9ba8x9ad36751hm5p00s2pv71l215"))))
    (build-system go-build-system)
    (arguments
     (list
       #:import-path "github.com/insomniacslk/dhcp"
       #:phases
       #~(modify-phases %standard-phases
           (replace 'build
             (lambda* (#:key import-path build-flags #:allow-other-keys)
               (for-each
                 (lambda (directory)
                   ((assoc-ref %standard-phases 'build)
                    #:build-flags build-flags
                    #:import-path (string-append import-path directory)))
                 (list "/dhcpv4"
                       "/dhcpv6"))))
           (replace 'check
             (lambda* (#:key tests? import-path #:allow-other-keys)
               (for-each
                 (lambda (directory)
                   ((assoc-ref %standard-phases 'check)
                    #:tests? tests?
                    #:import-path (string-append import-path directory)))
                 (list "/dhcpv4"
                       "/dhcpv6"))))
           )))
    (propagated-inputs
     (list go-golang-org-x-sys
           go-golang-org-x-net
           go-github-com-u-root-uio
           go-github-com-stretchr-testify
           go-github-com-smartystreets-goconvey
           go-github-com-mdlayher-raw
           go-github-com-mdlayher-netlink
           go-github-com-mdlayher-ethernet
           go-github-com-jsimonetti-rtnetlink
           go-github-com-hugelgupf-socketpair
           ;go-github-com-fanliao-go-promise
           ))
    (home-page "https://github.com/insomniacslk/dhcp")
    (synopsis "dhcp")
    (description
     "DHCPv4 and DHCPv6 decoding/encoding library with client and server code, written
in Go.")
    (license license:bsd-3)))

(define-public go-github-com-issue9-assert
  (package
    (name "go-github-com-issue9-assert")
    (version "1.5.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/issue9/assert")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1bb5xaiq48xp64rawcaxbg9rivnv6kxgc8jfdi62lawiv1w009d3"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/issue9/assert"))
    (home-page "https://github.com/issue9/assert")
    (synopsis "assert")
    (description
      "Package assert æ\x98¯å¯¹ testing å\x8c\x85ç\x9a\x84ä¸\x80äº\x9bç®\x80å\x8d\x95å\x8c\x85è£\x85")
    (license license:expat)))

(define-public go-github-com-issue9-assert-v2
  (package
    (name "go-github-com-issue9-assert-v2")
    (version "2.0.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/issue9/assert")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0j2n9g88a2qx6j0fskz03ihl49fknlk8cakdvjpqwnl2rf5mr4w1"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/issue9/assert/v2"))
    (home-page "https://github.com/issue9/assert")
    (synopsis "assert")
    (description
      "Package assert æ\x98¯å¯¹ testing å\x8c\x85ç\x9a\x84ä¸\x80äº\x9bç®\x80å\x8d\x95å\x8c\x85è£\x85")
    (license license:expat)))

(define-public go-github-com-issue9-identicon
  (package
    (name "go-github-com-issue9-identicon")
    (version "1.2.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/issue9/identicon")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1zkr3841cv1qh8ivzf202pssmzxwa42qd6k78592na3bfdpb6sal"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/issue9/identicon"))
    (propagated-inputs
     (list go-github-com-issue9-assert-v2))
    (home-page "https://github.com/issue9/identicon")
    (synopsis "identicon")
    (description
      "Package identicon ä¸\x80ä¸ªå\x9fºäº\x8e hash å\x80¼ç\x94\x9fæ\x88\x90é\x9a\x8fæ\x9cºå\x9b¾å\x83\x8fç\x9a\x84å\x8c\x85")
    (license license:expat)))

(define-public go-github-com-jrick-logrotate
  (package
    (name "go-github-com-jrick-logrotate")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jrick/logrotate")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0srl6figwjqpi3nbp7br8sxpmvh4v8lzbny1b4lar4ny0156p5nl"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/jrick/logrotate"))
    (home-page "https://github.com/jrick/logrotate")
    (synopsis "logrotate: slightly better than")
    (description #f)
    (license license:bsd-2)))

(define-public go-github-com-jtolds-gls
  (package
    (name "go-github-com-jtolds-gls")
    (version "4.20.0+incompatible")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jtolio/gls")
               (commit (string-append "v" (go-version->git-ref version)))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1k7xd2q2ysv2xsh373qs801v6f359240kx0vrl0ydh7731lngvk6"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/jtolds/gls"))
    (home-page "https://github.com/jtolds/gls")
    (synopsis "gls")
    (description "Package gls implements goroutine-local storage.")
    (license license:expat)))

(define-public go-github-com-k0kubun-colorstring
  (package
    (name "go-github-com-k0kubun-colorstring")
    (version "0.0.0-20150214042306-9440f1994b88")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/k0kubun/colorstring")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0isskya7ky4k9znrh85crfc2pxwyfz2s8j1a5cbjb8b8zf2v0qbj"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/k0kubun/colorstring"))
    (home-page "https://github.com/k0kubun/colorstring")
    (synopsis "colorstring")
    (description #f)
    (license license:expat)))

(define-public go-github-com-kennygrant-sanitize
  (package
    (name "go-github-com-kennygrant-sanitize")
    (version "1.2.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/kennygrant/sanitize")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "06f2ljnic3215ihzc5px1q25548ijpixhmn4537gf507n1sxg7iq"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/kennygrant/sanitize"))
    (propagated-inputs
     `(("go-golang-org-x-net" ,go-golang-org-x-net)))
    (home-page "https://github.com/kennygrant/sanitize")
    (synopsis "sanitize")
    (description #f)
    (license license:bsd-3)))

(define-public go-github-com-keybase-backoff
  (package
    (name "go-github-com-keybase-backoff")
    (version "1.0.1-0.20160517061000-726b63b835ec")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/keybase/backoff")
                     (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wgcm3607zxm2v62dn0rg3b0zy8mlb98s7sbccfhg00di1611ijz"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/keybase/backoff"))
    (propagated-inputs
     (list go-golang-org-x-net))
    (home-page "https://github.com/keybase/backoff")
    (synopsis "Exponential Backoff")
    (description
     "Package backoff implements backoff algorithms for retrying operations.")
    (license license:expat)))

(define-public go-github-com-keybase-cli
  (package
    (name "go-github-com-keybase-cli")
    (version "1.2.1-0.20191217150554-9323fd7ddfab")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/keybase/cli")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0nbz8lwr1v2adcpivlfgm9350lkvf9dh5jc8zvg0ap1gjy2if2gh"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/keybase/cli"))
    (home-page "https://github.com/keybase/cli")
    (synopsis "cli.go")
    (description
     "Package cli provides a minimal framework for creating and organizing command
line Go applications.  cli is designed to be easy to understand and write, the
most simple cli application can be written as follows:")
    (license license:expat)))

(define-public go-github-com-keybase-clockwork
  (package
    (name "go-github-com-keybase-clockwork")
    (version "0.1.1-0.20161209210251-976f45f4a979")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/keybase/clockwork")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0110rc21h06695hlxg4zp3kp21mbw28bpa4rnl944sdqhjzfnsir"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/keybase/clockwork"))
    (home-page "https://github.com/keybase/clockwork")
    (synopsis "clockwork")
    (description "a simple fake clock for golang")
    (license license:asl2.0)))

(define-public go-github-com-keybase-colly
  (package
    (name "go-github-com-keybase-colly")
    (version "1.1.1-0.20190207010505-9a56fbe6c0e6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/keybase/colly")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "08ynibh4blfp5nlch6ia8z1chj9zfrcscmldggsyyffg2w5pas85"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/keybase/colly"))
    (propagated-inputs
     (list go-github-com-gocolly-colly))
    (home-page "https://github.com/keybase/colly")
    (synopsis "Colly")
    (description "Package colly implements a HTTP scraping framework")
    (license license:asl2.0)))

(define-public go-github-com-keybase-fuse
  (package
    (name "go-github-com-keybase-fuse")
    (version "0.0.0-20210104232444-d36009698767")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/keybase/fuse")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1p0s5xc2bkfbgrlsiyz9cf9b9hppzi978gmjppy9gzjh2jii840k"))
       (snippet
        #~(begin
            (use-modules (guix build utils))
            (substitute* '("mount_linux.go"
                           "unmount_linux.go")
              (("\"fusermount\"") "\"fusermount3\""))))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/keybase/fuse"
       #:phases
       (modify-phases %standard-phases
         (delete 'build)
         (delete 'check))))
    (propagated-inputs
     (list go-golang-org-x-sys
           go-github-com-tv42-httpunix))
    (native-inputs
     `(("fuse" ,(@ (gnu packages linux) fuse))))
    (home-page "https://github.com/keybase/fuse")
    (synopsis "bazil.org/fuse -- Filesystems in Go")
    (description
     "Package fuse enables writing FUSE file systems on Linux, OS X, and FreeBSD.")
    (license license:bsd-3)))

(define-public go-github-com-keybase-go-billy
  (package
    (name "go-github-com-keybase-go-billy")
    (version "3.1.1-0.20180828145748-b5a7b7bc2074+incompatible")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/keybase/go-billy")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0z0xb9ss9gn05az3hwiwg4i9x8s0w7nbia3a5zx0mbkiy4kbls1p"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f  ; Tests expect original project
       #:import-path "github.com/keybase/go-billy"))
    (native-inputs
     (list go-gopkg-in-check-v1))
    (home-page "https://github.com/keybase/go-billy")
    (synopsis "go-billy")
    (description
     "The missing interface filesystem abstraction for Go.  Billy implements an
interface based on the @@code{os} standard library, allowing to develop
applications without dependency on the underlying storage.  Make virtually free
implement an mocks and testing over filesystem operations.")
    (license license:expat)))

(define-public go-github-com-keybase-go-codec
  (package
    (name "go-github-com-keybase-go-codec")
    (version "0.0.0-20180928230036-164397562123")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/keybase/go-codec")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "15jjbc2772jgc1g2sy1a35bl6d3lq0xdfp3zj1kpx30y1fd1553c"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f  ;src/github.com/keybase/go-codec/codec/cbor_test.go:152:4: logT format %v reads arg #4, but call has 3 args
       #:unpack-path "github.com/keybase/go-codec"
       #:import-path "github.com/keybase/go-codec/codec"))
    (home-page "https://github.com/keybase/go-codec")
    (synopsis "go/codec")
    (description
     "This repository contains the @@code{go-codec} library, a High Performance and
Feature-Rich Idiomatic encode/decode and rpc library for")
    (license license:expat)))

(define-public go-github-com-keybase-go-crypto
  (package
    (name "go-github-com-keybase-go-crypto")
    (version "0.0.0-20200123153347-de78d2cb44f4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/keybase/go-crypto")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0prrpv0x7nbq5k6swn2jwypzxa8h4aj5lgyw372n6c8ln34fh9jq"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/keybase/go-crypto"
       #:modules ((guix build go-build-system)
                  (guix build utils)
                  (srfi srfi-1))
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key import-path build-flags #:allow-other-keys)
             (for-each
               (lambda (directory)
                 ((assoc-ref %standard-phases 'build)
                  #:build-flags build-flags
                  #:import-path (string-drop directory 4)))
               (find-files "src/github.com/keybase/go-crypto"
                 (lambda (file stat)
                   (and
                     (eq? (stat:type stat) 'directory)
                     (let ((files (find-files file "\\.go$")))
                       (and
                         (not (null? files))
                         (not (null?
                                (filter-map
                                  (lambda (test-entry)
                                    (not (string-contains test-entry file-name-separator-string)))
                                  (map (lambda (entry)
                                         (string-drop entry (1+ (string-length file))))
                                       files))))))))
                 #:directories? #t))))
         (replace 'check
           (lambda* (#:key tests? import-path #:allow-other-keys)
             (for-each
               (lambda (directory)
                 ((assoc-ref %standard-phases 'check)
                  #:tests? tests?
                  #:import-path (string-drop directory 4)))
               (find-files "src/github.com/keybase/go-crypto"
                 (lambda (file stat)
                   (and
                     (eq? (stat:type stat) 'directory)
                     (let ((files (find-files file "\\.go$")))
                       (and
                         (not (null? files))
                         (not (null?
                                (filter-map
                                  (lambda (test-entry)
                                    (not (string-contains test-entry file-name-separator-string)))
                                  (map (lambda (entry)
                                         (string-drop entry (1+ (string-length file))))
                                       files))))))))
                 #:directories? #t)))))))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://github.com/keybase/go-crypto")
    (synopsis "Go supplementary cryptography libraries")
    (description "This repository holds supplementary Go cryptography libraries
with changes by keybase.")
    (license license:bsd-3)))

(define-public go-github-com-keybase-go-framed-msgpack-rpc
  (package
    (name "go-github-com-keybase-go-framed-msgpack-rpc")
    (version "0.0.0-20211118173254-f892386581e8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/keybase/go-framed-msgpack-rpc")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0w8v7xc3v72xpahrmsm4prankvxs9bhgyf68jf4qa0ypq9dicjm2"))))
    (build-system go-build-system)
    (arguments
     '(#:unpack-path "github.com/keybase/go-framed-msgpack-rpc"
       #:import-path "github.com/keybase/go-framed-msgpack-rpc/rpc"))
    (propagated-inputs `(("go-gopkg-in-yaml-v2" ,go-gopkg-in-yaml-v2)
                         ;("go-github-com-reiver-go-oi" ,go-github-com-reiver-go-oi)
                         ("go-github-com-pmezard-go-difflib" ,go-github-com-pmezard-go-difflib)
                         ("go-github-com-davecgh-go-spew" ,go-github-com-davecgh-go-spew)
                         ("go-golang-org-x-net" ,go-golang-org-x-net)
                         ("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)
                         ("go-github-com-keybase-msgpackzip" ,go-github-com-keybase-msgpackzip)
                         ("go-github-com-keybase-go-codec" ,go-github-com-keybase-go-codec)
                         ("go-github-com-keybase-backoff" ,go-github-com-keybase-backoff)))
    (native-inputs
     (list go-github-com-reiver-go-telnet))
    (home-page "https://github.com/keybase/go-framed-msgpack-rpc")
    (synopsis "go-framed-msgpack-rpc")
    (description "Framed RPC for go.")
    (license license:expat)))

(define-public go-github-com-keybase-go-git
  (package
    (name "go-github-com-keybase-go-git")
    (version "4.0.0-rc9.0.20190209005256-3a78daa8ce8e+incompatible")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/keybase/go-git")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0jc3wpvj27cqdpl0s5g9dcmk8c6l2r4j39h4i696ylxyjkqbs0fd"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/keybase/go-git"
       #:phases
       (modify-phases %standard-phases
         (delete 'build)
         (delete 'check))))
    (home-page "https://github.com/keybase/go-git")
    (synopsis "go-git")
    (description
     "Package git is a low level and highly extensible git client library for reading
repositories from git servers.  It is written in Go from scratch, without any C
dependencies.")
    (license license:expat)))

(define-public go-github-com-keybase-go-jsonw
  (package
    (name "go-github-com-keybase-go-jsonw")
    (version "0.0.0-20200325173637-df90f282c233")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/keybase/go-jsonw")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0k67cwhxwmqi7rbk24lpzqgnvj1zc5kvmhpm9xrsi92vay05cvch"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/keybase/go-jsonw"))
    (home-page "https://github.com/keybase/go-jsonw")
    (synopsis "Json Wrapper for Go")
    (description #f)
    (license license:bsd-3)))

(define-public go-github-com-keybase-go-kext
  (package
    (name "go-github-com-keybase-go-kext")
    (version "0.0.0-20211119181951-8d5d2e919472")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/keybase/go-kext")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1yxym50n7jhcmgllp01h8p5d04f63m3d0f45hvpacsamss0jgx61"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/keybase/go-kext"))
    (home-page "https://github.com/keybase/go-kext")
    (synopsis "Go Kext")
    (description
     "This package provides a library for loading, unloading and viewing info for
kernel extensions on OS X (golang).")
    (license license:expat)))

(define-public go-github-com-keybase-go-keychain
  (package
    (name "go-github-com-keybase-go-keychain")
    (version "0.0.0-20220610143837-c2ce06069005")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/keybase/go-keychain")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jk7aanfrwgshs3h0a15zhjl17fp3jccs3j19z6mrk7xfh65i0wj"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/keybase/go-keychain"))
    (propagated-inputs `(("go-gopkg-in-yaml-v2" ,go-gopkg-in-yaml-v2)
                         ("go-github-com-pmezard-go-difflib" ,go-github-com-pmezard-go-difflib)
                         ("go-github-com-davecgh-go-spew" ,go-github-com-davecgh-go-spew)
                         ("go-golang-org-x-crypto" ,go-golang-org-x-crypto)
                         ("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)
                         ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
                         ("go-github-com-keybase-dbus" ,go-github-com-keybase-dbus)))
    (home-page "https://github.com/keybase/go-keychain")
    (synopsis "Go Keychain")
    (description
     "This package provides a library for accessing the Keychain for macOS, iOS, and
Linux in Go (golang).")
    (license license:expat)))

(define-public go-github-com-keybase-go-logging
  (package
    (name "go-github-com-keybase-go-logging")
    (version "0.0.0-20211118164508-35a15a9fa71a")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/keybase/go-logging")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0gqjwv24gh41bm6im1nlc5qf23w7yjy8cbplrv58l86mdcg0h2y9"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/keybase/go-logging"))
    (home-page "https://github.com/keybase/go-logging")
    (synopsis "Golang logging library")
    (description
     "Package logging implements a logging infrastructure for Go.  It supports
different logging backends like syslog, file and memory.  Multiple backends can
be utilized with different log levels per backend and logger.")
    (license license:bsd-3)))

(define-public go-github-com-keybase-go-merkle-tree
  (package
    (name "go-github-com-keybase-go-merkle-tree")
    (version "0.0.0-20211118173306-f89b06604d00")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/keybase/go-merkle-tree")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1bs7yagq6fzc7k4h4lhhkpjjcqssac85gdfq3rhqfb9asr10wdw1"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/keybase/go-merkle-tree"))
    (propagated-inputs `(("go-golang-org-x-net" ,go-golang-org-x-net)
                         ("go-github-com-keybase-go-codec" ,go-github-com-keybase-go-codec)))
    (home-page "https://github.com/keybase/go-merkle-tree")
    (synopsis "go-merkle-tree")
    (description
     "Package merkleTree is a generic Merkle Tree implementation, for provably
publishing lots of data under one succinct tree root.")
    (license license:bsd-3)))

(define-public go-github-com-keybase-go-porterstemmer
  (package
    (name "go-github-com-keybase-go-porterstemmer")
    (version "1.0.2-0.20181016185745-521f1ed5c3f7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/keybase/go-porterstemmer")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "16aqly34lvlvm99q0zv0jqqgyxlksk0czbc485r0zrcw5ynnmq8q"))))
    (build-system go-build-system)
    (arguments
     ;; porterstemmer_has_suffix_test.go:424:4: Errorf format %d has arg datum.Expected of wrong type bool
     '(#:tests? #f
       #:import-path "github.com/keybase/go-porterstemmer"))
    (home-page "https://github.com/keybase/go-porterstemmer")
    (synopsis "Go Porter Stemmer")
    (description
     "This package provides a native Go clean room implementation of the Porter
Stemming Algorithm.")
    (license license:expat)))

(define-public go-github-com-keybase-go-triplesec
  (package
    (name "go-github-com-keybase-go-triplesec")
    (version "0.0.0-20211109205539-1f96eeacbd86")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/keybase/go-triplesec")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12l0adi8rm7y6lyygjbczpix46npggwj410w597ig9axmli2716x"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/keybase/go-triplesec"))
    (propagated-inputs `(("go-gopkg-in-yaml-v3" ,go-gopkg-in-yaml-v3)
                         ("go-github-com-pmezard-go-difflib" ,go-github-com-pmezard-go-difflib)
                         ("go-github-com-davecgh-go-spew" ,go-github-com-davecgh-go-spew)
                         ("go-golang-org-x-crypto" ,go-golang-org-x-crypto)
                         ("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)
                         ("go-github-com-keybase-go-crypto" ,go-github-com-keybase-go-crypto)))
    (home-page "https://github.com/keybase/go-triplesec")
    (synopsis "TripleSec")
    (description
     "Package triplesec implements the TripleSec v3 and v4 encryption and
authentication scheme.")
    (license license:expat)))

(define-public go-github-com-keybase-go-triplesec-insecure
  (package
    (name "go-github-com-keybase-go-triplesec-insecure")
    (version "0.0.0-20211118164850-99654891ba7c")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/keybase/go-triplesec-insecure")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "064m3a146bc1nhk291a52v7q3l0rfl20s6gzd244wx28z6rzrwrg"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/keybase/go-triplesec-insecure"))
    (propagated-inputs `(("go-golang-org-x-crypto" ,go-golang-org-x-crypto)
                         ("go-github-com-keybase-go-crypto" ,go-github-com-keybase-go-crypto)))
    (home-page "https://github.com/keybase/go-triplesec-insecure")
    (synopsis "TripleSec")
    (description
     "Package triplesec implements the TripleSec v3 and v4 encryption and
authentication scheme.")
    (license license:expat)))

(define-public go-github-com-keybase-go-updater
  (package
    (name "go-github-com-keybase-go-updater")
    (version "0.0.0-20221221215057-da7f21f4d90b")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/keybase/go-updater")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1xkgp7gh61sddbna089hl92l7sdym6js88n5hskaz5clx69sdr5c"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/keybase/go-updater"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'build))))
    (propagated-inputs `(;("go-github-com-keybase-xurls-v2" ,go-github-com-keybase-xurls-v2)
                         ("go-github-com-keybase-go-git" ,go-github-com-keybase-go-git)
                         ("go-github-com-keybase-go-billy" ,go-github-com-keybase-go-billy)
                         ;("go-github-com-keybase-goleveldb" ,go-github-com-keybase-goleveldb)
                         ("go-github-com-keybase-stellar-org" ,go-github-com-keybase-stellar-org)
                         ;("go-github-com-adjust-goautoneg" ,go-github-com-adjust-goautoneg)
                         ("go-github-com-keybase-fuse" ,go-github-com-keybase-fuse)
                         ("go-gopkg-in-yaml-v3" ,go-gopkg-in-yaml-v3)
                         ("go-gopkg-in-check-v1" ,go-gopkg-in-check-v1)
                         ("go-golang-org-x-net" ,go-golang-org-x-net)
                         ("go-golang-org-x-crypto" ,go-golang-org-x-crypto)
                         ("go-github-com-pmezard-go-difflib" ,go-github-com-pmezard-go-difflib)
                         ;("go-github-com-niemeyer-pretty" ,go-github-com-niemeyer-pretty)
                         ("go-github-com-keybase-msgpackzip" ,go-github-com-keybase-msgpackzip)
                         ("go-github-com-keybase-go-jsonw" ,go-github-com-keybase-go-jsonw)
                         ("go-github-com-keybase-go-framed-msgpack-rpc" ,go-github-com-keybase-go-framed-msgpack-rpc)
                         ("go-github-com-keybase-go-crypto" ,go-github-com-keybase-go-crypto)
                         ("go-github-com-keybase-go-codec" ,go-github-com-keybase-go-codec)
                         ("go-github-com-keybase-clockwork" ,go-github-com-keybase-clockwork)
                         ("go-github-com-keybase-backoff" ,go-github-com-keybase-backoff)
                         ("go-github-com-dustin-go-humanize" ,go-github-com-dustin-go-humanize)
                         ("go-github-com-davecgh-go-spew" ,go-github-com-davecgh-go-spew)
                         ("go-golang-org-x-sys" ,go-golang-org-x-sys)
                         ("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)
                         ("go-github-com-keybase-saltpack" ,go-github-com-keybase-saltpack)
                         ("go-github-com-keybase-go-ps" ,go-github-com-keybase-go-ps)
                         ("go-github-com-keybase-go-logging" ,go-github-com-keybase-go-logging)
                         ;("go-github-com-keybase-client-go" ,go-github-com-keybase-client-go)
                         ("go-github-com-kardianos-osext" ,go-github-com-kardianos-osext)
                         ("go-github-com-blang-semver" ,go-github-com-blang-semver)))
    (home-page "https://github.com/keybase/go-updater")
    (synopsis "Updater")
    (description
     "@@strong{Warning}: This isn't ready for non-Keybase libraries to use yet!")
    (license license:expat)))

(define-public go-github-com-keybase-go-winio
  (package
    (name "go-github-com-keybase-go-winio")
    (version "0.4.12-0.20180913221037-b1d96ab97b58")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/keybase/go-winio")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1vw79bmf214inxqpbh36i69jkgb0k1av5ff9j4fqy5ahy5pn6wjn"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/keybase/go-winio"))
    (home-page "https://github.com/keybase/go-winio")
    (synopsis "go-winio")
    (description
     "This repository contains utilities for efficiently performing Win32 IO
operations in Go.  Currently, this is focused on accessing named pipes and other
file handles, and for using named pipes as a net transport.")
    (license license:expat)))

(define-public go-github-com-keybase-go-dbus
  (package
    (name "go-github-com-keybase-go-dbus")
    (version "0.0.0-20200324223359-a94be52c0b03")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/keybase/dbus")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wsmzx41ri8isvippzjfbh2jkzr0zfg0vz94lq3rhigfjxb6rw8s"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f                  ; Tests expect running dbus session.
       #:import-path "github.com/keybase/go.dbus"))
    (home-page "https://github.com/keybase/go.dbus")
    (synopsis "go.dbus")
    (description
     "Package dbus implements bindings to the D-Bus message bus system.")
    (license license:bsd-2)))

(define-public go-github-com-keybase-golang-ico
  (package
    (name "go-github-com-keybase-golang-ico")
    (version "0.0.0-20181117022008-819cbeb217c9")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/keybase/golang-ico")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1grsgl0z0qrn23s2m9cx2xjilm3yj6x3x10mbagbdp5dq6y9rz5i"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f      ; Tests fail to write to directory
       #:import-path "github.com/keybase/golang-ico"))
    (propagated-inputs
     (list go-golang-org-x-image))
    (home-page "https://github.com/keybase/golang-ico")
    (synopsis #f)
    (description #f)
    (license #f)))

(define-public go-github-com-keybase-gomounts
  (package
    (name "go-github-com-keybase-gomounts")
    (version "0.0.0-20180302000443-349507f4d353")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/keybase/gomounts")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0bvdvb5wvnmy5h6hahi209bzv3in84jnv4dyc4n1nvw7grmmh7al"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/keybase/gomounts"))
    (native-inputs
     (list go-github-com-strib-gomounts))
    (home-page "https://github.com/keybase/gomounts")
    (synopsis "gomounts")
    (description
     "Package gomounts implements a cross-platform library for retrieving mounted
filesystem volumes.")
    (license license:expat)))

(define-public go-github-com-keybase-msgpackzip
  (package
    (name "go-github-com-keybase-msgpackzip")
    (version "0.0.0-20211109205514-10e4bc329851")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/keybase/msgpackzip")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0zlb90bnv09ybdhbhdbf5j66dhyav9lcbpgmnm2zxv3lrbd0y4gm"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/keybase/msgpackzip"))
    (propagated-inputs `(("go-github-com-pmezard-go-difflib" ,go-github-com-pmezard-go-difflib)
                         ("go-github-com-davecgh-go-spew" ,go-github-com-davecgh-go-spew)
                         ("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))
    (home-page "https://github.com/keybase/msgpackzip")
    (synopsis "msgpackzip")
    (description
     "You can test compressing a message pack encoded input file by running:")
    (license license:bsd-3)))

(define-public go-github-com-keybase-keybase-test-vectors
  (package
    (name "go-github-com-keybase-keybase-test-vectors")
    (version "1.0.12-0.20200309162119-ea1e58fecd5d")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/keybase/keybase-test-vectors")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0nmawklwj2hmw1xfshn9jyzmjk13pcb3pj7icfp740pc9sp5fa4k"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/keybase/keybase-test-vectors"))
    (home-page "https://github.com/keybase/keybase-test-vectors")
    (synopsis #f)
    (description #f)
    (license #f)))

(define-public go-github-com-keybase-pipeliner
  (package
    (name "go-github-com-keybase-pipeliner")
    (version "0.0.0-20190828022149-31ef4ee63659")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/keybase/pipeliner")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "11j8678cqxjnjldqxhpca5si2r10nrv2z4cd62hfpg59sb8npk7x"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/keybase/pipeliner"))
    (propagated-inputs
     (list go-golang-org-x-net))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/keybase/pipeliner")
    (synopsis "pipeliner")
    (description
     "This package provides a simplified pipline library, for parallel requests with
bounded parallelism.")
    (license license:bsd-3)))

(define-public go-github-com-keybase-release
  (package
    (name "go-github-com-keybase-release")
    (version "0.0.0-20221220220653-50771d921175")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/keybase/release")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "18h8iabbc7vd161pfkadki5f6hhrwb45vsg22ixmd5b9h3vhxymz"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/keybase/release"))
    (propagated-inputs `(("go-gopkg-in-yaml-v3" ,go-gopkg-in-yaml-v3)
                         ("go-gopkg-in-check-v1" ,go-gopkg-in-check-v1)
                         ("go-github-com-smartystreets-goconvey" ,go-github-com-smartystreets-goconvey)
                         ("go-github-com-sergi-go-diff" ,go-github-com-sergi-go-diff)
                         ("go-github-com-pmezard-go-difflib" ,go-github-com-pmezard-go-difflib)
                         ("go-github-com-mattn-go-isatty" ,go-github-com-mattn-go-isatty)
                         ("go-github-com-kr-pretty" ,go-github-com-kr-pretty)
                         ("go-github-com-jmespath-go-jmespath" ,go-github-com-jmespath-go-jmespath)
                         ("go-github-com-go-ini-ini" ,go-github-com-go-ini-ini)
                         ("go-github-com-davecgh-go-spew" ,go-github-com-davecgh-go-spew)
                         ("go-github-com-alecthomas-units" ,go-github-com-alecthomas-units)
                         ("go-github-com-alecthomas-repr" ,go-github-com-alecthomas-repr)
                         ("go-github-com-alecthomas-colour" ,go-github-com-alecthomas-colour)
                         ("go-github-com-alecthomas-assert-v2" ,go-github-com-alecthomas-assert-v2)
                         ("go-gopkg-in-alecthomas-kingpin-v2" ,go-gopkg-in-alecthomas-kingpin-v2)
                         ("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)
                         ("go-github-com-blang-semver" ,go-github-com-blang-semver)
                         ("go-github-com-aws-aws-sdk-go" ,go-github-com-aws-aws-sdk-go)
                         ("go-github-com-alecthomas-template" ,go-github-com-alecthomas-template)))
    (home-page "https://github.com/keybase/release")
    (synopsis "Release")
    (description
     "This is a command line tool for build and release scripts for generating
updates, interacting with Github and S3.")
    (license license:bsd-3)))

(define-public go-github-com-keybase-saltpack
  (package
    (name "go-github-com-keybase-saltpack")
    (version "0.0.0-20211118165207-4039c5df46c0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/keybase/saltpack")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "181763midr61bc4nmhq4ds6pxsjyw8qnm2mwbhsla10lkfdl9vpm"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/keybase/saltpack"))
    (propagated-inputs `(("go-gopkg-in-yaml-v2" ,go-gopkg-in-yaml-v2)
                         ("go-golang-org-x-sys" ,go-golang-org-x-sys)
                         ("go-github-com-pmezard-go-difflib" ,go-github-com-pmezard-go-difflib)
                         ("go-github-com-davecgh-go-spew" ,go-github-com-davecgh-go-spew)
                         ("go-golang-org-x-sync" ,go-golang-org-x-sync)
                         ("go-golang-org-x-crypto" ,go-golang-org-x-crypto)
                         ("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)
                         ("go-github-com-keybase-go-codec" ,go-github-com-keybase-go-codec)))
    (home-page "https://github.com/keybase/saltpack")
    (synopsis "saltpack")
    (description
     "Package saltpack is an implementation of the saltpack message format.  Saltpack
is a light wrapper around Dan Berstein's famous NaCl library.  It adds support
for longer messages, streaming input and output of data, multiple recipients for
encrypted messages, and a reasonable armoring format.  We intend Saltpack as a
replacement for the PGP messaging format, as it can be used in many of the same
circumstances.  However, it is designed to be: (1) simpler; (2) easier to
implement; (3) judicious (perhaps judgmental) in its crypto usage; (4) fully
modern (no CFB mode here); (5) high performance; (6) less bug- prone; (7)
generally unwilling to output unauthenticated data; and (8) easier to compose
with other software in any manner of languages or platforms.")
    (license license:bsd-3)))

(define-public go-github-com-keybase-stellar-org
  (package
    (name "go-github-com-keybase-stellar-org")
    (version "0.0.0-20191010205648-0fc3bfe3dfa7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/keybase/stellar-org")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "032n758lfycldrrb0ciscrm182jnx4nnqxdkl876scsacxxqm906"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/keybase/stellar-org"
       #:phases
       (modify-phases %standard-phases
         (delete 'build)
         (delete 'check))))
    (propagated-inputs `(;("go-gopkg-in-tylerb-graceful-v1" ,go-gopkg-in-tylerb-graceful-v1)
                         ;("go-gopkg-in-gorp-v1" ,go-gopkg-in-gorp-v1)
                         ;("go-gopkg-in-gemnasium-logrus-airbrake-hook-v2" ,go-gopkg-in-gemnasium-logrus-airbrake-hook-v2)
                         ("go-gopkg-in-gavv-httpexpect-v1" ,go-gopkg-in-gavv-httpexpect-v1)
                         ("go-gopkg-in-check-v1" ,go-gopkg-in-check-v1)
                         ;("go-gopkg-in-airbrake-gobrake-v2" ,go-gopkg-in-airbrake-gobrake-v2)
                         ("go-google-golang-org-appengine" ,go-google-golang-org-appengine)
                         ("go-golang-org-x-tools" ,go-golang-org-x-tools)
                         ("go-golang-org-x-net" ,go-golang-org-x-net)
                         ("go-golang-org-x-crypto" ,go-golang-org-x-crypto)
                         ("go-github-com-ziutek-mymysql" ,go-github-com-ziutek-mymysql)
                         ("go-github-com-yudai-pp" ,go-github-com-yudai-pp)
                         ("go-github-com-yudai-golcs" ,go-github-com-yudai-golcs)
                         ("go-github-com-yudai-gojsondiff" ,go-github-com-yudai-gojsondiff)
                         ("go-github-com-yalp-jsonpath" ,go-github-com-yalp-jsonpath)
                         ("go-github-com-xeipuuv-gojsonschema" ,go-github-com-xeipuuv-gojsonschema)
                         ("go-github-com-xeipuuv-gojsonreference" ,go-github-com-xeipuuv-gojsonreference)
                         ("go-github-com-xeipuuv-gojsonpointer" ,go-github-com-xeipuuv-gojsonpointer)
                         ("go-github-com-valyala-fasthttp" ,go-github-com-valyala-fasthttp)
                         ("go-github-com-valyala-bytebufferpool" ,go-github-com-valyala-bytebufferpool)
                         ;("go-github-com-tyler-smith-go-bip39" ,go-github-com-tyler-smith-go-bip39)
                         ("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)
                         ;("go-github-com-stellar-throttled" ,go-github-com-stellar-throttled)
                         ("go-github-com-stellar-go-xdr" ,go-github-com-stellar-go-xdr)
                         ("go-github-com-spf13-viper" ,go-github-com-spf13-viper)
                         ("go-github-com-spf13-pflag" ,go-github-com-spf13-pflag)
                         ("go-github-com-spf13-jwalterweatherman" ,go-github-com-spf13-jwalterweatherman)
                         ("go-github-com-spf13-cobra" ,go-github-com-spf13-cobra)
                         ("go-github-com-spf13-cast" ,go-github-com-spf13-cast)
                         ("go-github-com-smartystreets-goconvey" ,go-github-com-smartystreets-goconvey)
                         ("go-github-com-sirupsen-logrus" ,go-github-com-sirupsen-logrus)
                         ("go-github-com-shurcool-httpfs" ,go-github-com-shurcool-httpfs)
                         ("go-github-com-sergi-go-diff" ,go-github-com-sergi-go-diff)
                         ("go-github-com-segmentio-go-loggly" ,go-github-com-segmentio-go-loggly)
                         ;("go-github-com-sebest-xff" ,go-github-com-sebest-xff)
                         ;("go-github-com-rubenv-sql-migrate" ,go-github-com-rubenv-sql-migrate)
                         ;("go-github-com-rs-xhandler" ,go-github-com-rs-xhandler)
                         ("go-github-com-rs-cors" ,go-github-com-rs-cors)
                         ("go-github-com-rcrowley-go-metrics" ,go-github-com-rcrowley-go-metrics)
                         ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
                         ("go-github-com-opentracing-opentracing-go" ,go-github-com-opentracing-opentracing-go)
                         ("go-github-com-onsi-gomega" ,go-github-com-onsi-gomega)
                         ("go-github-com-onsi-ginkgo" ,go-github-com-onsi-ginkgo)
                         ;("go-github-com-nullstyle-go-xdr" ,go-github-com-nullstyle-go-xdr)
                         ;("go-github-com-moul-http2curl" ,go-github-com-moul-http2curl)
                         ;("go-github-com-mndrix-ps" ,go-github-com-mndrix-ps)
                         ("go-github-com-mitchellh-mapstructure" ,go-github-com-mitchellh-mapstructure)
                         ("go-github-com-mitchellh-go-homedir" ,go-github-com-mitchellh-go-homedir)
                         ("go-github-com-mattn-go-colorable" ,go-github-com-mattn-go-colorable)
                         ("go-github-com-manucorporat-sse" ,go-github-com-manucorporat-sse)
                         ("go-github-com-magiconair-properties" ,go-github-com-magiconair-properties)
                         ("go-github-com-lib-pq" ,go-github-com-lib-pq)
                         ;("go-github-com-lann-builder" ,go-github-com-lann-builder)
                         ("go-github-com-kr-text" ,go-github-com-kr-text)
                         ("go-github-com-kr-pretty" ,go-github-com-kr-pretty)
                         ;("go-github-com-klauspost-crc32" ,go-github-com-klauspost-crc32)
                         ("go-github-com-klauspost-cpuid" ,go-github-com-klauspost-cpuid)
                         ("go-github-com-klauspost-compress" ,go-github-com-klauspost-compress)
                         ("go-github-com-k0kubun-colorstring" ,go-github-com-k0kubun-colorstring)
                         ;("go-github-com-jmoiron-sqlx" ,go-github-com-jmoiron-sqlx)
                         ("go-github-com-jmespath-go-jmespath" ,go-github-com-jmespath-go-jmespath)
                         ("go-github-com-jarcoal-httpmock" ,go-github-com-jarcoal-httpmock)
                         ("go-github-com-inconshreveable-mousetrap" ,go-github-com-inconshreveable-mousetrap)
                         ("go-github-com-imkira-go-interpol" ,go-github-com-imkira-go-interpol)
                         ;("go-github-com-howeyc-gopass" ,go-github-com-howeyc-gopass)
                         ("go-github-com-hashicorp-golang-lru" ,go-github-com-hashicorp-golang-lru)
                         ;("go-github-com-guregu-null" ,go-github-com-guregu-null)
                         ;("go-github-com-graph-gophers-graphql-go" ,go-github-com-graph-gophers-graphql-go)
                         ("go-github-com-google-go-querystring" ,go-github-com-google-go-querystring)
                         ("go-github-com-gomodule-redigo" ,go-github-com-gomodule-redigo)
                         ;("go-github-com-goji-httpauth" ,go-github-com-goji-httpauth)
                         ;("go-github-com-gobuffalo-packr" ,go-github-com-gobuffalo-packr)
                         ("go-github-com-go-ini-ini" ,go-github-com-go-ini-ini)
                         ("go-github-com-go-errors-errors" ,go-github-com-go-errors-errors)
                         ("go-github-com-go-chi-chi" ,go-github-com-go-chi-chi)
                         ("go-github-com-getsentry-raven-go" ,go-github-com-getsentry-raven-go)
                         ("go-github-com-gavv-monotime" ,go-github-com-gavv-monotime)
                         ("go-github-com-fatih-structs" ,go-github-com-fatih-structs)
                         ;("go-github-com-facebookgo-subset" ,go-github-com-facebookgo-subset)
                         ;("go-github-com-facebookgo-structtag" ,go-github-com-facebookgo-structtag)
                         ;("go-github-com-facebookgo-stack" ,go-github-com-facebookgo-stack)
                         ;("go-github-com-facebookgo-inject" ,go-github-com-facebookgo-inject)
                         ;("go-github-com-facebookgo-ensure" ,go-github-com-facebookgo-ensure)
                         ;("go-github-com-elazarl-go-bindata-assetfs" ,go-github-com-elazarl-go-bindata-assetfs)
                         ("go-github-com-aws-aws-sdk-go" ,go-github-com-aws-aws-sdk-go)
                         ("go-github-com-asaskevich-govalidator" ,go-github-com-asaskevich-govalidator)
                         ("go-github-com-ajg-form" ,go-github-com-ajg-form)
                         ;("go-github-com-masterminds-squirrel" ,go-github-com-masterminds-squirrel)
                         ("go-github-com-burntsushi-toml" ,go-github-com-burntsushi-toml)
                         ;("go-bitbucket-org-ww-goautoneg" ,go-bitbucket-org-ww-goautoneg)
                         ))
    (home-page "https://github.com/keybase/stellar-org")
    (synopsis "Stellar Go")
    (description
     "This repo is the home for all of the public go code produced by SDF. In addition
to various tools and services, this repository is the SDK from which you may
develop your own applications that integrate with the stellar network.")
    (license license:asl2.0)))

(define-public go-github-com-keybase-stellarnet
  (package
    (name "go-github-com-keybase-stellarnet")
    (version "0.0.0-20200318171650-0b120a59a787")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/keybase/stellarnet")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "02qn742g72vf7y2z7zzxl1bxd92q40kq00lva7diyzaj3p1kdwdx"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/keybase/stellarnet"))
    (propagated-inputs
     `(("go-github-com-sirupsen-logrus" ,go-github-com-sirupsen-logrus)
       ("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)
       ("go-golang-org-x-crypto" ,go-golang-org-x-crypto)))
    (home-page "https://github.com/keybase/stellarnet")
    (synopsis "stellarnet")
    (description #f)
    (license license:bsd-3)))

(define-public go-github-com-keybase-xurls
  (package
    (name "go-github-com-keybase-xurls")
    (version "1.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/keybase/xurls")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "05q4nqbpgfb0a35sn22rn9mlag2ks4cgwb54dx925hipp6zgj1hx"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/keybase/xurls"))
    (home-page "https://github.com/keybase/xurls")
    (synopsis "xurls")
    (description
     "Package xurls extracts urls from plain text using regular expressions.")
    (license license:bsd-3)))

(define-public go-github-com-klauspost-compress-1.16
  (package
    (name "go-github-com-klauspost-compress")
    (version "1.16.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/klauspost/compress")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0p2q6f8aav1w0whys25z4m8cq7xyrclanyv8hdp4i111jz5xmjgd"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/klauspost/compress"))
    (home-page "https://github.com/klauspost/compress")
    (synopsis "compress")
    (description "This package provides various compression algorithms.")
    (license license:bsd-3)))

(define-public go-github-com-kyokomi-emoji
  (package
    (name "go-github-com-kyokomi-emoji")
    (version "2.2.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/kyokomi/emoji")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0n3wpqqlvqfxy1cw1apswilyfnnc18qgr615k04r8pvzw2vx1icn"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/kyokomi/emoji"))
    (home-page "https://github.com/kyokomi/emoji")
    (synopsis "Emoji")
    (description #f)
    (license license:expat)))

(define-public go-github-com-lafriks-xormstore
  (package
    (name "go-github-com-lafriks-xormstore")
    (version "1.4.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/lafriks/xormstore")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0ys4cg8d4r1b7hbwm8a5aradrj7rc32yjkm93mkl3yydjppd7xhi"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/lafriks/xormstore"))
    (propagated-inputs
     (list go-xorm-io-xorm
           go-github-com-gorilla-sessions
           go-github-com-gorilla-securecookie
           go-github-com-gorilla-context))
    (native-inputs
     (list go-github-com-denisenkom-go-mssqldb
           go-github-com-go-sql-driver-mysql
           go-github-com-lib-pq
           go-github-com-mattn-go-sqlite3))
    (home-page "https://github.com/lafriks/xormstore")
    (synopsis "XORM backend for gorilla sessions")
    (description "Package xormstore is a XORM backend for gorilla sessions")
    (license license:expat)))

(define-public go-github-com-lestrrat-go-backoff-v2
  (package
    (name "go-github-com-lestrrat-go-backoff-v2")
    (version "2.0.8")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/lestrrat-go/backoff")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1s939szsdv0ggp69rig8dkl74s5dvwzm5cw80h0b3dvkqhikim5d"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f      ; Tests only pass under low system load.
       #:import-path "github.com/lestrrat-go/backoff/v2"))
    (propagated-inputs
     (list go-github-com-lestrrat-go-option))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/lestrrat-go/backoff")
    (synopsis "backoff")
    (description
      "Package backoff implements backoff algorithms for retrying operations.")
    (license license:expat)))

(define-public go-github-com-lestrrat-go-blackmagic
  (package
    (name "go-github-com-lestrrat-go-blackmagic")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/lestrrat-go/blackmagic")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "18icd3lx6naz942rxg6hb3dgrihzqa1gab038p9wyh5g2b27x9b9"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/lestrrat-go/blackmagic"))
    (propagated-inputs
     (list go-github-com-pkg-errors))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/lestrrat-go/blackmagic")
    (synopsis "blackmagic")
    (description "Reflect-based black magic.  YMMV, and use with caution")
    (license license:expat)))

(define-public go-github-com-lestrrat-go-httpcc
  (package
    (name "go-github-com-lestrrat-go-httpcc")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/lestrrat-go/httpcc")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1lih3b0xas3j80pp8b0v0pjxrad5iafv44nbxni5nh7l54bp4wd0"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/lestrrat-go/httpcc"))
    (propagated-inputs
     (list go-github-com-pkg-errors))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/lestrrat-go/httpcc")
    (synopsis "httpcc")
    (description
      "Parses HTTP/1.1 Cache-Control header, and returns a struct that is convenient
for the end-user to do what they will with.")
    (license license:expat)))

(define-public go-github-com-lestrrat-go-iter
  (package
    (name "go-github-com-lestrrat-go-iter")
    (version "1.0.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/lestrrat-go/iter")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "07601yzh1dgldi2pp7lvbrlhfd17wqvdqfcxl44jg2n9dmcbmaam"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/lestrrat-go/iter"
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key import-path build-flags #:allow-other-keys)
             (for-each
               (lambda (directory)
                 ((assoc-ref %standard-phases 'build)
                  #:build-flags build-flags
                  #:import-path (string-append "github.com/lestrrat-go/iter/" directory)))
               (list "arrayiter"
                     "mapiter"))))
         (replace 'check
           (lambda* (#:key tests? import-path #:allow-other-keys)
             (for-each
               (lambda (directory)
                 ((assoc-ref %standard-phases 'check)
                  #:tests? tests?
                  #:import-path (string-append "github.com/lestrrat-go/iter/" directory)))
               (list "arrayiter"
                     "mapiter")))))))
    (propagated-inputs
     (list go-github-com-pkg-errors))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/lestrrat-go/iter")
    (synopsis "iter")
    (description "Simple tools for container iteration")
    (license license:expat)))

(define-public go-github-com-lestrrat-go-jwx
  (package
    (name "go-github-com-lestrrat-go-jwx")
    (version "1.2.14")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/lestrrat-go/jwx")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1wyf6x6vlik1dsbh1cbmzm52h8cnz40zzgr8fhdy7h9l5b96wdib"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/lestrrat-go/jwx"))
    (propagated-inputs
     (list go-golang-org-x-crypto
           go-github-com-pkg-errors
           go-github-com-lestrrat-go-option
           go-github-com-lestrrat-go-iter
           go-github-com-lestrrat-go-httpcc
           go-github-com-lestrrat-go-blackmagic
           go-github-com-lestrrat-go-backoff-v2
           go-github-com-goccy-go-json
           go-github-com-decred-dcrd-dcrec-secp256k1-v4))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/lestrrat-go/jwx")
    (synopsis "github.com/lestrrat-go/jwx")
    (description
      "Package jwx contains tools that deal with the various JWx (JOSE) technologies
such as JWT, JWS, JWE, etc in Go.")
    (license license:expat)))

(define-public go-github-com-lestrrat-go-option
  (package
    (name "go-github-com-lestrrat-go-option")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/lestrrat-go/option")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0wic12v84ga7fm14lc51jh5qjp242fm0f67l9xldjz0rm0d6qxwd"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/lestrrat-go/option"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/lestrrat-go/option")
    (synopsis "option")
    (description
      "Base object for what I call the \"Optional Parameters Pattern\".")
    (license license:expat)))

(define-public go-github-com-lunny-dingtalk-webhook
  (package
    (name "go-github-com-lunny-dingtalk-webhook")
    (version "0.0.0-20171025031554-e3534c89ef96")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/lunny/dingtalk_webhook")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0dw6vzv6aq1yfxyllc406q69vlrk39m5jdcj355y9h9ak84plznw"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f      ; All tests require network access.
       #:import-path "github.com/lunny/dingtalk_webhook"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/lunny/dingtalk_webhook")
    (synopsis "Dingtalk webhook Golang SDK")
    (description
     "Dingtalk webhook Golang SDK")
    (license license:expat)))

(define-public go-github-com-lunny-log
  (package
    (name "go-github-com-lunny-log")
    (version "0.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/lunny/log")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1yyqk5lmz3nrd6dnpafmrpkx80f311i5dzw6n2lvn29n9jkkjg97"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/lunny/log"))
    (native-inputs
     (list go-github-com-mattn-go-sqlite3))
    (home-page "https://github.com/lunny/log")
    (synopsis "Extension of Golang log")
    (description "This package is an extension of Golang log.")
    (license license:bsd-3)))

(define-public go-github-com-lunny-nodb
  (package
    (name "go-github-com-lunny-nodb")
    (version "0.0.0-20160621015157-fc1ef06ad4af")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/lunny/nodb")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "08p6sxphi5w9mm43pj9ma5v4n5r2v0xr7nzmp2nya3hpn9fq2vcj"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f      ; Repository archived.
       #:import-path "github.com/lunny/nodb"))
    (propagated-inputs
     (list go-github-com-burntsushi-toml
           go-github-com-lunny-log
           go-github-com-siddontang-go-snappy
           go-github-com-syndtr-goleveldb))
    (home-page "https://github.com/lunny/nodb")
    (synopsis "NoDB")
    (description "Package nodb is a high performance embedded NoSQL.")
    (license license:expat)))

(define-public go-github-com-lyft-protoc-gen-star
  (package
    (name "go-github-com-lyft-protoc-gen-star")
    (version "0.6.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/lyft/protoc-gen-star")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0mbgwnd3nhafx9hvjbyyl38x1ch1b4nmk03pisybqfq1qyadx93q"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/lyft/protoc-gen-star"))
    (propagated-inputs
     (list ;go-google-golang-org-protobuf
        ;go-github-com-stretchr-testify
        ;go-github-com-spf13-afero
        ;go-github-com-golang-protobuf
        ))
    (home-page "https://github.com/lyft/protoc-gen-star")
    (synopsis "protoc-gen-star (PG*)")
    (description "Package pgs provides a library for building protoc plugins")
    (license license:asl2.0)))

(define-public go-github-com-manucorporat-sse
  (package
    (name "go-github-com-manucorporat-sse")
    (version "0.0.0-20160126180136-ee05b128a739")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/manucorporat/sse")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1rvbrq1qvfqpajyd3b0kiczvgwrrffc7fdzw7kvnk53a7sb7gwj4"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/manucorporat/sse"
       ;; https://github.com/manucorporat/sse/issues/3
       #:tests? #f))
    (native-inputs
     `(("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))
    (home-page "https://github.com/manucorporat/sse")
    (synopsis #f)
    (description #f)
    (license license:expat)))

(define-public go-github-com-markbates-going
  (package
    (name "go-github-com-markbates-going")
    (version "1.0.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/markbates/going")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "14b8mj3x7bynpdw99a260j6gpjsqnki5fh3bbsf9cyggsgai11zz"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/markbates/going"
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key import-path build-flags #:allow-other-keys)
             (for-each
               (lambda (directory)
                 ((assoc-ref %standard-phases 'build)
                  #:build-flags build-flags
                  #:import-path (string-append "github.com/markbates/going/" directory)))
               (list "clam"
                     "defaults"
                     "randx"
                     "validate"
                     "wait"))))
         (replace 'check
           (lambda* (#:key tests? import-path #:allow-other-keys)
             (for-each
               (lambda (directory)
                 ((assoc-ref %standard-phases 'check)
                  #:tests? tests?
                  #:import-path (string-append "github.com/markbates/going/" directory)))
               (list "clam"
                     "defaults"
                     "randx"
                     "validate"
                     "wait")))))))
    (propagated-inputs
     (list go-github-com-serenize-snaker
           go-github-com-onsi-gomega
           go-github-com-onsi-ginkgo))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/markbates/going")
    (synopsis "github.com/markbates/going")
    (description
      "This project houses, what I consider to be some help packages for writing Go
applications.  Your mileage may vary, but I find them to be pretty darn helpful.")
    (license license:expat)))

(define-public go-github-com-markbates-goth
  (package
    (name "go-github-com-markbates-goth")
    (version "1.68.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/markbates/goth")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0wvsigrpfys3933hacrd6c5ggx0kfcw9jq9d3m4yx83hsc0jc9jq"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/markbates/goth"))
    (propagated-inputs
     (list go-golang-org-x-oauth2
           go-golang-org-x-net
           go-github-com-pkg-errors
           go-github-com-mrjones-oauth
           go-github-com-markbates-going
           go-github-com-lestrrat-go-jwx
           go-github-com-jarcoal-httpmock
           go-github-com-gorilla-sessions
           go-github-com-gorilla-pat
           go-github-com-gorilla-mux
           go-github-com-golang-jwt-jwt
           go-github-com-davecgh-go-spew
           go-cloud-google-com-go))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/markbates/goth")
    (synopsis "Goth: Multi-Provider Authentication for Go")
    (description
      "Package goth provides a simple, clean, and idiomatic way to write authentication
packages for Go web applications.")
    (license license:expat)))

(define-public go-github-com-mdlayher-ethernet
  (package
    (name "go-github-com-mdlayher-ethernet")
    (version "0.0.0-20220221185849-529eae5b6118")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/mdlayher/ethernet")
                     (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "154jx1i5g5nphzlbx0hr2v0rhhri2p9z80hjnnngbzcqzmy1npbm"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/mdlayher/ethernet"))
    (propagated-inputs
     (list go-golang-org-x-net
           go-github-com-mdlayher-packet))
    (home-page "https://github.com/mdlayher/ethernet")
    (synopsis "ethernet")
    (description
     "Package ethernet implements marshaling and unmarshaling of IEEE 802.3 Ethernet
II frames and IEEE 802.1Q VLAN tags.")
    (license license:expat)))

(define-public go-github-com-mdlayher-packet
  (package
    (name "go-github-com-mdlayher-packet")
    (version "1.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/mdlayher/packet")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0bryji73rn3flqa2d803js7lpbyl3b95mlm9y3h49k8gqxv3gv4j"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/mdlayher/packet"
       #:phases
       (modify-phases %standard-phases
         (delete 'build)
         (delete 'check))))
    (propagated-inputs
     (list go-golang-org-x-sync
           go-golang-org-x-sys
           go-golang-org-x-net
           go-github-com-mdlayher-socket
           go-github-com-josharian-native
           go-github-com-google-go-cmp))
    (home-page "https://github.com/mdlayher/packet")
    (synopsis "packet")
    (description
     "Package packet provides access to Linux packet sockets (AF_PACKET).")
    (license license:expat)))

(define-public go-github-com-mdlayher-raw
  (package
    (name "go-github-com-mdlayher-raw")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/mdlayher/raw")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "04qq2z9gb9rhky62cp0zfd4y16cvjgwyjhk0vpnrxzr90ajyjq46"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/mdlayher/raw"
       #:phases
       (modify-phases %standard-phases
         (delete 'build)
         (delete 'check))))
    (propagated-inputs
     (list go-golang-org-x-sync
           go-github-com-mdlayher-socket
           go-golang-org-x-sys
           go-golang-org-x-net
           go-github-com-mdlayher-packet
           go-github-com-google-go-cmp))
    (home-page "https://github.com/mdlayher/raw")
    (synopsis "raw")
    (description
     "Package raw enables reading and writing data at the device driver level for a
network interface.")
    (license license:expat)))

(define-public go-github-com-mdlayher-sdnotify
  (package
    (name "go-github-com-mdlayher-sdnotify")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mdlayher/sdnotify")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0nxpbjba4n4vfkwfy9q2x5djv8agfnvd6pp7l7a6d6ssl2vhkcrv"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/mdlayher/sdnotify"))
    (propagated-inputs `(("go-github-com-google-go-cmp" ,go-github-com-google-go-cmp)))
    (home-page "https://github.com/mdlayher/sdnotify")
    (synopsis "sdnotify")
    (description
     "Package sdnotify implements systemd readiness notifications as described in
@@url{https://www.freedesktop.org/software/systemd/man/sd_notify.html,https://www.freedesktop.org/software/systemd/man/sd_notify.html}.")
    (license license:expat)))

(define-public go-github-com-mgechev-dots
  (package
    (name "go-github-com-mgechev-dots")
    (version "0.0.0-20210922191527-e955255bf517")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/mgechev/dots")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1abvh8fs3rhw97yhvryczjfa5c8qasmg00dnza0kgyan7n2087pd"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/mgechev/dots"))
    (home-page "https://github.com/mgechev/dots")
    (synopsis "Dots")
    (description
      "Implements the wildcard file matching in Go used by golint, go test etc.")
    (license license:expat)))

(define-public go-github-com-mgechev-revive
  (package
    (name "go-github-com-mgechev-revive")
    (version "1.1.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/mgechev/revive")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1n4ivw04c9yccaxjalm7rb7gmrks2dkh6rrhfl7ia50pq34632cx"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/mgechev/revive"))
    (propagated-inputs
     (list go-golang-org-x-tools
           go-github-com-pkg-errors
           go-github-com-olekukonko-tablewriter
           go-github-com-mitchellh-go-homedir
           go-github-com-mgechev-dots
           go-github-com-fatih-structtag
           go-github-com-fatih-color
           go-github-com-chavacava-garif
           go-github-com-burntsushi-toml))
    (home-page "https://github.com/mgechev/revive")
    (synopsis "revive")
    (description
      "Fast, configurable, extensible, flexible, and beautiful linter for Go.  Drop-in
replacement of golint. .")
    (license license:expat)))

(define-public go-github-com-microsoft-go-winio
  (package
    (name "go-github-com-microsoft-go-winio")
    (version "0.5.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/microsoft/go-winio")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "180fvli0g005bs7ahsm00w4b9nmz9qqkrqn7dy8wqfi7fph1ssff"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f      ; Not on Windows.
       #:import-path "github.com/Microsoft/go-winio"))
    (propagated-inputs
     (list go-golang-org-x-sys
           go-github-com-sirupsen-logrus
           go-github-com-pkg-errors))
    (home-page "https://github.com/Microsoft/go-winio")
    (synopsis "go-winio")
    (description
     "This repository contains utilities for efficiently performing Win32 IO
operations in Go.  Currently, this is focused on accessing named pipes and other
file handles, and for using named pipes as a net transport.")
    (license license:expat)))

(define-public go-github-com-minio-md5-simd
  (package
    (name "go-github-com-minio-md5-simd")
    (version "1.1.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/minio/md5-simd")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0qj8ipifbdg3ppilyqj8zy68f72rmqy8flli1vch3fibrbw8vpd0"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/minio/md5-simd"))
    (propagated-inputs
     (list go-github-com-klauspost-cpuid-v2))
    (home-page "https://github.com/minio/md5-simd")
    (synopsis "md5-simd")
    (description
      "This is a SIMD accelerated MD5 package, allowing up to either 8 (AVX2) or 16
(AVX512) independent MD5 sums to be calculated on a single CPU core.")
    (license license:asl2.0)))

(define-public go-github-com-minio-sha256-simd-1.0.0
  (package
    (name "go-github-com-minio-sha256-simd")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/minio/sha256-simd")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0c151l574qclh93pwn9ckq71b4p19iky7yd3ysm0fad8fkhqqq5g"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/minio/sha256-simd"))
    (propagated-inputs (list go-github-com-klauspost-cpuid-v2))
    (home-page "https://github.com/minio/sha256-simd")
    (synopsis "sha256-simd")
    (description
     "Accelerate SHA256 computations in pure Go using AVX512, SHA Extensions for x86
and ARM64 for ARM. On AVX512 it provides an up to 8x improvement (over 3 GB/s
per core).  SHA Extensions give a performance boost of close to 4x over native.")
    (license license:asl2.0)))

(define-public go-github-com-minio-minio-go-v7
  (package
    (name "go-github-com-minio-minio-go-v7")
    (version "7.0.53")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/minio/minio-go")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1zim60k2h22j7igy90swwqafp456566m175kyjjk278gnihfcxmq"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/minio/minio-go/v7"
       ;;#:phases
       #;(modify-phases %standard-phases
         (add-after 'unpack 'disable-network-tests
           (lambda _
             (substitute* "src/github.com/minio/minio-go/v7/core_test.go"
               (("TestGet") "DisabledTestGet")
               (("TestCore") "DisabledTestCore")))))))
    (propagated-inputs
     (list go-gopkg-in-ini-v1
           go-golang-org-x-net-0.10
           go-golang-org-x-crypto
           go-github-com-sirupsen-logrus
           go-github-com-rs-xid
           go-github-com-minio-sha256-simd-1.0.0
           go-github-com-minio-md5-simd
           go-github-com-klauspost-compress-1.16
           go-github-com-json-iterator-go
           go-github-com-google-uuid-1.3
           go-github-com-dustin-go-humanize))
    (home-page "https://github.com/minio/minio-go")
    (synopsis "MinIO Go Client SDK for Amazon S3 Compatible Cloud Storage")
    (description
     "The MinIO Go Client SDK provides simple APIs to access any Amazon S3 compatible
object storage.")
    (license license:asl2.0)))

(define-public go-github-com-mitchellh-go-ps
  (package
    (name "go-github-com-mitchellh-go-ps")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mitchellh/go-ps")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ipcbz66x7q8xczi7cyfq06y7n7v0syvkp730vn9jrn7s8f5ag0z"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/mitchellh/go-ps"))
    (home-page "https://github.com/mitchellh/go-ps")
    (synopsis "Process List Library for Go")
    (description
     "ps provides an API for finding and listing processes in a platform-agnostic way.")
    (license license:expat)))

(define-public go-github-com-mrjones-oauth
  (package
    (name "go-github-com-mrjones-oauth")
    (version "0.0.0-20190623134757-126b35219450")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/mrjones/oauth")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "120ajfcqcbdiglxl31yq1xgp6ahpb7jfnyk0587b9mljkaggs8gh"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/mrjones/oauth"))
    (home-page "https://github.com/mrjones/oauth")
    (synopsis "OAuth 1.0 Library for")
    (description
      "OAuth 1.0 consumer implementation.  See
@url{http://www.oauth.net,http://www.oauth.net} and
@url{https://rfc-editor.org/rfc/rfc5849.html,RFC 5849}")
    (license license:expat)))

(define-public go-github-com-mschoch-smat
  (package
    (name "go-github-com-mschoch-smat")
    (version "0.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/mschoch/smat")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1qcb2jjg37krxmc915kqynghd6n26w2wxwgcafvxcwn8g0jx96qd"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/mschoch/smat"))
    (home-page "https://github.com/mschoch/smat")
    (synopsis "smat â\x80\x93 State Machine Assisted Testing")
    (description
     "The concept is simple, describe valid uses of your library as states and
actions.  States describe which actions are possible, and with what probability
they should occur.  Actions mutate the context and transition to another state.")
    (license license:asl2.0)))

(define-public go-github-com-neelance-astrewrite
  (package
    (name "go-github-com-neelance-astrewrite")
    (version "0.0.0-20160511093645-99348263ae86")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/neelance/astrewrite")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "07527807p8q6h05iq4qy0xrlcmwyzj76gpk0yqf71yaj447mz24v"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/neelance/astrewrite"))
    (home-page "https://github.com/neelance/astrewrite")
    (synopsis #f)
    (description #f)
    (license license:bsd-2)))

(define-public go-github-com-neelance-sourcemap
  (package
    (name "go-github-com-neelance-sourcemap")
    (version "0.0.0-20200213170602-2833bce08e4c")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/neelance/sourcemap")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "05ymjg1z9phf0wp4w058kvf13bmn4skv67chb3r04z69in8y8jih"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/neelance/sourcemap"))
    (home-page "https://github.com/neelance/sourcemap")
    (synopsis #f)
    (description #f)
    (license license:bsd-2)))

(define-public go-github-com-nf-cr2
  (package
    (name "go-github-com-nf-cr2")
    (version "0.0.0-20180623103828-4699471a17ed")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/nf/cr2")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "105dqvynd2dm0xv0sjyf2qkhx70pf84i8mk5fmj4bfxjc06z4gmv"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/nf/cr2"
       ;; src/github.com/nf/cr2/reader_test.go:52:4: Error call has possible formatting directive %v
       ;; src/github.com/nf/cr2/reader_test.go:55:4: Error call has possible formatting directive %v
       #:tests? #f))
    (home-page "https://github.com/nf/cr2")
    (synopsis "Basic Camera Raw 2 reader")
    (description "This package implements rudimentary support for reading
@acronym{CR, Canon Camera Raw 2} files.")
    (license license:bsd-3)))

(define-public go-github-com-nfnt-resize
  (package
    (name "go-github-com-nfnt-resize")
    (version "0.0.0-20180221191011-83c6a9932646")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/nfnt/resize")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "005cpiwq28krbjf0zjwpfh63rp4s4is58700idn24fs3g7wdbwya"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/nfnt/resize"))
    (home-page "https://github.com/nfnt/resize")
    (synopsis
      "This package is no longer being updated! Please look for alternatives if that bothers you.")
    (description "Package resize implements various image resizing methods.")
    (license license:isc)))

(define-public go-github-com-nwaples-rardecode
  (package
    (name "go-github-com-nwaples-rardecode")
    (version "1.1.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/nwaples/rardecode")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1v89hkai3qr5kizqhdsn4kg5vskpxbxz7fpbldfvn8g7756q0v6z"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/nwaples/rardecode"))
    (home-page "https://github.com/nwaples/rardecode")
    (synopsis "rardecode")
    (description
      "This package provides a go package for reading RAR archives.")
    (license license:bsd-2)))

(define-public go-github-com-oliamb-cutter
  (package
    (name "go-github-com-oliamb-cutter")
    (version "0.2.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/oliamb/cutter")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1cp7449pqg8wkcpcaqqq3rrmxgd88kk4pwh32hx1k4xdlvwm8ffr"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/oliamb/cutter"))
    (home-page "https://github.com/oliamb/cutter")
    (synopsis "Cutter")
    (description "Package cutter provides a function to crop image.")
    (license license:expat)))

(define-public go-github-com-olivere-elastic-v7
  (package
    (name "go-github-com-olivere-elastic-v7")
    (version "7.0.30")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/olivere/elastic")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "105bcf5477sv6a31kfk8zfyfnyvi5gxhxvax3rc1wpgml5grgl4p"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f      ; Tests require elasticsearch node.
       #:import-path "github.com/olivere/elastic/v7"))
    (propagated-inputs
     (list ;go-go-opencensus-io
        ;go-github-com-smarty-gunit
        ;go-github-com-smartystreets-go-aws-auth
        go-github-com-smarty-assertions
        go-github-com-pkg-errors
        ;go-github-com-opentracing-opentracing-go
        go-github-com-mailru-easyjson
        ;go-github-com-golang-groupcache
        ;go-github-com-aws-aws-sdk-go
        ))
    (native-inputs
     (list
        go-github-com-google-go-cmp
        go-github-com-fortytw2-leaktest
       ))
    (home-page "https://github.com/olivere/elastic")
    (synopsis "Elastic")
    (description
      "Package elastic provides an interface to the Elasticsearch server
(@url{https://www.elastic.co/products/elasticsearch,https://www.elastic.co/products/elasticsearch}).")
    (license license:expat)))

(define-public go-github-com-peterbourgon-ff-v3
  (package
    (name "go-github-com-peterbourgon-ff-v3")
    (version "3.1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/peterbourgon/ff")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1i3kczf0afv3ba0a6g02wg5l7rk4agid3d85lgamyy13v2lzif7k"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/peterbourgon/ff/v3"))
    (propagated-inputs `(("go-gopkg-in-yaml-v2" ,go-gopkg-in-yaml-v2)
                         ("go-github-com-pelletier-go-toml" ,go-github-com-pelletier-go-toml)))
    (home-page "https://github.com/peterbourgon/ff")
    (synopsis "ff")
    (description
     "Package ff is a flags-first helper package for configuring programs.")
    (license license:asl2.0)))

(define-public go-github-com-peterh-liner
  (package
    (name "go-github-com-peterh-liner")
    (version "1.2.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/peterh/liner")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0ma4wk01f63s1vshdly3m7pn56xlapi1hfmhqvs8yqx5pqi2w2yf"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/peterh/liner"))
    (propagated-inputs
     (list go-github-com-mattn-go-runewidth))
    (home-page "https://github.com/peterh/liner")
    (synopsis "Line editor in Golang")
    (description
     "Package liner implements a simple command line editor, inspired by
@url{https://github.com/antirez/linenoise/,linenoise}.")
    (license license:expat)))

(define-public go-github-com-pquerna-otp
  (package
    (name "go-github-com-pquerna-otp")
    (version "1.3.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/pquerna/otp")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1cvac28nchyi9l6iagdsq8mqm59498n4sfjvw88pf0rxhzmbxfcf"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/pquerna/otp"))
    (propagated-inputs
     (list go-github-com-boombuler-barcode))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/pquerna/otp")
    (synopsis "otp: One Time Password utilities Go / Golang")
    (description
      "Package otp implements both HOTP and TOTP based one time passcodes in a Google
Authenticator compatible manner.")
    (license license:asl2.0)))

(define-public go-github-com-qrtz-nativemessaging
  (package
    (name "go-github-com-qrtz-nativemessaging")
    (version "0.0.0-20161221035708-f4769a80e040")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/qrtz/nativemessaging")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1gp4wdg5rvcxhnzv55pwd8qvxdk9cq5i0cpcwlx35vqn6v28nyvc"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/qrtz/nativemessaging"))
    (home-page "https://github.com/qrtz/nativemessaging")
    (synopsis "nativemessaging")
    (description #f)
    (license license:expat)))

(define-public go-github-com-quasoft-websspi
  (package
    (name "go-github-com-quasoft-websspi")
    (version "1.1.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/quasoft/websspi")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1xgq9cghrf25zqjrx7hmc2qcjgh38lpkxb8b5i71c7k1nv1hh65z"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/quasoft/websspi"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-some-tests
           (lambda _
             ;; Some of the tests are Windows specific and can't be easily separated.
             (delete-file "src/github.com/quasoft/websspi/utf16_test.go"))))))
    (propagated-inputs
     (list go-golang-org-x-sys
           go-github-com-gorilla-sessions
           go-github-com-gorilla-securecookie))
    (home-page "https://github.com/quasoft/websspi")
    (synopsis "websspi")
    (description
      "@code{websspi} is an HTTP middleware for Golang that uses Kerberos/NTLM for
single sign-on (SSO) authentication of browser based clients in a Windows
environment.")
    (license license:expat)))

(define-public go-github-com-reiver-go-oi
  (package
    (name "go-github-com-reiver-go-oi")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/reiver/go-oi")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0icqnz0lvpj9ghy5s69x0v2mfb67mjnfyggs1ff14shqjai2p6i2"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/reiver/go-oi"))
    (home-page "https://github.com/reiver/go-oi")
    (synopsis "go-oi")
    (description
     "Package oi provides useful tools to be used with Go's standard \"io\" package.")
    (license license:expat)))

(define-public go-github-com-reiver-go-telnet
  (package
    (name "go-github-com-reiver-go-telnet")
    (version "0.0.0-20180421082511-9ff0b2ab096e")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/reiver/go-telnet")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "05kgn00l5l1g48ry16l90lghg3x937apb9j7rg12hh2ghkxczha9"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/reiver/go-telnet"))
    (propagated-inputs
     (list go-github-com-reiver-go-oi))
    (home-page "https://github.com/reiver/go-telnet")
    (synopsis "go-telnet")
    (description
     "Package telnet provides TELNET and TELNETS client and server implementations in
a style similar to the \"net/http\" library that is part of the Go standard
library, including support for \"middleware\"; TELNETS is secure TELNET, with the
TELNET protocol over a secured TLS (or SSL) connection.")
    (license license:expat)))

(define-public go-github-com-roaringbitmap-roaring
  (package
    (name "go-github-com-roaringbitmap-roaring")
    (version "0.9.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/RoaringBitmap/roaring")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0fpm6z725hvk09w5l7sxy0rzb0gwjz9404s2s7i8r6i6xp1fp470"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/RoaringBitmap/roaring"))
    (propagated-inputs
     (list go-github-com-mschoch-smat
           go-github-com-bits-and-blooms-bitset))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/RoaringBitmap/roaring")
    (synopsis "roaring")
    (description
     "Package roaring is an implementation of Roaring Bitmaps in Go.  They provide
fast compressed bitmap data structures (also called bitset).  They are ideally
suited to represent sets of integers over relatively small ranges.  See
@url{http://roaringbitmap.org,http://roaringbitmap.org} for details.")
    (license license:asl2.0)))

(define-public go-github-com-rwcarlsen-goexif
  (package
    (name "go-github-com-rwcarlsen-goexif")
    (version "0.0.0-20190401172101-9e8deecbddbd")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/rwcarlsen/goexif")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1drqhzplg72lvrf3qmb9awbggnjqp23hwn2pgvksi3spv17kc9h2"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/rwcarlsen/goexif"
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key import-path build-flags #:allow-other-keys)
             (for-each
               (lambda (directory)
                 ((assoc-ref %standard-phases 'build)
                  #:build-flags build-flags
                  #:import-path directory))
               (list "github.com/rwcarlsen/goexif/exif"
                     "github.com/rwcarlsen/goexif/tiff"))))
         (replace 'check
           (lambda* (#:key tests? import-path #:allow-other-keys)
             (for-each
               (lambda (directory)
                 ((assoc-ref %standard-phases 'check)
                  #:tests? tests?
                  #:import-path directory))
               (list "github.com/rwcarlsen/goexif/exif"
                     "github.com/rwcarlsen/goexif/tiff")))))))
    (home-page "https://github.com/rwcarlsen/goexif")
    (synopsis "goexif")
    (description #f)
    (license license:bsd-2)))

(define-public go-github-com-saintfish-chardet
  (package
    (name "go-github-com-saintfish-chardet")
    (version "0.0.0-20120816061221-3af4cd4741ca")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/saintfish/chardet")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0czh50md64k9lbllayq0asir3174saxb88yzxrh640yhfxd98pcb"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/saintfish/chardet"))
    (home-page "https://github.com/saintfish/chardet")
    (synopsis "chardet")
    (description #f)
    (license license:expat)))

(define-public go-github-com-scylladb-termtables
  (package
    (name "go-github-com-scylladb-termtables")
    (version "0.0.0-20191203121021-c4c0b6d42ff4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/scylladb/termtables")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "12qjh5gjw2hvrjdh99d4ng8sxicjgdf5bbadrlp4sbd86rwskr54"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/scylladb/termtables"))
    (home-page "https://github.com/scylladb/termtables")
    (synopsis "Termtables")
    (description #f)
    (license license:asl2.0)))

(define-public go-github-com-segmentio-go-loggly
  (package
    (name "go-github-com-segmentio-go-loggly")
    (version "0.5.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/segmentio/go-loggly")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "04wgxd5dp7azrlhw9qrv4v5j541khw0kgdp4va0rhfylcyggz6k3"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/segmentio/go-loggly"
       #:tests? #f))
    (home-page "https://github.com/segmentio/go-loggly")
    (synopsis #f)
    (description #f)
    (license #f)))

(define-public go-github-com-serenize-snaker
  (package
    (name "go-github-com-serenize-snaker")
    (version "0.0.0-20201027110005-a7ad2135616e")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/serenize/snaker")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1c1d335q1i3mz55bhs2k84rcrz4xdaps2y63vwkyv9fsjpb2wnzb"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/serenize/snaker"))
    (native-inputs
     (list go-github-com-onsi-ginkgo))
    (home-page "https://github.com/serenize/snaker")
    (synopsis "snaker")
    (description
      "Package snaker provides methods to convert CamelCase names to snake_case and
back.  It considers the list of allowed initialsms used by
github.com/golang/lint/golint (e.g.  ID or HTTP)")
    (license license:expat)))

(define-public go-github-com-shurcool-go
  (package
    (name "go-github-com-shurcool-go")
    (version "0.0.0-20200502201357-93f07166e636")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/shurcooL/go")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0wgwlhsgx1c2v650xvf099hgrd4av18gfb0kha09klmsh0p0hc5r"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/shurcooL/go"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'build))))
    (home-page "https://github.com/shurcooL/go")
    (synopsis "go")
    (description "Common Go code.")
    (license license:expat)))

(define-public go-github-com-shurcool-go-goon
  (package
    (name "go-github-com-shurcool-go-goon")
    (version "0.0.0-20210110234559-7585751d9a17")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/shurcooL/go-goon")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1c0gkv255kjbbcx3ab26driihnq685vp08axrp5ls8vq7g67rrwl"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/shurcooL/go-goon"))
    (home-page "https://github.com/shurcooL/go-goon")
    (synopsis "Pretty printer with Go-like notation")
    (description
     "Package goon is a deep pretty printer with Go-like notation.  It implements the
goon specification.")
    (license license:expat)))

(define-public go-github-com-siddontang-go
  (package
    (name "go-github-com-siddontang-go")
    (version "0.0.0-20180604090527-bdc77568d726")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/siddontang/go")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0qx28xwqby3pl2r62y700x7j7aplmfm4hrq0y49p4ar8927mpxl6"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/siddontang/go"
       #:modules ((guix build go-build-system)
                  (guix build utils)
                  (srfi srfi-1))
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key import-path build-flags #:allow-other-keys)
             (for-each
               (lambda (directory)
                 ((assoc-ref %standard-phases 'build)
                  #:build-flags build-flags
                  #:import-path (string-drop directory 4)))
               (find-files "src/github.com/siddontang/go"
                 (lambda (file stat)
                   (and
                     (eq? (stat:type stat) 'directory)
                     (let ((files (find-files file "\\.go$")))
                       (and
                         (not (null? files))
                         (not (null?
                                (filter-map
                                  (lambda (test-entry)
                                    (not (string-contains test-entry file-name-separator-string)))
                                  (map (lambda (entry)
                                         (string-drop entry (1+ (string-length file))))
                                       files))))))))
                 #:directories? #t))))
         (replace 'check
           (lambda* (#:key tests? import-path #:allow-other-keys)
             (for-each
               (lambda (directory)
                 ((assoc-ref %standard-phases 'check)
                  #:tests? tests?
                  #:import-path (string-drop directory 4)))
               (find-files "src/github.com/siddontang/go"
                 (lambda (file stat)
                   (and
                     (eq? (stat:type stat) 'directory)
                     (let ((files (find-files file "\\.go$")))
                       (and
                         (not (null? files))
                         (not (string-contains file "go/bson"))
                         (not (string-contains file "go/filelock"))
                         (not (string-contains file "go/list2"))
                         (not (string-contains file "go/rpc"))          ; wants network access
                         (not (null?
                                (filter-map
                                  (lambda (test-entry)
                                    (not (string-contains test-entry file-name-separator-string)))
                                  (map (lambda (entry)
                                         (string-drop entry (1+ (string-length file))))
                                       files))))))))
                 #:directories? #t)))))))
    (native-inputs
     (list go-github-com-gorilla-websocket))
    (home-page "https://github.com/siddontang/go")
    (synopsis "Collection of Golang libraries")
    (description "This package contains a curated collection of Golang libraries.")
    (license license:expat)))

(define-public go-github-com-siddontang-goredis
  (package
    (name "go-github-com-siddontang-goredis")
    (version "0.0.0-20180423163523-0b4019cbd7b7")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/siddontang/goredis")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1cmkmljgyqvfc5ba5jj6xfiwdc82vksagvh2v7z06265i2snvhw5"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/siddontang/goredis"))
    (native-inputs
     (list go-github-com-alicebob-miniredis))
    (home-page "https://github.com/siddontang/goredis")
    (synopsis "Redis client in Go")
    (description "Package goredis is a client for redis and ledisdb.")
    (license license:expat)))

;; Can we not use this library?
(define-public go-github-com-siddontang-go-snappy
  (package
    (name "go-github-com-siddontang-go-snappy")
    (version "0.0.0-20140704025258-d8f7bb82a96d")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/siddontang/go-snappy")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "11h25k4h8mdnhm8jyllsvv597bgbq3zf5ym2ccppla1jflp5r3jg"))))
    (build-system go-build-system)
    (arguments
     '(
       #:unpack-path "github.com/siddontang/go-snappy"
       #:import-path "github.com/siddontang/go-snappy/snappy"

       ;#:import-path "github.com/siddontang/go-snappy"
       ;#:phases
       ;(modify-phases %standard-phases
       ;  (replace 'build
       ;    (lambda* (#:key import-path build-flags #:allow-other-keys)
       ;      (lambda (directory)
       ;        ((assoc-ref %standard-phases 'build)
       ;         #:build-flags build-flags
       ;         #:import-path "github.com/siddontang/go-snappy/snappy"))))
       ;  (replace 'check
       ;    (lambda* (#:key tests? import-path #:allow-other-keys)
       ;      (lambda (directory)
       ;        ((assoc-ref %standard-phases 'check)
       ;         #:tests? tests?
       ;         #:import-path "github.com/siddontang/go-snappy/snappy")))))
       ))
    (home-page "https://github.com/siddontang/go-snappy")
    (synopsis "Snappy library for Golang")
    (description "This is a Snappy library for the Go programming language.")
    (license license:bsd-3)))

(define-public go-github-com-siddontang-ledisdb
  (package
    (name "go-github-com-siddontang-ledisdb")
    (version "0.0.0-20200510135210-d35789ec47e6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ledisdb/ledisdb")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1vjsjscnbg2l9id5psn3ja0hs0jf3bal01b87cx34swjxmnawh1p"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (delete-file-recursively "vendor")))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/ledisdb/ledisdb"
       #:modules ((guix build go-build-system)
                  (guix build utils)
                  (srfi srfi-1))
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key import-path build-flags #:allow-other-keys)
             (for-each
               (lambda (directory)
                 ((assoc-ref %standard-phases 'build)
                  #:build-flags build-flags
                  #:import-path (string-drop directory 4)))
               (find-files "src/github.com/ledisdb/ledisdb"
                 (lambda (file stat)
                   (and
                     (eq? (stat:type stat) 'directory)
                     (let ((files (find-files file "\\.go$")))
                       (and
                         (not (null? files))
                         (not (null?
                                (filter-map
                                  (lambda (test-entry)
                                    (not (string-contains test-entry file-name-separator-string)))
                                  (map (lambda (entry)
                                         (string-drop entry (1+ (string-length file))))
                                       files))))))))
                 #:directories? #t))))
         (replace 'check
           (lambda* (#:key tests? import-path #:allow-other-keys)
             (for-each
               (lambda (directory)
                 ((assoc-ref %standard-phases 'check)
                  #:tests? tests?
                  #:import-path (string-drop directory 4)))
               (find-files "src/github.com/ledisdb/ledisdb"
                 (lambda (file stat)
                   (and
                     (eq? (stat:type stat) 'directory)
                     (let ((files (find-files file "\\.go$")))
                       (and
                         (not (null? files))
                         (not (string-contains file "ledisdb/ledisdb/ledis"))
                         (not (string-contains file "ledisdb/server"))
                         (not (string-contains file "ledisdb/store"))
                         (not (null?
                                (filter-map
                                  (lambda (test-entry)
                                    (not (string-contains test-entry file-name-separator-string)))
                                  (map (lambda (entry)
                                         (string-drop entry (1+ (string-length file))))
                                       files))))))))
                 #:directories? #t)))))))
    (propagated-inputs
     (list go-github-com-edsrzf-mmap-go
           go-github-com-glendc-gopher-json
           go-github-com-pelletier-go-toml
           go-github-com-peterh-liner
           go-github-com-siddontang-go
           go-github-com-siddontang-goredis
           go-github-com-siddontang-rdb
           go-github-com-syndtr-goleveldb
           go-github-com-ugorji-go
           go-github-com-yuin-gopher-lua
           go-golang-org-x-net))
    (home-page "https://github.com/ledisdb/ledisdb")
    (synopsis "LedisDB")
    (description
     "Ledisdb is a high-performance NoSQL database, similar to Redis, written in
Go.  It supports many data structures including kv, list, hash, zset, set.")
    (license license:expat)))

(define-public go-github-com-siddontang-rdb
  (package
    (name "go-github-com-siddontang-rdb")
    (version "0.0.0-20150307021120-fc89ed2e418d")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/siddontang/rdb")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "107wb2kcg67iggfx1bjmm5nhy8cg96zi1iw7nkv9dydivnvalbbd"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/siddontang/rdb"))
    (propagated-inputs
     (list go-github-com-cupcake-rdb))
    (home-page "https://github.com/siddontang/rdb")
    (synopsis "Handle Redis RDB format in Golang")
    (description "This package handles Redis RDB format in Golang.")
    (license license:expat)))

(define-public go-github-com-smartystreets-go-aws-auth
  (package
    (name "go-github-com-smartystreets-go-aws-auth")
    (version "0.0.0-20180515143844-0c1422d1fdb9")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/smartystreets-archives/go-aws-auth")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0krfdpgn3gfii1z9fi8ydfw0wwfqyvp6w3rji7w92m528zkjl93d"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/smartystreets/go-aws-auth"))
    (home-page "https://github.com/smartystreets/go-aws-auth")
    (synopsis "go-aws-auth")
    (description
      "Package awsauth implements AWS request signing using Signed Signature Version 2,
Signed Signature Version 3, and Signed Signature Version 4.  Supports S3 and
STS.")
    (license license:expat)))

(define-public go-github-com-smartystreets-gunit
  (package
    (name "go-github-com-smartystreets-gunit")
    (version "1.4.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/smartystreets/gunit")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "12f9b15qf7qvnbbpc3lzz9vsyddwmw8nq7a2wamwh9k7g6gh3x8j"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/smartystreets/gunit"))
    (home-page "https://github.com/smartystreets/gunit")
    (synopsis "gunit")
    (description
      "Package gunit provides \"testing\" package hooks and convenience functions for
writing tests in an xUnit style.  See the README file and the examples folder
for examples.")
    (license license:expat)))

(define-public go-github-com-sourcegraph-go-diff
  (package
    (name "go-github-com-sourcegraph-go-diff")
    (version "0.6.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/sourcegraph/go-diff")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0ic58wi4cac61kh5sasn9iylcbzbqawlzva964rk0y0nifsyjcmc"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/sourcegraph/go-diff"))
    (propagated-inputs
     (list go-github-com-shurcool-go-goon
           go-github-com-google-go-cmp))
    (home-page "https://github.com/sourcegraph/go-diff")
    (synopsis "Unified diff parser and printer for Go")
    (description
     "This package provides a unified diff parser and printer for Go.")
    (license license:expat)))

(define-public go-github-com-src-d-gcfg
  (package
    (name "go-github-com-src-d-gcfg")
    (version "1.4.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/src-d/gcfg")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "044j95skmyrwjw5fwjk6ka32rjgsg0ar0mfp9np19sh1acwv4x4r"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/src-d/gcfg"))
    (propagated-inputs
     `(("go-gopkg-in-warnings" ,go-gopkg-in-warnings-v0)))
    (native-inputs
     `(("go-github-com-pkg-errors" ,go-github-com-pkg-errors)))
    (home-page "https://github.com/src-d/gcfg")
    (synopsis #f)
    (description #f)
    (license license:bsd-3)))

(define-public go-github-com-stellar-go
  (package
    (name "go-github-com-stellar-go")
    (version "0.0.0-20191010205648-0fc3bfe3dfa7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/stellar/go")
                     (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "032n758lfycldrrb0ciscrm182jnx4nnqxdkl876scsacxxqm906"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/stellar/go"))
    (propagated-inputs
     `(;("go-gopkg-in-tylerb-graceful-v1" ,go-gopkg-in-tylerb-graceful-v1)
       ;("go-gopkg-in-gorp-v1" ,go-gopkg-in-gorp-v1)
       ;("go-gopkg-in-gemnasium-logrus-airbrake-hook-v2" ,go-gopkg-in-gemnasium-logrus-airbrake-hook-v2)
       ;("go-gopkg-in-gavv-httpexpect-v1" ,go-gopkg-in-gavv-httpexpect-v1)  ; FTBFS
       ("go-gopkg-in-check-v1" ,go-gopkg-in-check-v1)
       ;("go-gopkg-in-airbrake-gobrake-v2" ,go-gopkg-in-airbrake-gobrake-v2)
       ("go-google-golang-org-appengine" ,go-google-golang-org-appengine)
       ("go-golang-org-x-tools" ,go-golang-org-x-tools)
       ("go-golang-org-x-net" ,go-golang-org-x-net)
       ("go-golang-org-x-crypto" ,go-golang-org-x-crypto)
       ("go-github-com-ziutek-mymysql" ,go-github-com-ziutek-mymysql)  ; FTBFS
       ("go-github-com-yudai-pp" ,go-github-com-yudai-pp)
       ("go-github-com-yudai-golcs" ,go-github-com-yudai-golcs)
       ("go-github-com-yudai-gojsondiff" ,go-github-com-yudai-gojsondiff)
       ("go-github-com-yalp-jsonpath" ,go-github-com-yalp-jsonpath)
       ("go-github-com-xeipuuv-gojsonschema" ,go-github-com-xeipuuv-gojsonschema)
       ("go-github-com-xeipuuv-gojsonreference" ,go-github-com-xeipuuv-gojsonreference)
       ("go-github-com-xeipuuv-gojsonpointer" ,go-github-com-xeipuuv-gojsonpointer)
       ("go-github-com-valyala-fasthttp" ,go-github-com-valyala-fasthttp)
       ("go-github-com-valyala-bytebufferpool" ,go-github-com-valyala-bytebufferpool)
       ;("go-github-com-tyler-smith-go-bip39" ,go-github-com-tyler-smith-go-bip39)
       ("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)
       ;("go-github-com-stellar-throttled" ,go-github-com-stellar-throttled)
       ("go-github-com-stellar-go-xdr" ,go-github-com-stellar-go-xdr)
       ("go-github-com-spf13-viper" ,go-github-com-spf13-viper)
       ("go-github-com-spf13-pflag" ,go-github-com-spf13-pflag)
       ("go-github-com-spf13-jwalterweatherman" ,go-github-com-spf13-jwalterweatherman)
       ("go-github-com-spf13-cobra" ,go-github-com-spf13-cobra)
       ("go-github-com-spf13-cast" ,go-github-com-spf13-cast)
       ("go-github-com-smartystreets-goconvey" ,go-github-com-smartystreets-goconvey)
       ("go-github-com-sirupsen-logrus" ,go-github-com-sirupsen-logrus)
       ("go-github-com-shurcool-httpfs" ,go-github-com-shurcool-httpfs)
       ("go-github-com-sergi-go-diff" ,go-github-com-sergi-go-diff)
       ("go-github-com-segmentio-go-loggly" ,go-github-com-segmentio-go-loggly)
       ;("go-github-com-sebest-xff" ,go-github-com-sebest-xff)
       ;("go-github-com-rubenv-sql-migrate" ,go-github-com-rubenv-sql-migrate)
       ;("go-github-com-rs-xhandler" ,go-github-com-rs-xhandler)
       ("go-github-com-rs-cors" ,go-github-com-rs-cors)
       ("go-github-com-rcrowley-go-metrics" ,go-github-com-rcrowley-go-metrics)
       ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
       ("go-github-com-opentracing-opentracing-go" ,go-github-com-opentracing-opentracing-go)
       ("go-github-com-onsi-gomega" ,go-github-com-onsi-gomega)
       ("go-github-com-onsi-ginkgo" ,go-github-com-onsi-ginkgo)
       ;("go-github-com-nullstyle-go-xdr" ,go-github-com-nullstyle-go-xdr)
       ;("go-github-com-moul-http2curl" ,go-github-com-moul-http2curl)
       ;("go-github-com-mndrix-ps" ,go-github-com-mndrix-ps)
       ("go-github-com-mitchellh-mapstructure" ,go-github-com-mitchellh-mapstructure)
       ("go-github-com-mitchellh-go-homedir" ,go-github-com-mitchellh-go-homedir)
       ("go-github-com-mattn-go-colorable" ,go-github-com-mattn-go-colorable)
       ("go-github-com-manucorporat-sse" ,go-github-com-manucorporat-sse)
       ("go-github-com-magiconair-properties" ,go-github-com-magiconair-properties)
       ("go-github-com-lib-pq" ,go-github-com-lib-pq)
       ;("go-github-com-lann-builder" ,go-github-com-lann-builder)
       ("go-github-com-kr-text" ,go-github-com-kr-text)
       ("go-github-com-kr-pretty" ,go-github-com-kr-pretty)
       ;("go-github-com-klauspost-crc32" ,go-github-com-klauspost-crc32)
       ("go-github-com-klauspost-cpuid" ,go-github-com-klauspost-cpuid)
       ("go-github-com-klauspost-compress" ,go-github-com-klauspost-compress)
       ("go-github-com-k0kubun-colorstring" ,go-github-com-k0kubun-colorstring)
       ;("go-github-com-jmoiron-sqlx" ,go-github-com-jmoiron-sqlx)
       ("go-github-com-jmespath-go-jmespath" ,go-github-com-jmespath-go-jmespath)
       ("go-github-com-jarcoal-httpmock" ,go-github-com-jarcoal-httpmock)
       ("go-github-com-inconshreveable-mousetrap" ,go-github-com-inconshreveable-mousetrap)
       ("go-github-com-imkira-go-interpol" ,go-github-com-imkira-go-interpol)
       ;("go-github-com-howeyc-gopass" ,go-github-com-howeyc-gopass)
       ("go-github-com-hashicorp-golang-lru" ,go-github-com-hashicorp-golang-lru)
       ;("go-github-com-guregu-null" ,go-github-com-guregu-null)
       ;("go-github-com-graph-gophers-graphql-go" ,go-github-com-graph-gophers-graphql-go)
       ("go-github-com-google-go-querystring" ,go-github-com-google-go-querystring)
       ("go-github-com-gomodule-redigo" ,go-github-com-gomodule-redigo)
       ;("go-github-com-goji-httpauth" ,go-github-com-goji-httpauth)
       ;("go-github-com-gobuffalo-packr" ,go-github-com-gobuffalo-packr)
       ("go-github-com-go-ini-ini" ,go-github-com-go-ini-ini)
       ("go-github-com-go-errors-errors" ,go-github-com-go-errors-errors)
       ("go-github-com-go-chi-chi" ,go-github-com-go-chi-chi)
       ("go-github-com-getsentry-raven-go" ,go-github-com-getsentry-raven-go)
       ("go-github-com-gavv-monotime" ,go-github-com-gavv-monotime)
       ("go-github-com-fatih-structs" ,go-github-com-fatih-structs)
       ;("go-github-com-facebookgo-subset" ,go-github-com-facebookgo-subset)
       ;("go-github-com-facebookgo-structtag" ,go-github-com-facebookgo-structtag)
       ;("go-github-com-facebookgo-stack" ,go-github-com-facebookgo-stack)
       ;("go-github-com-facebookgo-inject" ,go-github-com-facebookgo-inject)
       ;("go-github-com-facebookgo-ensure" ,go-github-com-facebookgo-ensure)
       ;("go-github-com-elazarl-go-bindata-assetfs" ,go-github-com-elazarl-go-bindata-assetfs)
       ("go-github-com-aws-aws-sdk-go" ,go-github-com-aws-aws-sdk-go)
       ("go-github-com-asaskevich-govalidator" ,go-github-com-asaskevich-govalidator)
       ("go-github-com-ajg-form" ,go-github-com-ajg-form)
       ;("go-github-com-masterminds-squirrel" ,go-github-com-masterminds-squirrel)
       ("go-github-com-burntsushi-toml" ,go-github-com-burntsushi-toml)
       ;("go-bitbucket-org-ww-goautoneg" ,go-bitbucket-org-ww-goautoneg)
       ))
    (home-page "https://github.com/stellar/go")
    (synopsis "Package Index")
    (description
     "This repo is the home for all of the public Go code produced by the
@@url{https://stellar.org,Stellar Development Foundation}.")
    (license (list license:asl2.0 license:asl2.0))))

(define (go-github-com-stellar-go-package suffix)
  (package
    (name (string-append "go-github-com-stellar-go-"
                         (string-replace-substring suffix "/" "-")))
    (version "0.0.0-20191010205648-0fc3bfe3dfa7")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/stellar/go")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name "go-github-com-stellar-go" version))
        (sha256
         (base32
          "032n758lfycldrrb0ciscrm182jnx4nnqxdkl876scsacxxqm906"))))
    (build-system go-build-system)
    (arguments
     `(#:unpack-path "github.com/stellar/go"
       #:import-path ,(string-append "github.com/stellar/go/" suffix)
       #:tests? #f))
    (home-page "https://github.com/stellar/go")
    (synopsis #f)
    (description #f)
    (license license:asl2.0)))

(define-public go-github-com-stellar-go-address
  (package (inherit (go-github-com-stellar-go-package "address"))
    (propagated-inputs
     `(("go-github-com-asaskevich-govalidator" ,go-github-com-asaskevich-govalidator)
       ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
       ;("go-github-com-stellar-go-support" ,go-github-com-stellar-go-support)
       ))
    (native-inputs
     `(("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))))

(define-public go-github-com-stellar-go-amount
  (package (inherit (go-github-com-stellar-go-package "amount"))
    (propagated-inputs
     `(;("go-github-com-stellar-go-support" ,go-github-com-stellar-go-support)
       ("go-github-com-stellar-go-xdr" ,go-github-com-stellar-go-xdr)))))

(define-public go-github-com-stellar-go-build
  (package (inherit (go-github-com-stellar-go-package "build"))
    (propagated-inputs
     `(("go-github-com-lib-pq" ,go-github-com-lib-pq)
       ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
       ("go-github-com-stellar-go-xdr" ,go-github-com-stellar-go-xdr)
       ("go-golang-org-x-crypto" ,go-golang-org-x-crypto)))))

(define-public go-github-com-stellar-go-clients-federation
  (package (inherit (go-github-com-stellar-go-package "clients/federation"))
    (propagated-inputs
     `(
       ("go-github-com-stellar-go-address" ,go-github-com-stellar-go-address)
       ("go-github-com-stellar-go-clients-horizonclient" ,go-github-com-stellar-go-clients-horizonclient)
       ("go-github-com-stellar-go-clients-stellartoml" ,go-github-com-stellar-go-clients-stellartoml)
       ;("go-github-com-stellar-go-support" ,go-github-com-stellar-go-support)
       ("go-github-com-stellar-go-protocols-federation" ,go-github-com-stellar-go-protocols-federation)
       ))
    (native-inputs
     `(
       ;("go-github-com-stellar-go-support" ,go-github-com-stellar-go-support)
       ("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)
       ))))

(define-public go-github-com-stellar-go-clients-horizon
  (package (inherit (go-github-com-stellar-go-package "clients/horizon"))
    (propagated-inputs
     `())
    (native-inputs
     `())))

(define-public go-github-com-stellar-go-clients-horizonclient
  (package (inherit (go-github-com-stellar-go-package "clients/horizonclient"))
    (propagated-inputs
     `(("go-github-com-go-chi-chi" ,go-github-com-go-chi-chi)
       ("go-github-com-go-errors-errors" ,go-github-com-go-errors-errors)
       ("go-github-com-gorilla-schema" ,go-github-com-gorilla-schema)
       ("go-github-com-lib-pq" ,go-github-com-lib-pq)
       ("go-github-com-manucorporat-sse" ,go-github-com-manucorporat-sse)
       ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
       ("go-github-com-segmentio-go-loggly" ,go-github-com-segmentio-go-loggly)
       ("go-github-com-sirupsen-logrus" ,go-github-com-sirupsen-logrus)
       ("go-github-com-stellar-go-xdr-xdr3" ,go-github-com-stellar-go-xdr-xdr3)
       ("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))
    (native-inputs
     `(("go-github-com-jarcoal-httpmock" ,go-github-com-jarcoal-httpmock)
       ("go-gopkg-in-gavv-httpexpect-v1" ,go-gopkg-in-gavv-httpexpect-v1)))))

(define-public go-github-com-stellar-go-clients-stellartoml
  (package (inherit (go-github-com-stellar-go-package "clients/stellartoml"))
    (propagated-inputs
     `(("go-github-com-asaskevich-govalidator" ,go-github-com-asaskevich-govalidator)
       ("go-github-com-burntsushi-toml" ,go-github-com-burntsushi-toml)
       ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
       ("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))
    (native-inputs
     `(("go-github-com-jarcoal-httpmock" ,go-github-com-jarcoal-httpmock)
       ("go-gopkg-in-gavv-httpexpect-v1" ,go-gopkg-in-gavv-httpexpect-v1)))))

(define-public go-github-com-stellar-go-crc16
  (package (inherit (go-github-com-stellar-go-package "crc16"))))

(define-public go-github-com-stellar-go-hash
  (package (inherit (go-github-com-stellar-go-package "hash"))
    (native-inputs
     `(("go-github-com-onsi-ginkgo" ,go-github-com-onsi-ginkgo)
       ("go-github-com-onsi-gomega" ,go-github-com-onsi-gomega)))))

(define-public go-github-com-stellar-go-keypair
  (package (inherit (go-github-com-stellar-go-package "keypair"))
    (propagated-inputs
     `(("go-github-com-lib-pq" ,go-github-com-lib-pq)
       ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
       ("go-github-com-stellar-go-xdr-xdr3" ,go-github-com-stellar-go-xdr-xdr3)
       ("go-golang-org-x-crypto" ,go-golang-org-x-crypto)))
    (native-inputs
     `(("go-github-com-onsi-ginkgo" ,go-github-com-onsi-ginkgo)
       ("go-github-com-onsi-gomega" ,go-github-com-onsi-gomega)))))

(define-public go-github-com-stellar-go-network
  (package (inherit (go-github-com-stellar-go-package "network"))
    (propagated-inputs
     `(("go-github-com-lib-pq" ,go-github-com-lib-pq)
       ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
       ("go-github-com-stellar-go-xdr-xdr3" ,go-github-com-stellar-go-xdr-xdr3)))
    (native-inputs
     `(("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))))

(define-public go-github-com-stellar-go-price
  (package (inherit (go-github-com-stellar-go-package "price"))
    (propagated-inputs
     `(("go-github-com-lib-pq" ,go-github-com-lib-pq)
       ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
       ("go-github-com-stellar-go-xdr-xdr3" ,go-github-com-stellar-go-xdr-xdr3)))
    (native-inputs
     `(("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))))

(define-public go-github-com-stellar-go-protocols-federation
  (package (inherit (go-github-com-stellar-go-package "protocols/federation"))
    (native-inputs
     `(("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))))

(define-public go-github-com-stellar-go-protocols-horizon
  (package (inherit (go-github-com-stellar-go-package "protocols/horizon"))
    (propagated-inputs
     `(("go-github-com-go-chi-chi" ,go-github-com-go-chi-chi)
       ("go-github-com-go-errors-errors" ,go-github-com-go-errors-errors)
       ("go-github-com-gorilla-schema" ,go-github-com-gorilla-schema)
       ("go-github-com-lib-pq" ,go-github-com-lib-pq)
       ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
       ("go-github-com-segmentio-go-loggly" ,go-github-com-segmentio-go-loggly)
       ("go-github-com-sirupsen-logrus" ,go-github-com-sirupsen-logrus)
       ("go-github-com-stellar-go-xdr-xdr3" ,go-github-com-stellar-go-xdr-xdr3)))))

(define-public go-github-com-stellar-go-strkey
  (package (inherit (go-github-com-stellar-go-package "strkey"))
    (propagated-inputs
     `(("go-github-com-pkg-errors" ,go-github-com-pkg-errors)))
    (native-inputs
     `(("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))))

(define-public go-github-com-stellar-go-support-clock
  (package (inherit (go-github-com-stellar-go-package "support/clock"))
    (native-inputs
     `(("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))))

(define-public go-github-com-stellar-go-support-errors
  (package (inherit (go-github-com-stellar-go-package "support/errors"))
    (propagated-inputs
     `(("go-github-com-pkg-errors" ,go-github-com-pkg-errors)))))

(define-public go-github-com-stellar-go-support-http-httpdecode
  (package (inherit (go-github-com-stellar-go-package "support/http/httpdecode"))
    (propagated-inputs
     `(("go-github-com-go-chi-chi" ,go-github-com-go-chi-chi)
       ("go-github-com-gorilla-schema" ,go-github-com-gorilla-schema)
       ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)))
    (native-inputs
     `(("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))))

(define-public go-github-com-stellar-go-support-log
  (package (inherit (go-github-com-stellar-go-package "support/log"))
    (propagated-inputs
     `(("go-github-com-go-errors-errors" ,go-github-com-go-errors-errors)
       ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
       ("go-github-com-segmentio-go-loggly" ,go-github-com-segmentio-go-loggly)
       ("go-github-com-sirupsen-logrus" ,go-github-com-sirupsen-logrus)))))

(define-public go-github-com-stellar-go-support-render-hal
  (package (inherit (go-github-com-stellar-go-package "support/render/hal"))
    (propagated-inputs
     `(("go-github-com-go-chi-chi" ,go-github-com-go-chi-chi)
       ("go-github-com-go-errors-errors" ,go-github-com-go-errors-errors)
       ("go-github-com-gorilla-schema" ,go-github-com-gorilla-schema)
       ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
       ("go-github-com-segmentio-go-loggly" ,go-github-com-segmentio-go-loggly)
       ("go-github-com-sirupsen-logrus" ,go-github-com-sirupsen-logrus)))))

(define-public go-github-com-stellar-go-support-render-httpjson
  (package (inherit (go-github-com-stellar-go-package "support/render/httpjson"))
    (propagated-inputs
     `(("go-github-com-go-chi-chi" ,go-github-com-go-chi-chi)
       ("go-github-com-go-errors-errors" ,go-github-com-go-errors-errors)
       ("go-github-com-gorilla-schema" ,go-github-com-gorilla-schema)
       ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
       ("go-github-com-segmentio-go-loggly" ,go-github-com-segmentio-go-loggly)
       ("go-github-com-sirupsen-logrus" ,go-github-com-sirupsen-logrus)))))

(define-public go-github-com-stellar-go-support-render-problem
  (package (inherit (go-github-com-stellar-go-package "support/render/problem"))
    (propagated-inputs
     `(("go-github-com-go-errors-errors" ,go-github-com-go-errors-errors)
       ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
       ("go-github-com-segmentio-go-loggly" ,go-github-com-segmentio-go-loggly)
       ("go-github-com-sirupsen-logrus" ,go-github-com-sirupsen-logrus)))))

(define-public go-github-com-stellar-go-support-url
  (package (inherit (go-github-com-stellar-go-package "support/url"))
    (native-inputs
     `(("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))))

(define-public go-github-com-stellar-go-txnbuild
  (package (inherit (go-github-com-stellar-go-package "txnbuild"))
    (propagated-inputs
     `(("go-github-com-lib-pq" ,go-github-com-lib-pq)
       ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
       ;("go-github-com-stellar-go-amount" ,go-github-com-stellar-go-amount)
       ("go-github-com-stellar-go-xdr-xdr3" ,go-github-com-stellar-go-xdr-xdr3)))
    (native-inputs
     `(("go-github-com-go-chi-chi" ,go-github-com-go-chi-chi)
       ("go-github-com-go-errors-errors" ,go-github-com-go-errors-errors)
       ("go-github-com-gorilla-schema" ,go-github-com-gorilla-schema)
       ("go-github-com-manucorporat-sse" ,go-github-com-manucorporat-sse)
       ("go-github-com-segmentio-go-loggly" ,go-github-com-segmentio-go-loggly)
       ("go-github-com-sirupsen-logrus" ,go-github-com-sirupsen-logrus)))))

(define-public go-github-com-stellar-go-xdr
  (let ((base (go-github-com-stellar-go-package "xdr")))
    (package (inherit base)
      (propagated-inputs
       `(("go-github-com-lib-pq" ,go-github-com-lib-pq)
         ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
         ("go-github-com-stellar-go-xdr-xdr3" ,go-github-com-stellar-go-xdr-xdr3)))
      (native-inputs
       `(("go-github-com-onsi-ginkgo" ,go-github-com-onsi-ginkgo)
         ("go-github-com-onsi-gomega" ,go-github-com-onsi-gomega)
         ("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))
      (arguments
       ;; Tests expect a 64-bit system.
       `(#:tests? ,(target-64bit?)
         ,@(package-arguments base))))))

(define-public go-github-com-stellar-go-xdr-xdr3
  (package
    (name "go-github-com-stellar-go-xdr-xdr3")
    (version "0.0.0-20201028102745-f80a23dac78a")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/stellar/go-xdr")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1yvqvxgpax53d8m0l97q7k416gpaj33jzzaw5sa3q4kdsb9hl1j8"))))
    (build-system go-build-system)
    (arguments
     '(#:unpack-path "github.com/stellar/go-xdr"
       #:import-path "github.com/stellar/go-xdr/xdr3"))
    (home-page "https://github.com/stellar/go-xdr")
    (synopsis "go-xdr")
    (description #f)
    (license license:isc)))

(define-public go-github-com-steveyen-gtreap
  (package
    (name "go-github-com-steveyen-gtreap")
    (version "0.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/steveyen/gtreap")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0qim822f4f6r6ks1z3qvzxljjv6kzrz3bm5bixg6sjz1c2cqvj2j"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/steveyen/gtreap"))
    (home-page "https://github.com/steveyen/gtreap")
    (synopsis "gtreap")
    (description
     "gtreap is an immutable treap implementation in the Go Language")
    (license license:expat)))

(define-public go-github-com-strib-gomounts
  (package
    (name "go-github-com-strib-gomounts")
    (version "0.0.0-20180215003523-d9ea4eaa52ca")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/strib/gomounts")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0rnmgcsjjv8kwxlk76n7fbigxxnb2y9nk6zqi0yrx0ikcq409qvw"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/strib/gomounts"))
    (home-page "https://github.com/strib/gomounts")
    (synopsis "gomounts")
    (description
     "Package gomounts implements a cross-platform library for retrieving mounted
filesystem volumes.")
    (license license:expat)))

(define-public go-github-com-tailscale-golang-x-crypto
  (package
    (name "go-github-com-tailscale-golang-x-crypto")
    (version "v0.0.0-20221102133106-bc99ab8c2d17")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tailscale/golang-x-crypto")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0000000000000000000000000000000004aq2mmhbaj3x1ckrcbb"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/tailscale/golang-x-crypto"
       #:phases
       (modify-phases %standard-phases
         (delete 'build)
         (delete 'check))))
    (propagated-inputs `(("go-golang-org-x-text" ,go-golang-org-x-text)
                         ("go-golang-org-x-crypto" ,go-golang-org-x-crypto)
                         ("go-golang-org-x-term" ,go-golang-org-x-term)
                         ("go-golang-org-x-sys" ,go-golang-org-x-sys)
                         ("go-golang-org-x-net" ,go-golang-org-x-net)))
    (home-page "https://github.com/tailscale/golang-x-crypto")
    (synopsis "golang-x-crypto")
    (description
     "This is a temporary dev fork of Go's @@code{golang.org/x/crypto} module, but
only for @@code{ssh} development.")
    (license license:bsd-3)))

(define-public go-github-com-tailscale-goupnp
  (package
    (name "go-github-com-tailscale-goupnp")
    (version "1.0.1-0.20210804011211-c64d0f06ea05")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tailscale/goupnp")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "10nkgf29jwg8qp2lkxgnh3cf2jiwjc2pbga03rdkmhiyhrqhj4rx"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/tailscale/goupnp"))
    (propagated-inputs `(("go-golang-org-x-sync" ,go-golang-org-x-sync)))
    (home-page "https://github.com/tailscale/goupnp")
    (synopsis "tailscale/goupnp")
    (description
     "goupnp is an implementation of a client for various UPnP services.")
    (license license:bsd-2)))

(define-public go-github-com-tailscale-netlink
  (package
    (name "go-github-com-tailscale-netlink")
    (version "1.1.1-0.20211101221916-cabfb018fe85")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tailscale/netlink")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0brf9k2ynla12gn0237gll6wdi1zk49mxfgwdi4cqccvlfmbkhkc"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f      ; Tests need network access
       #:import-path "github.com/vishvananda/netlink"))
     ;'(#:import-path "github.com/tailscale/netlink"))
    (propagated-inputs `(("go-golang-org-x-sys" ,go-golang-org-x-sys)
                         ("go-github-com-vishvananda-netns" ,go-github-com-vishvananda-netns)))
    (home-page "https://github.com/tailscale/netlink")
    (synopsis "netlink - netlink library for go")
    (description
     "Package netlink provides a simple library for netlink.  Netlink is the interface
a user-space program in linux uses to communicate with the kernel.  It can be
used to add and remove interfaces, set up ip addresses and routes, and confiugre
ipsec.  Netlink communication requires elevated privileges, so in most cases
this code needs to be run as root.  The low level primitives for netlink are
contained in the nl subpackage.  This package attempts to provide a high-level
interface that is loosely modeled on the iproute2 cli.")
    (license license:asl2.0)))

(define-public go-github-com-tcnksm-go-httpstat
  (package
    (name "go-github-com-tcnksm-go-httpstat")
    (version "0.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tcnksm/go-httpstat")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "18jn40ra97waxx1mf23pkh6rq46y0nqd7vi3zcx9cwc39zqaf9bc"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f      ; Tests require network access
       #:import-path "github.com/tcnksm/go-httpstat"))
    (home-page "https://github.com/tcnksm/go-httpstat")
    (synopsis "go-httpstat")
    (description
     "Package httpstat traces HTTP latency information (DNSLookup, TCP Connection and
so on) on any golang HTTP request.  It uses `httptrace` package.  Just create
`go-httpstat` powered `context.Context` and give it your `http.Request` (no big
code modification is required).")
    (license license:expat)))

(define-public go-github-com-temoto-robotstxt
  (package
    (name "go-github-com-temoto-robotstxt")
    (version "1.1.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/temoto/robotstxt")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0a1bbym8gr9wyanh2br6hmxhdbqpfdr3nb56b4bvglad19fv2fpg"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/temoto/robotstxt"))
    (native-inputs
     `(("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))
    (home-page "https://github.com/temoto/robotstxt")
    (synopsis #f)
    (description #f)
    (license license:expat)))

;; ready to upstream
(define-public go-github-com-toqueteos-webbrowser
  (package
    (name "go-github-com-toqueteos-webbrowser")
    (version "1.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/toqueteos/webbrowser")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1j2hz0mq06v4vxksssg20yb34wwh24l55v2x7nplksfri1rmwbn0"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/toqueteos/webbrowser"))
    (home-page "https://github.com/toqueteos/webbrowser")
    (synopsis "Open web pages in the default browser")
    (description "Webbrowser provides a simple API for opening web pages on your
default browser.")
    (license license:expat)))

(define-public go-github-com-tstranex-u2f
  (package
    (name "go-github-com-tstranex-u2f")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/tstranex/u2f")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0xgyxmi8amlx35f23ldlkn900cyic77r525wpk5s58cpyw3hn5cd"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/tstranex/u2f"))
    (home-page "https://github.com/tstranex/u2f")
    (synopsis "Go FIDO U2F Library")
    (description
      "Package u2f implements the server-side parts of the FIDO Universal 2nd Factor
(U2F) specification.")
    (license license:expat)))

(define-public go-github-com-ttacon-chalk
  (package
    (name "go-github-com-ttacon-chalk")
    (version "0.0.0-20160626202418-22c06c80ed31")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ttacon/chalk")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1yacf9w6vp36hkhl9lq3gk7c551jcsbs348ivv2h3lwkhznldiwl"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/ttacon/chalk"))
    (home-page "https://github.com/ttacon/chalk")
    (synopsis "chalk")
    (description #f)
    (license license:expat)))

(define-public go-github-com-ugorji-go
  (package
    (name "go-github-com-ugorji-go")
    (version "1.2.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ugorji/go")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0nwqx41f82r9rmdhzi01bgvwpsa7jgcl3s6n3r7q3hq48kw13g67"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/ugorji/go"))
    (home-page "https://github.com/ugorji/go")
    (synopsis "Golang go-codec library")
    (description
     "This repository contains the @code{go-codec} library, the @code{codecgen} tool
and benchmarks for comparing against other libraries.")
    (license license:expat)))

(define-public go-github-com-unknwon-com
  ;; Archived repository, this commit fixes build with go-1.15+
  (let ((commit "b41c64acd94be7e673c9c8301344d31cce99e06c")
        (revision "1"))
    (package
      (name "go-github-com-unknwon-com")
      (version (git-version "1.0.1" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/unknwon/com")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32 "11yn59zhyhlmnwa005a1ashqahcrybnyv208gpp8dfx28m2flbys"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/unknwon/com"
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-tests
             (lambda* (#:key import-path #:allow-other-keys)
               ;; This test tries to use curl and connect to the internet.
               (delete-file (string-append "src/" import-path "/http_test.go")))))))
      (propagated-inputs
       (list go-github-com-smartystreets-goconvey
             go-github-com-smarty-assertions
             go-github-com-jtolds-gls
             go-github-com-gopherjs-gopherjs))
      (home-page "https://github.com/unknwon/com")
      (synopsis "Common Functions")
      (description
        "Package com is an open source project for commonly used functions for the Go
programming language.")
      (license license:asl2.0))))

(define-public go-github-com-unknwon-i18n
  (package
    (name "go-github-com-unknwon-i18n")
    (version "0.0.0-20210904045753-ff3a8617e361")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/unknwon/i18n")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0wbbqzh8j03z5w8bw9ald3jlvz18fk3889hdrz4dbig94di60ksi"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/unknwon/i18n"))
    (propagated-inputs
     (list go-gopkg-in-ini-v1
           go-github-com-unknwon-com
           go-github-com-smartystreets-goconvey))
    (home-page "https://github.com/unknwon/i18n")
    (synopsis "i18n")
    (description
      "Package i18n is for app Internationalization and Localization.")
    (license license:asl2.0)))

(define-public go-github-com-unknwon-paginater
  (package
    (name "go-github-com-unknwon-paginater")
    (version "0.0.0-20200328080006-042474bd0eae")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/unknwon/paginater")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "15cyx8vhik5b5mlnywspak51fjbppf4ihs045gp9k5q2jqd5acgw"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/unknwon/paginater"))
    (propagated-inputs
     (list go-github-com-smartystreets-goconvey))
    (home-page "https://github.com/unknwon/paginater")
    (synopsis "Paginater")
    (description
      "Package paginater is a helper module for custom pagination calculation.")
    (license license:asl2.0)))

(define-public go-github-com-unrolled-render
  (package
    (name "go-github-com-unrolled-render")
    (version "1.4.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/unrolled/render")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "088cp6k58cha6m814qafwix50pp5dnj4kac63d5n9zggrsgb9q3q"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/unrolled/render"))
    (propagated-inputs
     (list go-golang-org-x-sys
           go-github-com-fsnotify-fsnotify))
    (home-page "https://github.com/unrolled/render")
    (synopsis "Render")
    (description
      "Package render is a package that provides functionality for easily rendering
JSON, XML, binary data, and HTML templates.")
    (license license:expat)))

(define-public go-github-com-u-root-u-root
  (package
    (name "go-github-com-u-root-u-root")
    (version "0.9.1-0.20230109201855-948a78c969ad")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/u-root/u-root")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "157539vjqkw5r7p8lx0xdykrga1ivwdpv47dywn71swamy02v7jw"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f      ; Tests seem to hang forever
       #:import-path "github.com/u-root/u-root"))
    (propagated-inputs `(("go-google-golang-org-grpc" ,go-google-golang-org-grpc)
                         ("go-golang-org-x-xerrors" ,go-golang-org-x-xerrors)
                         ("go-golang-org-x-sync" ,go-golang-org-x-sync)
                         ("go-golang-org-x-net" ,go-golang-org-x-net)
                         ("go-golang-org-x-mod" ,go-golang-org-x-mod)
                         ("go-github-com-vishvananda-netns" ,go-github-com-vishvananda-netns)
                         ("go-github-com-u-root-uio" ,go-github-com-u-root-uio)
                         ("go-github-com-pmezard-go-difflib" ,go-github-com-pmezard-go-difflib)
                         ;("go-github-com-pkg-term" ,go-github-com-pkg-term)
                         ("go-github-com-mdlayher-raw" ,go-github-com-mdlayher-raw)
                         ("go-github-com-mdlayher-netlink" ,go-github-com-mdlayher-netlink)
                         ("go-github-com-mdlayher-ethernet" ,go-github-com-mdlayher-ethernet)
                         ("go-github-com-mattn-go-tty" ,go-github-com-mattn-go-tty)
                         ("go-github-com-mattn-go-runewidth" ,go-github-com-mattn-go-runewidth)
                         ("go-github-com-mattn-go-isatty" ,go-github-com-mattn-go-isatty)
                         ("go-github-com-mattn-go-colorable" ,go-github-com-mattn-go-colorable)
                         ;("go-github-com-kaey-framebuffer" ,go-github-com-kaey-framebuffer)
                         ("go-github-com-jsimonetti-rtnetlink" ,go-github-com-jsimonetti-rtnetlink)
                         ("go-github-com-google-goterm" ,go-github-com-google-goterm)
                         ("go-github-com-anmitsu-go-shlex" ,go-github-com-anmitsu-go-shlex)
                         ;("go-src-elv-sh" ,go-src-elv-sh)
                         ;("go-pack-ag-tftp" ,go-pack-ag-tftp)
                         ;("go-mvdan-cc-sh-v3" ,go-mvdan-cc-sh-v3)
                         ("go-gopkg-in-yaml-v2" ,go-gopkg-in-yaml-v2)
                         ("go-golang-org-x-tools" ,go-golang-org-x-tools)
                         ("go-golang-org-x-text" ,go-golang-org-x-text)
                         ("go-golang-org-x-term" ,go-golang-org-x-term)
                         ("go-golang-org-x-sys" ,go-golang-org-x-sys)
                         ("go-golang-org-x-crypto" ,go-golang-org-x-crypto)
                         ;("go-github-com-vtolstov-go-ioctl" ,go-github-com-vtolstov-go-ioctl)
                         ("go-github-com-vishvananda-netlink" ,go-github-com-vishvananda-netlink)
                         ("go-github-com-ulikunitz-xz" ,go-github-com-ulikunitz-xz)
                         ;("go-github-com-u-root-iscsinl" ,go-github-com-u-root-iscsinl)
                         ;("go-github-com-u-root-gobusybox-src" ,go-github-com-u-root-gobusybox-src)
                         ("go-github-com-spf13-pflag" ,go-github-com-spf13-pflag)
                         ;("go-github-com-safchain-ethtool" ,go-github-com-safchain-ethtool)
                         ;("go-github-com-rekby-gpt" ,go-github-com-rekby-gpt)
                         ;("go-github-com-rck-unit" ,go-github-com-rck-unit)
                         ("go-github-com-pierrec-lz4-v4" ,go-github-com-pierrec-lz4-v4)
                         ;("go-github-com-pborman-getopt-v2" ,go-github-com-pborman-getopt-v2)
                         ;("go-github-com-orangecms-go-framebuffer" ,go-github-com-orangecms-go-framebuffer)
                         ;("go-github-com-nanmu42-limitio" ,go-github-com-nanmu42-limitio)
                         ;("go-github-com-kr-pty" ,go-github-com-kr-pty)
                         ("go-github-com-klauspost-pgzip" ,go-github-com-klauspost-pgzip)
                         ("go-github-com-klauspost-compress" ,go-github-com-klauspost-compress)
                         ("go-github-com-kevinburke-ssh-config" ,go-github-com-kevinburke-ssh-config)
                         ;("go-github-com-intel-go-cpuid" ,go-github-com-intel-go-cpuid)
                         ("go-github-com-insomniacslk-dhcp" ,go-github-com-insomniacslk-dhcp)
                         ("go-github-com-google-uuid" ,go-github-com-google-uuid)
                         ;("go-github-com-google-goexpect" ,go-github-com-google-goexpect)
                         ("go-github-com-google-go-tpm" ,go-github-com-google-go-tpm)
                         ("go-github-com-google-go-cmp" ,go-github-com-google-go-cmp)
                         ;("go-github-com-gojuno-minimock-v3" ,go-github-com-gojuno-minimock-v3)
                         ("go-github-com-gliderlabs-ssh" ,go-github-com-gliderlabs-ssh)
                         ("go-github-com-dustin-go-humanize" ,go-github-com-dustin-go-humanize)
                         ("go-github-com-davecgh-go-spew" ,go-github-com-davecgh-go-spew)
                         ("go-github-com-creack-pty" ,go-github-com-creack-pty)
                         ("go-github-com-cenkalti-backoff-v4" ,go-github-com-cenkalti-backoff-v4)
                         ;("go-github-com-c-bata-go-prompt" ,go-github-com-c-bata-go-prompt)
                         ;("go-github-com-beevik-ntp" ,go-github-com-beevik-ntp)
                         ))
    (home-page "https://github.com/u-root/u-root")
    (synopsis "u-root")
    (description "u-root embodies four different projects.")
    (license license:bsd-3)))

(define-public go-github-com-u-root-uio
  (package
    (name "go-github-com-u-root-uio")
    (version "0.0.0-20221210192040-301ac5150d9e")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/u-root/uio")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1h11mhxgxwyvmlm8a2j9n0lg8d6scblz9fhqpk5ljkc6407ywsqy"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/u-root/uio"
       #:phases
       (modify-phases %standard-phases
         (delete 'build)
         (delete 'check))))
    (propagated-inputs `(("go-golang-org-x-sys" ,go-golang-org-x-sys)))
    (home-page "https://github.com/u-root/uio")
    (synopsis "uio")
    (description #f)
    (license license:bsd-3)))

(define-public go-github-com-vishvananda-netlink
  (package
    (name "go-github-com-vishvananda-netlink")
    (version "1.1.1-0.20211118161826-650dca95af54")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/vishvananda/netlink")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "18j88lzynik932i4h4qrpdwz9w5667sb845kh33zsl10ywyipx74"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f      ; Tests need network access
       #:import-path "github.com/vishvananda/netlink"))
    (propagated-inputs `(("go-golang-org-x-sys" ,go-golang-org-x-sys)
                         ("go-github-com-vishvananda-netns" ,go-github-com-vishvananda-netns)))
    (home-page "https://github.com/vishvananda/netlink")
    (synopsis "netlink - netlink library for go")
    (description
     "Package netlink provides a simple library for netlink.  Netlink is the interface
a user-space program in linux uses to communicate with the kernel.  It can be
used to add and remove interfaces, set up ip addresses and routes, and confiugre
ipsec.  Netlink communication requires elevated privileges, so in most cases
this code needs to be run as root.  The low level primitives for netlink are
contained in the nl subpackage.  This package attempts to provide a high-level
interface that is loosely modeled on the iproute2 cli.")
    (license license:asl2.0)))

(define-public go-github-com-xeipuuv-gojsonpointer
  (package
    (name "go-github-com-xeipuuv-gojsonpointer")
    (version "0.0.0-20190905194746-02993c407bfb")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/xeipuuv/gojsonpointer")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0y7gmpgsm8c12ax4a0ij9srmd9d424iq224n172ckwfqf37amvzy"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/xeipuuv/gojsonpointer"))
    (native-inputs
     `(("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))
    (home-page "https://github.com/xeipuuv/gojsonpointer")
    (synopsis "gojsonpointer")
    (description #f)
    (license license:asl2.0)))

(define-public go-github-com-xeipuuv-gojsonreference
  (package
    (name "go-github-com-xeipuuv-gojsonreference")
    (version "0.0.0-20180127040603-bd5ef7bd5415")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/xeipuuv/gojsonreference")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1xby79padc7bmyb8rfbad8wfnfdzpnh51b1n8c0kibch0kwc1db5"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/xeipuuv/gojsonreference"))
    (propagated-inputs
     `(("go-github-com-xeipuuv-gojsonpointer" ,go-github-com-xeipuuv-gojsonpointer)))
    (home-page "https://github.com/xeipuuv/gojsonreference")
    (synopsis "gojsonreference")
    (description #f)
    (license license:asl2.0)))

(define-public go-github-com-xeipuuv-gojsonschema
  (package
    (name "go-github-com-xeipuuv-gojsonschema")
    (version "1.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/xeipuuv/gojsonschema")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1mqiq0r8qw4qlfp3ls8073r6514rmzwrmdn4j33rppk3zh942i6l"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/xeipuuv/gojsonschema"
       #:tests? #f))
    (propagated-inputs
     `(("go-github-com-xeipuuv-gojsonreference" ,go-github-com-xeipuuv-gojsonreference)))
    (native-inputs
     `(("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))
    (home-page "https://github.com/xeipuuv/gojsonschema")
    (synopsis #f)
    (description #f)
    (license license:asl2.0)))

(define-public go-github-com-yalp-jsonpath
  (package
    (name "go-github-com-yalp-jsonpath")
    (version "0.0.0-20180802001716-5cc68e5049a0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/yalp/jsonpath")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0kkyxp1cg3kfxy5hhwzxg132jin4xb492z5jpqq94ix15v6rdf4b"))))
    (build-system go-build-system)
    (arguments
    '(#:import-path "github.com/yalp/jsonpath"))
    (home-page "https://github.com/yalp/jsonpath")
    (synopsis #f)
    (description #f)
    (license license:bsd-3)))

(define-public go-github-com-yohcop-openid-go
  (package
    (name "go-github-com-yohcop-openid-go")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/yohcop/openid-go")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0ziwacqiyc62392nxnfyghr3fgfjxbnbgxdpbixrlviahz7mnh4f"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/yohcop/openid-go"))
    (propagated-inputs
     (list go-golang-org-x-net))
    (home-page "https://github.com/yohcop/openid-go")
    (synopsis "openid.go")
    (description
      "This is a consumer (Relying party) implementation of OpenId 2.0, written in Go.")
    (license license:asl2.0)))

(define-public go-github-com-yudai-pp
  (package
    (name "go-github-com-yudai-pp")
    (version "2.0.1-20150810000000-be8315415630")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/yudai/pp")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0jbhgcxabq9jikwxkrcrp0q4xpl9fisx9rvzgxp5ma97phflw5gb"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/yudai/pp"
       ;; Tests haven't withstood the test of time.
       #:tests? #f))
    (propagated-inputs
     `(("go-github-com-mattn-go-colorable" ,go-github-com-mattn-go-colorable)
       ("go-github-com-mattn-go-isatty" ,go-github-com-mattn-go-isatty)))
    (native-inputs
     `(("go-github-com-k0kubun-colorstring" ,go-github-com-k0kubun-colorstring)))
    (home-page "https://github.com/yudai/pp")
    (synopsis "Colored pretty printer for Go language")
    (description #f)
    (license license:expat)))

(define-public go-github-com-yuin-goldmark-highlighting
  (package
    (name "go-github-com-yuin-goldmark-highlighting")
    (version "0.0.0-20210516132338-9216f9c5aa01")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/yuin/goldmark-highlighting")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1dl2pjwd8j1pf09zhsb9xkz1q59kgjgv0q0w7lgwsrfpgajdqyw4"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f      ; TODO: figure out undefined testutil.DiffPretty
       #:import-path "github.com/yuin/goldmark-highlighting"))
    (propagated-inputs
     (list go-github-com-alecthomas-chroma
           go-github-com-danwakefield-fnmatch
           go-github-com-dlclark-regexp2
           go-github-com-yuin-goldmark))
    (home-page "https://github.com/yuin/goldmark-highlighting")
    (synopsis "goldmark-highlighting")
    (description
      "package highlighting is a extension for the
goldmark(@url{http://github.com/yuin/goldmark,http://github.com/yuin/goldmark}).")
    (license license:expat)))

(define-public go-github-com-yuin-goldmark-meta
  (package
    (name "go-github-com-yuin-goldmark-meta")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/yuin/goldmark-meta")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0jx2087lyq0vxsag79hyk9vmiw1416qhq7lmlynqwrmkkicr98jp"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/yuin/goldmark-meta"))
    (propagated-inputs
     (list go-gopkg-in-yaml-v2
           go-github-com-yuin-goldmark))
    (home-page "https://github.com/yuin/goldmark-meta")
    (synopsis "goldmark-meta")
    (description
      "package meta is a extension for the
goldmark(@url{http://github.com/yuin/goldmark,http://github.com/yuin/goldmark}).")
    (license license:expat)))

(define-public go-github-com-zeripath-jwt
  (package
    (name "go-github-com-zeripath-jwt")
    (version "3.2.2+incompatible")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/zeripath/jwt")
               (commit (string-append "v" (go-version->git-ref version)))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0hq8wz11g6kddx9ab0icl5h3k4lrivk1ixappnr5db2ng2wjks9c"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/zeripath/jwt"))
    (native-inputs
     (list go-github-com-golang-jwt-jwt))
    (home-page "https://github.com/zeripath/jwt")
    (synopsis "jwt-go")
    (description
      "Package jwt is a Go implementation of JSON Web Tokens:
@url{http://self-issued.info/docs/draft-jones-json-web-token.html,http://self-issued.info/docs/draft-jones-json-web-token.html}")
    (license license:expat)))

(define-public go-github-com-ziutek-mymysql
  (package
    (name "go-github-com-ziutek-mymysql")
    (version "1.5.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ziutek/mymysql")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "172s7sv5bgc40x81k18hypf9c4n8hn9v5w5zwyr4mi5prbavqcci"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/ziutek/mymysql"
       #:phases
       (modify-phases %standard-phases
         (delete 'build)
         (delete 'check))))
    (home-page "https://github.com/ziutek/mymysql")
    (synopsis "MyMySQL v1.5.4 (2015-01-08)")
    (description
      "Sorry for my poor English.  If you can help with improving the English in this
documentation, please contact me.")
    (license license:bsd-3)))

(define-public go-go4-org-mem
  (package
    (name "go-go4-org-mem")
    (version "0.0.0-20210711025021-927187094b94")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/go4org/mem")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0rcijgwf1xr1rwi7sqw17mx5kwy6ykfvca1ybw4rdyr5ldprjg4y"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "go4.org/mem"))
    (home-page "https://go4.org/mem")
    (synopsis "go4.org/mem")
    (description
     "Package mem provides the mem.RO type that allows you to cheaply pass & access
either a read-only []byte or a string.")
    (license license:asl2.0)))

(define-public go-go4-org-netipx
  (package
    (name "go-go4-org-netipx")
    (version "0.0.0-20220725152314-7e7bdc8411bf")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/go4org/netipx")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0s9hnl1n6bfvwyxcgzrx4vdfm64ajiix9nnwx60xmh248h8f4z9a"))))
    (build-system go-build-system)
    (arguments
     `(#:tests? #f      ; TODO
       #:import-path "go4.org/netipx"))
    (propagated-inputs `(("go-golang-org-x-tools" ,go-golang-org-x-tools)
                         ("go-golang-org-x-sys" ,go-golang-org-x-sys)
                         ("go-golang-org-x-mod" ,go-golang-org-x-mod)
                         ("go-golang-org-x-exp" ,go-golang-org-x-exp)
                         ;("go-go4-org-unsafe-assume-no-moving-gc" ,go-go4-org-unsafe-assume-no-moving-gc)
                         ("go-github-com-burntsushi-toml" ,go-github-com-burntsushi-toml)
                         ("go-honnef-co-go-tools" ,go-honnef-co-go-tools)
                         ;("go-github-com-dvyukov-go-fuzz" ,go-github-com-dvyukov-go-fuzz)
                         ))
    ;(native-inputs
    ; (list go-go4-org-intern))
    (home-page "https://go4.org/netipx")
    (synopsis "netipx")
    (description
     "Package netipx contains code and types that were left behind when the old
inet.af/netaddr package moved to the standard library in Go 1.18 as net/netip.")
    (license license:bsd-3)))

(define-public go-go4-org-readerutil
  (package
    (name "go-go4-org-readerutil")
    (version "0.0.0-20201209231011-d4a079459e60")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/go4org/go4")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0pimk20p34lnhhwgxl7mc1s2ggv3rxrdwydv10rhg1pgp54dxmal"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "go4.org/readerutil"
       #:unpack-path "go4.org"))
    (home-page "https://go4.org")
    (synopsis "go4")
    (description #f)
    (license license:asl2.0)))

(define-public go-go-jolheiser-com-hcaptcha
  (package
    (name "go-go-jolheiser-com-hcaptcha")
    (version "0.0.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gitea.com/jolheiser/hcaptcha")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1p38amhjqh9wglzzcdggsn1dymd6alcq5mva3xal7ygd95ybffkn"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f      ; Tests require network access.
       #:import-path "go.jolheiser.com/hcaptcha"))
    (home-page "https://go.jolheiser.com/hcaptcha")
    (synopsis "hCaptcha")
    (description
      "This library was based on the hCaptcha server-side verification
@url{https://docs.hcaptcha.com/#server,docs}.")
    (license license:expat)))

(define-public go-go-jolheiser-com-pwn
  (package
    (name "go-go-jolheiser-com-pwn")
    (version "0.0.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gitea.com/jolheiser/pwn")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "10yx5pdvchf8amfmvwmpba61ixyf3gr93ry411q5r65r66ljpf8v"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "go.jolheiser.com/pwn"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-network-tests
           (lambda _
             (delete-file "src/go.jolheiser.com/pwn/password_test.go"))))))
    (home-page "https://go.jolheiser.com/pwn")
    (synopsis "Have I Been Pwned")
    (description
      "Go library for interacting with
@url{https://haveibeenpwned.com/,HaveIBeenPwned}.")
    (license license:expat)))

(define-public go-golang-org-x-exp-0.0.0-20220613132600-b0d781184e0d
  (package
    (inherit go-golang-org-x-exp)
    (name "go-golang-org-x-exp")
    (version "0.0.0-20220613132600-b0d781184e0d")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://go.googlesource.com/exp")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0jfycqykcdcp78n4kgfbn5y59hk0cpn3xfvd8if3yvy397qcp55p"))))
    ;(arguments
    ; '(#:import-path "golang.org/x/exp"))
    (propagated-inputs
     (list go-golang-org-x-xerrors
           go-golang-org-x-sys
           go-github-com-google-go-cmp
           go-golang-org-x-tools
           go-golang-org-x-mod))))

(define-public go-golang-org-x-image-0.7
  (package
    (inherit go-golang-org-x-image)
    (name "go-golang-org-x-image")
    (version "0.7.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://go.googlesource.com/image")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0zyr9ylj8lyyq55q78gdk29ikzncanmgs9z84j1585k7dsh8wagk"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "golang.org/x/image"
       #:modules ((guix build go-build-system)
                  (guix build utils)
                  (srfi srfi-1))
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key import-path build-flags #:allow-other-keys)
             (for-each
               (lambda (directory)
                 ((assoc-ref %standard-phases 'build)
                  #:build-flags build-flags
                  #:import-path (string-drop directory 4)))
               (find-files "src/golang.org/x/image"
                 (lambda (file stat)
                   (and
                     (eq? (stat:type stat) 'directory)
                     (let ((files (find-files file "\\.go$")))
                       (and
                         (not (null? files))
                         (not (string-contains file "webp-manual-test"))
                         (not (string-contains file "example/font"))
                         (not (string-contains file "font/gofont"))
                         (not (null?
                                (filter-map
                                  (lambda (test-entry)
                                    (not (string-contains test-entry file-name-separator-string)))
                                  (map (lambda (entry)
                                         (string-drop entry (1+ (string-length file))))
                                       files))))))))
                 #:directories? #t))))
         (replace 'check
           (lambda* (#:key tests? import-path #:allow-other-keys)
             (for-each
               (lambda (directory)
                 ((assoc-ref %standard-phases 'check)
                  #:tests? tests?
                  #:import-path (string-drop directory 4)))
               (find-files "src/golang.org/x/image"
                 (lambda (file stat)
                   (and
                     (eq? (stat:type stat) 'directory)
                     (let ((files (find-files file "\\.go$")))
                       (and
                         (not (null? files))
                         (not (string-contains file "webp-manual-test"))
                         (not (string-contains file "example/font"))
                         (not (string-contains file "font/gofont"))
                         (not (null?
                                (filter-map
                                  (lambda (test-entry)
                                    (not (string-contains test-entry file-name-separator-string)))
                                  (map (lambda (entry)
                                         (string-drop entry (1+ (string-length file))))
                                       files))))))))
                 #:directories? #t)))))))
    (propagated-inputs (list go-golang-org-x-text))
    (home-page "https://golang.org/x/image")
    (synopsis "Go Images")
    (description "This repository holds supplementary Go image libraries.")
    (license license:bsd-3)))

(define-public go-golang-org-x-net-0.10
  (package
    (inherit go-golang-org-x-net)
    (name "go-golang-org-x-net")
    (version "0.10.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://go.googlesource.com/net")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1m27i5hpk2bdljz6zk8p6270bk6yrn6n5rnynr71ff2rh18s4h8y"))))
    (arguments
     '(#:import-path "golang.org/x/net"
       #:modules ((guix build go-build-system)
                  (guix build utils)
                  (srfi srfi-1))
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key import-path build-flags #:allow-other-keys)
             (for-each
               (lambda (directory)
                 ((assoc-ref %standard-phases 'build)
                  #:build-flags build-flags
                  #:import-path (string-drop directory 4)))
               (find-files "src/golang.org/x/net"
                 (lambda (file stat)
                   (and
                     (eq? (stat:type stat) 'directory)
                     (let ((files (find-files file "\\.go$")))
                       (and
                         (not (null? files))
                         (not (string-contains file "net/lif"))
                         (not (string-contains file "net/route"))
                         (not (null?
                                (filter-map
                                  (lambda (test-entry)
                                    (not (string-contains test-entry file-name-separator-string)))
                                  (map (lambda (entry)
                                         (string-drop entry (1+ (string-length file))))
                                       files))))))))
                 #:directories? #t))))
         (replace 'check
           (lambda* (#:key tests? import-path #:allow-other-keys)
             (for-each
               (lambda (directory)
                 ((assoc-ref %standard-phases 'check)
                  #:tests? tests?
                  #:import-path (string-drop directory 4)))
               (find-files "src/golang.org/x/net"
                 (lambda (file stat)
                   (and
                     (eq? (stat:type stat) 'directory)
                     (let ((files (find-files file "\\.go$")))
                       (and
                         (not (null? files))
                         (not (string-contains file "net/http2/h2c"))
                         (not (string-contains file "net/lif"))
                         (not (string-contains file "net/route"))
                         ;; These tests fail with gccgo.
                         (not (string-contains file "net/bpf"))
                         (not (string-contains file "net/icmp"))
                         (not (string-contains file "net/internal/socket"))
                         (not (string-contains file "net/ipv4"))
                         (not (string-contains file "net/ipv6"))

                         (not (null?
                                (filter-map
                                  (lambda (test-entry)
                                    (not (string-contains test-entry file-name-separator-string)))
                                  (map (lambda (entry)
                                         (string-drop entry (1+ (string-length file))))
                                       files))))))))
                 #:directories? #t)))))))
    (propagated-inputs
     (list go-golang-org-x-text-0.9
           go-golang-org-x-term-0.8
           go-golang-org-x-sys))))

(define-public go-golang-org-x-oauth2-0.8
  (package
    (name "go-golang-org-x-oauth2")
    (version "0.8.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://go.googlesource.com/oauth2")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "06ja6w7iwl01lqb386dfbj5bs8j839fd8mjz934rwskyskcv3vhr"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "golang.org/x/oauth2"))
    (propagated-inputs
     (list go-google-golang-org-protobuf
           go-golang-org-x-net
           go-github-com-golang-protobuf
           go-google-golang-org-appengine
           go-github-com-google-go-cmp
           go-cloud-google-com-go-compute-metadata))
    (home-page "https://golang.org/x/oauth2")
    (synopsis "OAuth2 for Go")
    (description
     "Package oauth2 provides support for making OAuth2 authorized and authenticated
HTTP requests, as specified in @@url{https://rfc-editor.org/rfc/rfc6749.html,RFC
6749}.  It can additionally grant authorization with Bearer JWT.")
    (license license:bsd-3)))

(define-public go-golang-org-x-term-0.8
  (package
    (inherit go-golang-org-x-term)
    (name "go-golang-org-x-term")
    (version "0.8.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://go.googlesource.com/term")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1c2y6ln6yqi9rrsn2i2yp8zga8j1vp0jcxd6152r8r92bhnsrgqf"))))
    (arguments
     '(#:import-path "golang.org/x/term"))
    (propagated-inputs
     (list go-golang-org-x-sys))))

(define-public go-golang-org-x-text-0.9
  (package
    (inherit go-golang-org-x-text)
    (name "go-golang-org-x-text")
    (version "0.9.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://go.googlesource.com/text")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1krb09z7vcl99h7a1ac47n4s13mjp0x549y4s49k2ncrqddwklf7"))))
    (arguments
     '(#:import-path "golang.org/x/text"))
    (native-inputs
     (list go-golang-org-x-tools))))    ;0.6

(define go-golang-org-x-tools-bootstrap
  (package
    (inherit go-golang-org-x-tools)
    (name "go-golang-org-x-tools")
    (version "0.1.8")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://go.googlesource.com/tools")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0h9ip7sry1y8z94jypygas4ylb403wji3vljcc5jlb54rf51x3z7"))))
    ;; Keep this as source-only due to the dependencies
    ;(arguments '(#:import-path "golang.org/x/tools"))
    ;(propagated-inputs
    ; (list
    ;       go-golang-org-x-xerrors
    ;       go-golang-org-x-sys
    ;       go-golang-org-x-sync
    ;       go-golang-org-x-net
    ;       go-golang-org-x-mod
    ;       go-github-com-yuin-goldmark
    ;       ))
    (properties `((hidden? . #t)))
    ))

(define-public go-golang-org-x-tools-0.6
  (package
    (inherit go-golang-org-x-tools)
    (name "go-golang-org-x-tools")
    (version "0.6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://go.googlesource.com/tools")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12gl0k0653zgsims5rgqm9pcgw76yxldkng9987c0k3ysx75z5sf"))))
    ;(arguments
    ; '(#:import-path "golang.org/x/tools"))
    (propagated-inputs
     (list go-golang-org-x-sync
           go-golang-org-x-sys
           go-golang-org-x-net-0.10
           go-golang-org-x-mod
           go-github-com-yuin-goldmark))))

(define-public go-google-golang-org-api
  (package
    (name "go-google-golang-org-api")
    (version "0.63.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/googleapis/google-api-go-client")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1ws1lr584mgff63r9rs4g5g963vvxvds861grlqlc076aknksqs8"))))
    (build-system go-build-system)
    (arguments '(#:import-path "google.golang.org/api"))
    (propagated-inputs
      `(;("go-google-golang-org-grpc" ,go-google-golang-org-grpc)
        ;("go-google-golang-org-genproto" ,go-google-golang-org-genproto)
        ;("go-google-golang-org-appengine" ,go-google-golang-org-appengine)
        ;("go-golang-org-x-sys" ,go-golang-org-x-sys)
        ;("go-golang-org-x-sync" ,go-golang-org-x-sync)
        ;("go-golang-org-x-oauth2" ,go-golang-org-x-oauth2)
        ;("go-golang-org-x-net" ,go-golang-org-x-net)
        ;("go-go-opencensus-io" ,go-go-opencensus-io)
        ;("go-github-com-googleapis-gax-go-v2" ,go-github-com-googleapis-gax-go-v2)
        ;("go-github-com-google-go-cmp" ,go-github-com-google-go-cmp)
        ;("go-cloud-google-com-go" ,go-cloud-google-com-go)
        ))
    (home-page "https://google.golang.org/api")
    (synopsis "Google APIs Client Library for Go")
    (description
      "Package api is the root of the packages used to access Google Cloud Services.
See
@url{https://godoc.org/google.golang.org/api,https://godoc.org/google.golang.org/api}
for a full list of sub-packages.")
    (license license:bsd-3)))

(define-public go-google-golang-org-appengine-internal
  (package
    (name "go-google-golang-org-appengine-internal")
    (version "2.0.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/golang/appengine")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "00g6nqiiy19djymkixv37f9k3yq87h7z7h840dvvbdrs26n5rxiq"))))
    (build-system go-build-system)
    (arguments
     (list
       #:unpack-path "google.golang.org/appengine"
       #:import-path "google.golang.org/appengine/internal"
       #:phases
       #~(modify-phases %standard-phases
           (replace 'check
             (lambda* (#:key inputs #:allow-other-keys #:rest args)
               (unless
                 ;; The tests try to connect to the network when run with gccgo.
                 (false-if-exception (search-input-file inputs "/bin/gccgo"))
                 (apply (assoc-ref %standard-phases 'check) args)))))))
    (propagated-inputs
     (list go-github-com-golang-protobuf
           go-golang-org-x-net))
    (home-page "https://google.golang.org/appengine")
    (synopsis "Go App Engine packages")
    (description #f)
    (license license:asl2.0)))

(define-public go-google-golang-org-appengine-urlfetch
  (package
    (inherit go-google-golang-org-appengine-internal)
    (name "go-google-golang-org-appengine-urlfetch")
    (arguments
     '(#:unpack-path "google.golang.org/appengine"
       #:import-path "google.golang.org/appengine/urlfetch"))
    (propagated-inputs
     `(("go-github-com-golang-protobuf" ,go-github-com-golang-protobuf)
       ("go-golang-org-x-net" ,go-golang-org-x-net)))
    (synopsis "Go App Engine packages")
    (description #f)))

(define-public go-google-golang-org-genproto
  (package
    (name "go-google-golang-org-genproto")
    (version "0.0.0-20211223182754-3ac035c7e7cb")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/googleapis/go-genproto")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0fk3n2f6x3pyrfx2nn5ws88sp16yr42awc1n5dsfkzx47w9grczi"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "google.golang.org/genproto"
       #:phases
       (modify-phases %standard-phases
         (delete 'build)
         (delete 'check))))
    (propagated-inputs
     (list ;go-google-golang-org-protobuf
        ;go-google-golang-org-grpc
        ;go-golang-org-x-text
        ;go-golang-org-x-sys
        ;go-golang-org-x-net
        ;go-github-com-golang-protobuf
        ))
    (home-page "https://google.golang.org/genproto")
    (synopsis "Go generated proto packages")
    (description
      "This repository contains the generated Go packages for common protocol buffer
types, and the generated @url{http://grpc.io,gRPC} code necessary for
interacting with Google's gRPC APIs.")
    (license license:asl2.0)))

(define-public go-google-golang-org-grpc
  (package
    (name "go-google-golang-org-grpc")
    (version "1.43.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/grpc/grpc-go")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "10f9363yir4l7rnj5z897qk79si7913vsyzy4nw5xhxjhxsppji8"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "google.golang.org/grpc"
       #:phases
       (modify-phases %standard-phases
         (delete 'build)
         (delete 'check))))
    (propagated-inputs
     (list go-google-golang-org-protobuf
           go-google-golang-org-genproto
           go-golang-org-x-text
           go-golang-org-x-sys
           ;go-golang-org-x-oauth2
           go-golang-org-x-net
           ;go-github-com-google-uuid
           ;go-github-com-google-go-cmp
           go-github-com-golang-protobuf
           go-github-com-golang-glog
           ;go-github-com-envoyproxy-go-control-plane
           ;go-github-com-cncf-xds-go
           ;go-github-com-cncf-udpa-go
           go-github-com-cespare-xxhash
           ))
    (home-page "https://google.golang.org/grpc")
    (synopsis "gRPC-Go")
    (description "Package grpc implements an RPC system called gRPC.")
    (license license:asl2.0)))

(define-public go-google-golang-org-grpc-cmd-protoc-gen-go-grpc
  (package
    (name "go-google-golang-org-grpc-cmd-protoc-gen-go-grpc")
    (version "1.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/grpc/grpc-go")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1f3zr5a49pdy531aznjapslf8z821wdnk9xrdq564jvva0i8k7m4"))))
    (build-system go-build-system)
    (arguments
      '(#:import-path "google.golang.org/grpc/cmd/protoc-gen-go-grpc"))
    (propagated-inputs
     (list go-google-golang-org-protobuf))
    (home-page "https://google.golang.org/grpc/cmd/protoc-gen-go-grpc")
    (synopsis "protoc-gen-go-grpc")
    (description
      "protoc-gen-go-grpc is a plugin for the Google protocol buffer compiler to
generate Go code.  Install it by building this program and making it accessible
within your PATH with the name:")
    (license license:asl2.0)))

(define-public go-go-opencensus-io
  (package
    (name "go-go-opencensus-io")
    (version "0.23.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/census-instrumentation/opencensus-go")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0gw4f7inf8y2ik00yfb36xganiq9rl4w2d1a41bsjqsh83ajz2km"))))
    (build-system go-build-system)
    (arguments '(#:import-path "go.opencensus.io"))
    (propagated-inputs
     (list ;go-google-golang-org-grpc
        ;go-golang-org-x-net
        ;go-github-com-stretchr-testify
        ;go-github-com-google-go-cmp
        ;go-github-com-golang-protobuf
        ;go-github-com-golang-groupcache
        ))
    (home-page "https://go.opencensus.io")
    (synopsis "OpenCensus Libraries for Go")
    (description "Package opencensus contains Go support for OpenCensus.")
    (license license:asl2.0)))

(define-public go-go-opentelemetry-io-proto-otlp
  (package
    (name "go-go-opentelemetry-io-proto-otlp")
    (version "0.11.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/open-telemetry/opentelemetry-proto-go")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1kyysszlkzwrvsis4lz7gby62nf9f0hbn342cq2n89h1y4bvxzw4"))))
    (build-system go-build-system)
    (arguments '(#:import-path "go.opentelemetry.io/proto/otlp"))
    (propagated-inputs
      `(;("go-google-golang-org-protobuf" ,go-google-golang-org-protobuf)
        ;("go-google-golang-org-grpc" ,go-google-golang-org-grpc)
        ;("go-github-com-grpc-ecosystem-grpc-gateway" ,go-github-com-grpc-ecosystem-grpc-gateway)
        ;("go-github-com-golang-protobuf" ,go-github-com-golang-protobuf)
        ))
    (home-page "https://go.opentelemetry.io/proto/otlp")
    (synopsis #f)
    (description #f)
    (license license:asl2.0)))

(define-public go-gopkg-in-alexcesaro-quotedprintable-v3
  (package
    (name "go-gopkg-in-alexcesaro-quotedprintable-v3")
    (version "3.0.0-20150716171945-2caba252f4dc")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gopkg.in/alexcesaro/quotedprintable.v3")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1fi38y0f7877ra8xi6782vp2ahfghzk4apj3ca6lljjyzgahij79"))))
    (build-system go-build-system)
    (arguments
      '(#:import-path
        "gopkg.in/alexcesaro/quotedprintable.v3"
        #:unpack-path
        "gopkg.in/alexcesaro/quotedprintable.v3"))
    (home-page "https://gopkg.in/alexcesaro/quotedprintable.v3")
    (synopsis "quotedprintable")
    (description
      "Package quotedprintable implements quoted-printable encoding as specified by
@url{https://rfc-editor.org/rfc/rfc2045.html,RFC 2045}.")
    (license license:expat)))

(define-public go-gopkg-in-gavv-httpexpect-v1
  (package
    (name "go-gopkg-in-gavv-httpexpect-v1")
    (version "1.1.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gopkg.in/gavv/httpexpect.v1")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0gnq1451d588nbgwc1q6xgl4facv6f6512v898y7vfylv9p5m1kq"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "gopkg.in/gavv/httpexpect.v1"
       #:tests? #f))
    (propagated-inputs
     (list go-github-com-ajg-form
           go-github-com-fatih-structs
           go-github-com-gavv-monotime
           go-github-com-google-go-querystring
           go-github-com-imkira-go-interpol
           go-github-com-stretchr-testify
           go-github-com-valyala-fasthttp
           go-golang-org-x-net
           go-github-com-xeipuuv-gojsonschema
           go-github-com-yalp-jsonpath
           go-github-com-yudai-gojsondiff
           go-moul-io-http2curl))
    (home-page "https://gopkg.in/gavv/httpexpect.v1")
    (synopsis "httpexpect")
    (description #f)
    (license license:expat)))

(define-public go-gopkg-in-gomail-v2
  (package
    (name "go-gopkg-in-gomail-v2")
    (version "2.0.0-20160411212932-81ebce5c23df")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gopkg.in/gomail.v2")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0zdykrv5s19lnq0g49p6njldy4cpk4g161vyjafiw7f84h8r28mc"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f      ; Package is unmaintained.
       #:import-path "gopkg.in/gomail.v2"))
    (home-page "https://gopkg.in/gomail.v2")
    (synopsis "Gomail")
    (description
      "Package gomail provides a simple interface to compose emails and to mail them
efficiently.")
    (license license:expat)))

(define-public go-gopkg-in-inf-v0
  (package
    (name "go-gopkg-in-inf-v0")
    (version "0.9.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gopkg.in/inf.v0")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00k5iqjcp371fllqxncv7jkf80hn1zww92zm78cclbcn4ybigkng"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "gopkg.in/inf.v0"
       #:unpack-path "gopkg.in/inf.v0"))
    (home-page "https://gopkg.in/inf.v0")
    (synopsis #f)
    (description
     "Package inf (type inf.Dec) implements \"infinite-precision\" decimal arithmetic.
\"Infinite precision\" describes two characteristics: practically unlimited
precision for decimal number representation and no support for calculating with
any specific fixed precision. (Although there is no practical limit on
precision, inf.Dec can only represent finite decimals.)")
    (license license:bsd-3)))

(define-public go-gopkg-in-src-d-go-billy-v4
  (package
    (inherit go-github-com-go-git-go-billy-v5)
    (name "go-gopkg-in-src-d-go-billy")
    (version "4.3.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gopkg.in/src-d/go-billy.v4")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0jcyi4ink2njgflp3f2mbl5b86p2w0rh945k5xplcl158i5wkrc6"))))
    (arguments
     '(#:import-path "gopkg.in/src-d/go-billy.v4"))
    (native-inputs
     `(("go-gopkg-in-check-v1" ,go-gopkg-in-check-v1)))))

(define-public go-gopkg-in-src-d-go-git-fixtures-v3
  (package
    (name "go-gopkg-in-src-d-go-git-fixtures-v3")
    (version "3.5.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gopkg.in/src-d/go-git-fixtures.v3")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0ckvfzj9nhvxi3aznid1wbw0yq5s3k2mfhlqmaz2ll8myzr97w96"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "gopkg.in/src-d/go-git-fixtures.v3"
       #:phases
       (modify-phases %standard-phases
         (delete 'reset-gzip-timestamps))))
    (propagated-inputs
     `(;("go-github-com-alcortesm-tgz" ,go-github-com-alcortesm-tgz)
       ("go-golang-org-x-sys" ,go-golang-org-x-sys)
       ("go-gopkg-in-check-v1" ,go-gopkg-in-check-v1)
       ("go-gopkg-in-src-d-go-billy-v4" ,go-gopkg-in-src-d-go-billy-v4)
       ("go-gopkg-in-src-d-go-git-v4" ,go-gopkg-in-src-d-go-git-v4)))
    (home-page "https://gopkg.in/src-d/go-git-fixtures.v3")
    (synopsis "go-git-fixtures")
    (description #f)
    (license license:asl2.0)))

(define-public go-gopkg-in-src-d-go-git-v4
  (package
    (name "go-gopkg-in-src-d-go-git-v4")
    (version "4.13.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gopkg.in/src-d/go-git.v4")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0n4x7r69qrmpss51b3wd3vj4b9jmip4archz3fbqk6q1yyd1pxjb"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "gopkg.in/src-d/go-git.v4"
       ;; Circular dependencies with test dependencies.
       #:tests? #f))
    (propagated-inputs
     `(("go-github-com-emirpasic-gods" ,go-github-com-emirpasic-gods)
       ("go-github-com-jbenet-go-context" ,go-github-com-jbenet-go-context)
       ("go-github-com-kevinburke-ssh-config" ,go-github-com-kevinburke-ssh-config)
       ("go-github-com-mitchellh-go-homedir" ,go-github-com-mitchellh-go-homedir)
       ("go-github-com-sergi-go-diff" ,go-github-com-sergi-go-diff)
       ("go-github-com-src-d-gcfg" ,go-github-com-src-d-gcfg)
       ("go-github-com-xanzy-ssh-agent" ,go-github-com-xanzy-ssh-agent)
       ("go-golang-org-x-crypto" ,go-golang-org-x-crypto)
       ("go-golang-org-x-net" ,go-golang-org-x-net)
       ("go-gopkg-in-src-d-go-billy-v4" ,go-gopkg-in-src-d-go-billy-v4)))
    (native-inputs
     `(("go-golang-org-x-text" ,go-golang-org-x-text)
       ("go-gopkg-in-check-v1" ,go-gopkg-in-check-v1)
       ;; Creates circular dependency chain
       ;("go-gopkg-in-src-d-go-git-fixtures-v3" ,go-gopkg-in-src-d-go-git-fixtures-v3)
       ))
    (home-page "https://gopkg.in/src-d/go-git.v4")
    (synopsis #f)
    (description #f)
    (license license:asl2.0)))

(define-public go-gopkg-in-yaml-v1
  (package
    (name "go-gopkg-in-yaml-v1")
    (version "1.0.0-20140924161607-9f9df34309c0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://gopkg.in/yaml.v1")
                     (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1r8d346szqa9x8q03wiycik5qy3d6w8qq4hs99z1p64q5lm0g7gm"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f      ; TODO?
       #:import-path "gopkg.in/yaml.v1"
       #:unpack-path "gopkg.in/yaml.v1"))
    (native-inputs
     (list go-gopkg-in-check-v1))
    (home-page "https://gopkg.in/yaml.v1")
    (synopsis "YAML support for the Go language")
    (description "Package yaml implements YAML support for the Go language.")
    (license license:lgpl3)))

(define-public go-gotest-tools
  (package
    (name "go-gotest-tools")
    (version "1.4.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/gotestyourself/gotest.tools")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0ykgj2rpi3yha9rd23abx2885rm72jarhpgw1hkasmrb9i7j6nqk"))))
    (build-system go-build-system)
    (arguments '(#:import-path "gotest.tools"))
    (home-page "https://gotest.tools")
    (synopsis "gotest.tools")
    (description
      "Package gotesttools is a collection of packages to augment `testing` and support
common patterns.")
    (license license:asl2.0)))


(define-public go-gvisor-dev-gvisor
  (package
    (name "go-gvisor-dev-gvisor")
    (version "0.0.0-20230504175454-7b0a1988a28f")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/google/gvisor")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0k2iklm6hrjl93v0aggl6rgvc5bjvcklsjrqzxj4l190ywycrcak"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "gvisor.dev/gvisor"
       #:phases
       (modify-phases %standard-phases
         (delete 'build)
         (delete 'check))
       ))
    (propagated-inputs
     `(("go-sigs-k8s-io-yaml" ,go-sigs-k8s-io-yaml)
       ("go-sigs-k8s-io-structured-merge-diff-v4" ,go-sigs-k8s-io-structured-merge-diff-v4)
       ("go-sigs-k8s-io-json" ,go-sigs-k8s-io-json)
       ("go-k8s-io-utils" ,go-k8s-io-utils)
       ("go-k8s-io-kube-openapi" ,go-k8s-io-kube-openapi)
       ("go-k8s-io-klog-v2" ,go-k8s-io-klog-v2)
       ("go-honnef-co-go-tools" ,go-honnef-co-go-tools)
       ;("go-gotest-tools-v3" ,go-gotest-tools-v3)
       ("go-gopkg-in-yaml-v3" ,go-gopkg-in-yaml-v3)
       ("go-gopkg-in-yaml-v2" ,go-gopkg-in-yaml-v2)
       ("go-gopkg-in-tomb-v1" ,go-gopkg-in-tomb-v1)
       ("go-gopkg-in-inf-v0" ,go-gopkg-in-inf-v0)
       ("go-google-golang-org-grpc" ,go-google-golang-org-grpc)
       ("go-google-golang-org-genproto" ,go-google-golang-org-genproto)
       ("go-google-golang-org-appengine" ,go-google-golang-org-appengine)
       ("go-golang-org-x-xerrors" ,go-golang-org-x-xerrors)
       ("go-golang-org-x-text" ,go-golang-org-x-text)
       ("go-golang-org-x-term" ,go-golang-org-x-term)
       ("go-golang-org-x-oauth2" ,go-golang-org-x-oauth2)
       ("go-golang-org-x-net" ,go-golang-org-x-net)
       ("go-go-opencensus-io" ,go-go-opencensus-io)
       ("go-github-com-vishvananda-netns" ,go-github-com-vishvananda-netns)
       ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
       ;("go-github-com-opencontainers-go-digest" ,go-github-com-opencontainers-go-digest)
       ("go-github-com-modern-go-reflect2" ,go-github-com-modern-go-reflect2)
       ("go-github-com-modern-go-concurrent" ,go-github-com-modern-go-concurrent)
       ("go-github-com-json-iterator-go" ,go-github-com-json-iterator-go)
       ("go-github-com-hashicorp-go-multierror" ,go-github-com-hashicorp-go-multierror)
       ("go-github-com-hashicorp-errwrap" ,go-github-com-hashicorp-errwrap)
       ;("go-github-com-googleapis-gnostic" ,go-github-com-googleapis-gnostic)
       ("go-github-com-google-gofuzz" ,go-github-com-google-gofuzz)
       ("go-github-com-google-go-cmp" ,go-github-com-google-go-cmp)
       ("go-github-com-golang-protobuf" ,go-github-com-golang-protobuf)
       ("go-github-com-golang-groupcache" ,go-github-com-golang-groupcache)
       ("go-github-com-go-logr-logr" ,go-github-com-go-logr-logr)
       ("go-github-com-docker-go-units" ,go-github-com-docker-go-units)
       ("go-github-com-davecgh-go-spew" ,go-github-com-davecgh-go-spew)
       ;("go-github-com-containerd-ttrpc" ,go-github-com-containerd-ttrpc)
       ;("go-github-com-containerd-continuity" ,go-github-com-containerd-continuity)
       ;("go-github-com-microsoft-hcsshim" ,go-github-com-microsoft-hcsshim)
       ("go-github-com-microsoft-go-winio" ,go-github-com-microsoft-go-winio)
       ("go-k8s-io-client-go" ,go-k8s-io-client-go)
       ("go-k8s-io-apimachinery" ,go-k8s-io-apimachinery)
       ("go-k8s-io-api" ,go-k8s-io-api)
       ("go-google-golang-org-protobuf" ,go-google-golang-org-protobuf)
       ("go-golang-org-x-tools" ,go-golang-org-x-tools)
       ("go-golang-org-x-time" ,go-golang-org-x-time)
       ("go-golang-org-x-sys" ,go-golang-org-x-sys)
       ("go-golang-org-x-sync" ,go-golang-org-x-sync)
       ("go-golang-org-x-mod" ,go-golang-org-x-mod)
       ("go-github-com-vishvananda-netlink" ,go-github-com-vishvananda-netlink)
       ;("go-github-com-syndtr-gocapability" ,go-github-com-syndtr-gocapability)
       ("go-github-com-sirupsen-logrus" ,go-github-com-sirupsen-logrus)
       ("go-github-com-opencontainers-runtime-spec" ,go-github-com-opencontainers-runtime-spec)
       ("go-github-com-mohae-deepcopy" ,go-github-com-mohae-deepcopy)
       ("go-github-com-mattbaird-jsonpatch" ,go-github-com-mattbaird-jsonpatch)
       ;("go-github-com-kr-pty" ,go-github-com-kr-pty)
       ("go-github-com-google-subcommands" ,go-github-com-google-subcommands)
       ("go-github-com-google-btree" ,go-github-com-google-btree)
       ("go-github-com-gogo-protobuf" ,go-github-com-gogo-protobuf)
       ("go-github-com-gofrs-flock" ,go-github-com-gofrs-flock)
       ("go-github-com-godbus-dbus-v5" ,go-github-com-godbus-dbus-v5)
       ("go-github-com-coreos-go-systemd-v22" ,go-github-com-coreos-go-systemd-v22)
       ("go-github-com-containerd-typeurl" ,go-github-com-containerd-typeurl)
       ;("go-github-com-containerd-go-runc" ,go-github-com-containerd-go-runc)
       ("go-github-com-containerd-fifo" ,go-github-com-containerd-fifo)
       ;("go-github-com-containerd-containerd" ,go-github-com-containerd-containerd)
       ("go-github-com-containerd-console" ,go-github-com-containerd-console)
       ("go-github-com-containerd-cgroups" ,go-github-com-containerd-cgroups)
       ("go-github-com-cilium-ebpf" ,go-github-com-cilium-ebpf)
       ;("go-github-com-cenkalti-backoff" ,go-github-com-cenkalti-backoff)
       ;("go-github-com-bazelbuild-rules-go" ,go-github-com-bazelbuild-rules-go)
       ("go-github-com-burntsushi-toml" ,go-github-com-burntsushi-toml)))
    (home-page "https://gvisor.dev/gvisor")
    (synopsis "What is gVisor?")
    (description
     "@@strong{gVisor} is an application kernel, written in Go, that implements a
substantial portion of the Linux system surface.  It includes an
@@url{https://www.opencontainers.org,Open Container Initiative (OCI)} runtime
called @@code{runsc} that provides an isolation boundary between the application
and the host kernel.  The @@code{runsc} runtime integrates with Docker and
Kubernetes, making it simple to run sandboxed containers.")
    (license license:asl2.0)))

(define-public go-howett-net-plist
  (package
    (name "go-howett-net-plist")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/DHowett/go-plist")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0rh8z67y527czv25ljmzqpmr0qsmn8cbaal7pw97c49y75kaj95k"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "howett.net/plist"))
    (propagated-inputs
     `(("go-gopkg-in-yaml-v1" ,go-gopkg-in-yaml-v1)
       ("go-gopkg-in-check-v1" ,go-gopkg-in-check-v1)
       ("go-github-com-jessevdk-go-flags" ,go-github-com-jessevdk-go-flags)))
    (home-page "https://howett.net/plist")
    (synopsis "plist - A pure Go property list transcoder")
    (description
     "Package plist implements encoding and decoding of Apple's \"property
list\" format.  Property lists come in three sorts: plain text (GNUStep and
OpenStep), XML and binary.  plist supports all of them.  The mapping between
property list and Go objects is described in the documentation for the Marshal
and Unmarshal functions.")
      (license (list license:bsd-2 license:bsd-3))))

(define-public go-humungus-tedunangst-com-r-go-sqlite3
  (package
    (name "go-humungus-tedunangst-com-r-go-sqlite3")
    (version "1.1.3")
    (source (origin
              (method hg-fetch)
              (uri (hg-reference (url
                                  "https://humungus.tedunangst.com/r/go-sqlite3")
                                 (changeset (string-append "v" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1xkx0ijljricbqyf98dgqcc2lx65a1h19ab8rx7vrimhyp7dw5c6"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "humungus.tedunangst.com/r/go-sqlite3"))
    (inputs
     (list sqlite))
    (home-page "https://humungus.tedunangst.com/r/go-sqlite3")
    (synopsis "go-sqlite3")
    (description "Package sqlite3 provides interface to SQLite3 databases.")
    (license license:expat)))

(define-public go-humungus-tedunangst-com-r-webs
  (package
    (name "go-humungus-tedunangst-com-r-webs")
    (version "0.7.9")
    (source
     (origin
       (method hg-fetch)
       (uri (hg-reference (url "https://humungus.tedunangst.com/r/webs")
                          (changeset (string-append "v" version))))
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32 "1xhmb7d3p201ps4bfcy5czgjzlv8ngnqf7aismcpvgik01ff36kl"))))
    (build-system go-build-system)
    (arguments
     (list
       #:import-path "humungus.tedunangst.com/r/webs"
       #:unpack-path "humungus.tedunangst.com/r/webs"
       #:phases
       #~(modify-phases %standard-phases
           (replace 'build
             (lambda* (#:key import-path build-flags #:allow-other-keys)
               (for-each
                 (lambda (directory)
                   ((assoc-ref %standard-phases 'build)
                    #:build-flags build-flags
                    #:import-path
                    (string-append "humungus.tedunangst.com/r/webs/" directory)))
                 (list "cache"
                       "gate"
                       ;"gencache"      ; This one fails with gccgo
                       "htfilter"
                       "httpsig"
                       "image"
                       "junk"
                       "log"
                       "login"
                       "mz"
                       "rss"
                       "synlight"
                       "templates"))))
           (replace 'check
             (lambda* (#:key tests? import-path #:allow-other-keys)
               (for-each
                 (lambda (directory)
                   ((assoc-ref %standard-phases 'check)
                    #:tests? tests?
                    #:import-path
                    (string-append "humungus.tedunangst.com/r/webs/" directory)))
                 (list "cache"
                       "gate"
                       ;"gencache"      ; This one fails with gccgo
                       "htfilter"
                       "httpsig"
                       "image"
                       "junk"
                       "log"
                       "login"
                       "mz"
                       "rss"
                       "synlight"
                       "templates")))))))
    (propagated-inputs
     (list go-golang-org-x-net          ; 0.14.0
           go-golang-org-x-image        ; 0.11.0
           go-golang-org-x-crypto))     ; 0.12.0
    (home-page "https://humungus.tedunangst.com/r/webs")
    (synopsis "Web utilities")
    (description "This package contains a collection of web utilities.")
    (license license:isc)))

(define-public go-inet-af-peercred
  (package
    (name "go-inet-af-peercred")
    (version "0.0.0-20210906144145-0893ea02156a")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/inetaf/peercred")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12jf9xkqp0h3crhw7brxhdcqik9vfj4lxb46xdbim9akyzs2qk3z"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f      ; Tests require network access
       #:import-path "inet.af/peercred"))
    (propagated-inputs `(("go-golang-org-x-sys" ,go-golang-org-x-sys)))
    (home-page "https://inet.af/peercred")
    (synopsis "peercred")
    (description
     "Package peercred maps from a net.Conn to information about the other side of the
connection, using various OS-specific facilities.")
    (license license:bsd-3)))

(define-public go-inet-af-wf
  (package
    (name "go-inet-af-wf")
    (version "0.0.0-20220728202103-50d96caab2f6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/inetaf/wf")
                    (commit (go-version->git-ref version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1c68h2a37hzp2smx0b2sbg389fpsgnm1xkcf3x1agl1lh28c90cn"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "inet.af/wf"))
    (propagated-inputs `(("go-golang-org-x-xerrors" ,go-golang-org-x-xerrors)
                         ("go-golang-org-x-tools" ,go-golang-org-x-tools)
                         ("go-golang-org-x-mod" ,go-golang-org-x-mod)
                         ("go-golang-org-x-exp" ,go-golang-org-x-exp)
                         ("go-github-com-burntsushi-toml" ,go-github-com-burntsushi-toml)
                         ("go-honnef-co-go-tools" ,go-honnef-co-go-tools)
                         ("go-golang-org-x-sys" ,go-golang-org-x-sys)
                         ("go-go4-org-netipx" ,go-go4-org-netipx)
                         ("go-github-com-peterbourgon-ff-v3" ,go-github-com-peterbourgon-ff-v3)
                         ("go-github-com-google-go-cmp" ,go-github-com-google-go-cmp)))
    (home-page "https://inet.af/wf")
    (synopsis "wf")
    (description
     "This is a package for controlling the Windows Filtering Platform (WFP), also
known as the Windows firewall.")
    (license license:bsd-3)))

(define-public go-k8s-io-api
  (package
    (name "go-k8s-io-api")
    (version "0.26.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/kubernetes/api")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0kcx7x87235k8pd6mc95d83qk7ip5a82gckddfb8jwq6ryf3qr7p"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "k8s.io/api"))
    (propagated-inputs
     `(("go-k8s-io-apimachinery" ,go-k8s-io-apimachinery)
       ("go-sigs-k8s-io-yaml" ,go-sigs-k8s-io-yaml)
       ("go-sigs-k8s-io-structured-merge-diff-v4" ,go-sigs-k8s-io-structured-merge-diff-v4)
       ("go-sigs-k8s-io-json" ,go-sigs-k8s-io-json)
       ("go-k8s-io-utils" ,go-k8s-io-utils)
       ("go-k8s-io-klog-v2" ,go-k8s-io-klog-v2)
       ("go-gopkg-in-yaml-v3" ,go-gopkg-in-yaml-v3)
       ("go-gopkg-in-yaml-v2" ,go-gopkg-in-yaml-v2)
       ("go-gopkg-in-inf-v0" ,go-gopkg-in-inf-v0)
       ("go-google-golang-org-protobuf" ,go-google-golang-org-protobuf)
       ("go-golang-org-x-text" ,go-golang-org-x-text)
       ("go-golang-org-x-net" ,go-golang-org-x-net)
       ("go-github-com-spf13-pflag" ,go-github-com-spf13-pflag)
       ("go-github-com-pmezard-go-difflib" ,go-github-com-pmezard-go-difflib)
       ("go-github-com-modern-go-reflect2" ,go-github-com-modern-go-reflect2)
       ("go-github-com-modern-go-concurrent" ,go-github-com-modern-go-concurrent)
       ("go-github-com-json-iterator-go" ,go-github-com-json-iterator-go)
       ("go-github-com-google-gofuzz" ,go-github-com-google-gofuzz)
       ("go-github-com-google-go-cmp" ,go-github-com-google-go-cmp)
       ("go-github-com-golang-protobuf" ,go-github-com-golang-protobuf)
       ("go-github-com-go-logr-logr" ,go-github-com-go-logr-logr)
       ("go-github-com-davecgh-go-spew" ,go-github-com-davecgh-go-spew)
       ("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)
       ("go-github-com-gogo-protobuf" ,go-github-com-gogo-protobuf)))
    (home-page "https://k8s.io/api")
    (synopsis "api")
    (description
     "Schema of the external API types that are served by the Kubernetes API server.")
    (license license:asl2.0)))

(define-public go-k8s-io-apimachinery
  (package
    (name "go-k8s-io-apimachinery")
    (version "0.26.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/kubernetes/apimachinery")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0z8aj1q7gs86yh2hmjyw6xq78mc6j5bqmdzflb3fffbp9dw4dvxy"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "k8s.io/apimachinery"))
    (propagated-inputs
     `(("go-gopkg-in-yaml-v3" ,go-gopkg-in-yaml-v3)
       ("go-gopkg-in-yaml-v2" ,go-gopkg-in-yaml-v2)
       ("go-gopkg-in-check-v1" ,go-gopkg-in-check-v1)
       ("go-google-golang-org-protobuf" ,go-google-golang-org-protobuf)
       ("go-golang-org-x-text" ,go-golang-org-x-text)
       ("go-github-com-pmezard-go-difflib" ,go-github-com-pmezard-go-difflib)
       ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
       ("go-github-com-onsi-gomega" ,go-github-com-onsi-gomega)
       ("go-github-com-onsi-ginkgo-v2" ,go-github-com-onsi-ginkgo-v2)
       ;("go-github-com-niemeyer-pretty" ,go-github-com-niemeyer-pretty)
       ("go-github-com-modern-go-reflect2" ,go-github-com-modern-go-reflect2)
       ("go-github-com-modern-go-concurrent" ,go-github-com-modern-go-concurrent)
       ("go-github-com-kr-text" ,go-github-com-kr-text)
       ("go-github-com-json-iterator-go" ,go-github-com-json-iterator-go)
       ("go-github-com-go-logr-logr" ,go-github-com-go-logr-logr)
       ("go-sigs-k8s-io-yaml" ,go-sigs-k8s-io-yaml)
       ("go-sigs-k8s-io-structured-merge-diff-v4" ,go-sigs-k8s-io-structured-merge-diff-v4)
       ("go-sigs-k8s-io-json" ,go-sigs-k8s-io-json)
       ("go-k8s-io-utils" ,go-k8s-io-utils)
       ("go-k8s-io-kube-openapi" ,go-k8s-io-kube-openapi)
       ("go-k8s-io-klog-v2" ,go-k8s-io-klog-v2)
       ("go-gopkg-in-inf-v0" ,go-gopkg-in-inf-v0)
       ("go-golang-org-x-net" ,go-golang-org-x-net)
       ("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)
       ("go-github-com-spf13-pflag" ,go-github-com-spf13-pflag)
       ;("go-github-com-mxk-go-flowrate" ,go-github-com-mxk-go-flowrate)
       ;("go-github-com-moby-spdystream" ,go-github-com-moby-spdystream)
       ("go-github-com-google-uuid" ,go-github-com-google-uuid)
       ("go-github-com-google-gofuzz" ,go-github-com-google-gofuzz)
       ("go-github-com-google-go-cmp" ,go-github-com-google-go-cmp)
       ("go-github-com-google-gnostic" ,go-github-com-google-gnostic)
       ("go-github-com-golang-protobuf" ,go-github-com-golang-protobuf)
       ("go-github-com-gogo-protobuf" ,go-github-com-gogo-protobuf)
       ("go-github-com-evanphx-json-patch" ,go-github-com-evanphx-json-patch-v4)
       ;("go-github-com-elazarl-goproxy" ,go-github-com-elazarl-goproxy)
       ("go-github-com-davecgh-go-spew" ,go-github-com-davecgh-go-spew)
       ("go-github-com-armon-go-socks5" ,go-github-com-armon-go-socks5)))
    (home-page "https://k8s.io/apimachinery")
    (synopsis "apimachinery")
    (description
     "Scheme, typing, encoding, decoding, and conversion packages for Kubernetes and
Kubernetes-like API objects.")
    (license license:asl2.0)))

(define-public go-k8s-io-client-go
  (package
    (name "go-k8s-io-client-go")
    (version "0.26.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/kubernetes/client-go")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1j26wg8rrpxdj7r1v40xjw3y8mwpgh182ws4cz789a81h1hiqwyv"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "k8s.io/client-go"))
    (propagated-inputs
     `(("go-k8s-io-apimachinery" ,go-k8s-io-apimachinery)
       ("go-k8s-io-api" ,go-k8s-io-api)
       ("go-sigs-k8s-io-json" ,go-sigs-k8s-io-json)
       ("go-gopkg-in-yaml-v3" ,go-gopkg-in-yaml-v3)
       ("go-gopkg-in-yaml-v2" ,go-gopkg-in-yaml-v2)
       ("go-gopkg-in-inf-v0" ,go-gopkg-in-inf-v0)
       ("go-google-golang-org-appengine" ,go-google-golang-org-appengine)
       ("go-golang-org-x-text" ,go-golang-org-x-text)
       ("go-golang-org-x-sys" ,go-golang-org-x-sys)
       ("go-github-com-pmezard-go-difflib" ,go-github-com-pmezard-go-difflib)
       ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
       ("go-github-com-munnerz-goautoneg" ,go-github-com-munnerz-goautoneg)
       ("go-github-com-modern-go-reflect2" ,go-github-com-modern-go-reflect2)
       ("go-github-com-modern-go-concurrent" ,go-github-com-modern-go-concurrent)
       ;("go-github-com-moby-spdystream" ,go-github-com-moby-spdystream)
       ("go-github-com-mailru-easyjson" ,go-github-com-mailru-easyjson)
       ("go-github-com-json-iterator-go" ,go-github-com-json-iterator-go)
       ("go-github-com-josharian-intern" ,go-github-com-josharian-intern)
       ("go-github-com-google-btree" ,go-github-com-google-btree)
       ("go-github-com-go-openapi-swag" ,go-github-com-go-openapi-swag)
       ("go-github-com-go-openapi-jsonreference" ,go-github-com-go-openapi-jsonreference)
       ("go-github-com-go-openapi-jsonpointer" ,go-github-com-go-openapi-jsonpointer)
       ("go-github-com-go-logr-logr" ,go-github-com-go-logr-logr)
       ("go-github-com-emicklei-go-restful-v3" ,go-github-com-emicklei-go-restful-v3)
       ("go-sigs-k8s-io-yaml" ,go-sigs-k8s-io-yaml)
       ("go-sigs-k8s-io-structured-merge-diff-v4" ,go-sigs-k8s-io-structured-merge-diff-v4)
       ("go-k8s-io-utils" ,go-k8s-io-utils)
       ("go-k8s-io-kube-openapi" ,go-k8s-io-kube-openapi)
       ("go-k8s-io-klog-v2" ,go-k8s-io-klog-v2)
       ("go-google-golang-org-protobuf" ,go-google-golang-org-protobuf)
       ("go-golang-org-x-time" ,go-golang-org-x-time)
       ("go-golang-org-x-term" ,go-golang-org-x-term)
       ("go-golang-org-x-oauth2" ,go-golang-org-x-oauth2)
       ("go-golang-org-x-net" ,go-golang-org-x-net)
       ("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)
       ("go-github-com-spf13-pflag" ,go-github-com-spf13-pflag)
       ("go-github-com-peterbourgon-diskv" ,go-github-com-peterbourgon-diskv)
       ;("go-github-com-imdario-mergo" ,go-github-com-imdario-mergo)
       ;("go-github-com-gregjones-httpcache" ,go-github-com-gregjones-httpcache)
       ("go-github-com-google-uuid" ,go-github-com-google-uuid)
       ("go-github-com-google-gofuzz" ,go-github-com-google-gofuzz)
       ("go-github-com-google-go-cmp" ,go-github-com-google-go-cmp)
       ("go-github-com-google-gnostic" ,go-github-com-google-gnostic)
       ("go-github-com-golang-protobuf" ,go-github-com-golang-protobuf)
       ("go-github-com-golang-groupcache" ,go-github-com-golang-groupcache)
       ("go-github-com-gogo-protobuf" ,go-github-com-gogo-protobuf)
       ("go-github-com-evanphx-json-patch" ,go-github-com-evanphx-json-patch-v4)
       ("go-github-com-davecgh-go-spew" ,go-github-com-davecgh-go-spew)))
    (home-page "https://k8s.io/client-go")
    (synopsis "client-go")
    (description
     "Go clients for talking to a @@url{http://kubernetes.io/,kubernetes} cluster.")
    (license license:asl2.0)))

(define-public go-modernc-org-cc-v3
  (package
    (name "go-modernc-org-cc-v3")
    (version "3.40.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gitlab.com/cznic/cc")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1dvil3zi71bsizl67wicg612glygpdwpjk6ab4hiiqlxxxjd8yi4"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "modernc.org/cc/v3"
       #:unpack-path "modernc.org/cc"))
    (propagated-inputs
     (list go-modernc-org-token
           go-modernc-org-strutil
           go-modernc-org-mathutil
           go-lukechampine-com-uint128
           go-github-com-google-go-cmp
           go-github-com-dustin-go-humanize))
    (home-page "https://modernc.org/cc/v3")
    (synopsis "cc/v3")
    (description "Package cc is a C99 compiler front end (Work in progress).")
    (license license:bsd-3)))

(define-public go-modernc-org-ccgo-v3
  (package
    (name "go-modernc-org-ccgo-v3")
    (version "3.16.13")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gitlab.com/cznic/ccgo")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1kxnrs4abxc3fm1637kz01xjxz7rij5gyy0lc3178j54hwwwr20n"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "modernc.org/ccgo/v3"
       #:unpack-path "modernc.org/ccgo"))
    (propagated-inputs
     (list go-modernc-org-token
           go-modernc-org-strutil
           go-modernc-org-memory
           go-modernc-org-httpfs
           go-lukechampine-com-uint128
           go-golang-org-x-xerrors
           go-golang-org-x-mod
           go-github-com-remyoudompheng-bigfft
           go-github-com-mattn-go-isatty
           go-github-com-google-uuid
           go-modernc-org-opt
           go-modernc-org-mathutil
           go-modernc-org-libc
           go-modernc-org-ccorpus
           go-modernc-org-cc-v3
           go-golang-org-x-tools
           go-golang-org-x-sys
           go-github-com-pmezard-go-difflib
           go-github-com-kballard-go-shellquote
           go-github-com-dustin-go-humanize))
    (home-page "https://modernc.org/ccgo/v3")
    (synopsis "ccgo/v3")
    (description "Command ccgo is a C compiler producing Go code.")
    (license license:bsd-3)))

(define-public go-modernc-org-ccorpus
  (package
    (name "go-modernc-org-ccorpus")
    (version "1.11.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gitlab.com/cznic/ccorpus")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1idj0w0ph5p3ynniw0cvd8j74k3h1qqmz0vfa7w1qmlsvc9jr1hc"))))
    (build-system go-build-system)
    (arguments '(#:import-path "modernc.org/ccorpus"))
    (propagated-inputs
     (list go-modernc-org-httpfs))
    (home-page "https://modernc.org/ccorpus")
    (synopsis "ccorpus")
    (description "Package ccorpus provides a test corpus of C code.")
    (license license:bsd-3)))

(define-public go-modernc-org-libc
  (package
    (name "go-modernc-org-libc")
    (version "1.22.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/cznic/libc")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0w1zb7swn12dnmf07xz57vqhir3bbqpp7jv194ikhdil6016rji0"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "modernc.org/libc"))
    (propagated-inputs
     (list go-modernc-org-memory
           go-modernc-org-mathutil
           go-golang-org-x-sys
           go-github-com-mattn-go-isatty
           go-github-com-google-uuid
           go-github-com-dustin-go-humanize))
    (home-page "https://modernc.org/libc")
    (synopsis "libc")
    (description
     "Package libc provides run time support for ccgo generated programs and
implements selected parts of the C standard library.")
    (license license:bsd-3)))

(define-public go-modernc-org-sqlite
  (package
    (name "go-modernc-org-sqlite")
    (version "1.22.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/cznic/sqlite")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0rl01gmbfp5fnjiflwq85qdiy78fa1myxkm0d9vvrrgd1jbx3k03"))))
    (build-system go-build-system)
    (arguments
     `(#:tests? #f      ; Tests hang forever
       #:import-path "modernc.org/sqlite"))
    (propagated-inputs
     (list go-modernc-org-tcl
           go-modernc-org-mathutil
           go-modernc-org-libc
           go-modernc-org-ccgo-v3
           go-golang-org-x-sys
           go-github-com-mattn-go-sqlite3
           go-github-com-klauspost-cpuid-v2
           go-github-com-google-pprof))
    (home-page "https://modernc.org/sqlite")
    (synopsis "sqlite")
    (description
     "Package sqlite is a sql/database driver using a CGo-free port of the C SQLite3
library.")
    (license license:bsd-3)))

(define-public go-modernc-org-tcl
  (package
    (name "go-modernc-org-tcl")
    (version "1.15.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gitlab.com/cznic/tcl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0ql3njgjr6svc93vigaqh968wc24sq5xvi4lp9q102j9xwi6dmcz"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f                      ; Tests hang
       #:import-path "modernc.org/tcl"))
    (propagated-inputs
     (list go-modernc-org-token
           go-modernc-org-strutil
           go-modernc-org-opt
           go-modernc-org-memory
           go-modernc-org-cc-v3
           go-lukechampine-com-uint128
           go-golang-org-x-xerrors
           go-golang-org-x-tools
           go-golang-org-x-sys
           go-golang-org-x-mod
           go-github-com-remyoudompheng-bigfft
           go-github-com-mattn-go-isatty
           go-github-com-kballard-go-shellquote
           go-github-com-google-uuid
           go-modernc-org-z
           go-modernc-org-mathutil
           go-modernc-org-libc
           go-modernc-org-httpfs
           go-modernc-org-ccgo-v3))
    (home-page "https://modernc.org/tcl")
    (synopsis "tcl")
    (description
     "Package tcl is a CGo-free port of the Tool Command Language (Tcl).")
    (license license:bsd-3)))

(define-public go-modernc-org-z
  (package
    (name "go-modernc-org-z")
    (version "1.7.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gitlab.com/cznic/z")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1apnzzcxrjyi14pxgdgifbb2b1gachxzl3jlsbld9hpn9cikgwmj"))))
    (build-system go-build-system)
    (arguments '(#:import-path "modernc.org/z"))
    (propagated-inputs
     (list go-modernc-org-token
           go-modernc-org-strutil
           go-modernc-org-opt
           go-modernc-org-memory
           go-modernc-org-mathutil
           go-modernc-org-cc-v3
           go-lukechampine-com-uint128
           go-golang-org-x-xerrors
           go-golang-org-x-tools
           go-golang-org-x-sys
           go-golang-org-x-mod
           go-github-com-remyoudompheng-bigfft
           go-github-com-mattn-go-isatty
           go-github-com-kballard-go-shellquote
           go-github-com-google-uuid
           go-modernc-org-libc
           go-modernc-org-ccgo-v3))
    (home-page "https://modernc.org/z")
    (synopsis "z")
    (description "Package z implements the native Go API for zlib.")
    (license (list license:zlib license:bsd-3))))

(define-public go-moul-io-http2curl
  (package
    (name "go-moul-io-http2curl")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/moul/http2curl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "15bpx33d3ygya8dg8hbsn24h7acpajl27006pj8lw1c0bfvbnrl0"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (delete-file-recursively "vendor")))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "moul.io/http2curl"))
    (native-inputs
     (list go-github-com-smartystreets-goconvey))
    (home-page "https://moul.io/http2curl")
    (synopsis "http2curl")
    (description #f)
    (license license:expat)))

(define-public go-rsc-io-qr
  (package
    (name "go-rsc-io-qr")
    (version "0.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/rsc/qr")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "04yx493g0fqp8i59zjxnl4k3s0cl0kr5m8xh0ph8m10r1hkw0xr3"))))
    (build-system go-build-system)
    (arguments '(#:import-path "rsc.io/qr"))
    (home-page "https://rsc.io/qr")
    (synopsis #f)
    (description #f)
    (license license:bsd-3)))

(define-public go-sigs-k8s-io-controller-runtime
  (package
    (name "go-sigs-k8s-io-controller-runtime")
    (version "0.14.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url
                     "https://github.com/kubernetes-sigs/controller-runtime")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "08gq6xygmpx4ixlnid40pa7c3h8karphrn7nb8f2ga33mh99mppy"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "sigs.k8s.io/controller-runtime"))
    (propagated-inputs
     `(("go-sigs-k8s-io-structured-merge-diff-v4" ,go-sigs-k8s-io-structured-merge-diff-v4)
       ("go-sigs-k8s-io-json" ,go-sigs-k8s-io-json)
       ("go-k8s-io-kube-openapi" ,go-k8s-io-kube-openapi)
       ("go-gopkg-in-yaml-v3" ,go-gopkg-in-yaml-v3)
       ("go-gopkg-in-yaml-v2" ,go-gopkg-in-yaml-v2)
       ("go-gopkg-in-inf-v0" ,go-gopkg-in-inf-v0)
       ("go-google-golang-org-protobuf" ,go-google-golang-org-protobuf)
       ("go-google-golang-org-appengine" ,go-google-golang-org-appengine)
       ("go-golang-org-x-text" ,go-golang-org-x-text)
       ("go-golang-org-x-term" ,go-golang-org-x-term)
       ("go-golang-org-x-oauth2" ,go-golang-org-x-oauth2)
       ("go-golang-org-x-net" ,go-golang-org-x-net)
       ("go-go-uber-org-multierr" ,go-go-uber-org-multierr)
       ("go-go-uber-org-atomic" ,go-go-uber-org-atomic)
       ("go-github-com-spf13-pflag" ,go-github-com-spf13-pflag)
       ("go-github-com-prometheus-procfs" ,go-github-com-prometheus-procfs)
       ("go-github-com-prometheus-common" ,go-github-com-prometheus-common)
       ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
       ("go-github-com-munnerz-goautoneg" ,go-github-com-munnerz-goautoneg)
       ("go-github-com-modern-go-reflect2" ,go-github-com-modern-go-reflect2)
       ("go-github-com-modern-go-concurrent" ,go-github-com-modern-go-concurrent)
       ;("go-github-com-matttproud-golang-protobuf-extensions" ,go-github-com-matttproud-golang-protobuf-extensions)
       ("go-github-com-mailru-easyjson" ,go-github-com-mailru-easyjson)
       ("go-github-com-json-iterator-go" ,go-github-com-json-iterator-go)
       ("go-github-com-josharian-intern" ,go-github-com-josharian-intern)
       ;("go-github-com-imdario-mergo" ,go-github-com-imdario-mergo)
       ("go-github-com-google-uuid" ,go-github-com-google-uuid)
       ("go-github-com-google-gofuzz" ,go-github-com-google-gofuzz)
       ("go-github-com-google-gnostic" ,go-github-com-google-gnostic)
       ("go-github-com-golang-protobuf" ,go-github-com-golang-protobuf)
       ("go-github-com-golang-groupcache" ,go-github-com-golang-groupcache)
       ("go-github-com-gogo-protobuf" ,go-github-com-gogo-protobuf)
       ("go-github-com-go-openapi-swag" ,go-github-com-go-openapi-swag)
       ("go-github-com-go-openapi-jsonreference" ,go-github-com-go-openapi-jsonreference)
       ("go-github-com-go-openapi-jsonpointer" ,go-github-com-go-openapi-jsonpointer)
       ("go-github-com-evanphx-json-patch" ,go-github-com-evanphx-json-patch-v4)
       ("go-github-com-emicklei-go-restful-v3" ,go-github-com-emicklei-go-restful-v3)
       ("go-github-com-davecgh-go-spew" ,go-github-com-davecgh-go-spew)
       ("go-github-com-cespare-xxhash-v2" ,go-github-com-cespare-xxhash-v2)
       ("go-github-com-beorn7-perks" ,go-github-com-beorn7-perks)
       ("go-sigs-k8s-io-yaml" ,go-sigs-k8s-io-yaml)
       ("go-k8s-io-utils" ,go-k8s-io-utils)
       ("go-k8s-io-klog-v2" ,go-k8s-io-klog-v2)
       ;("go-k8s-io-component-base" ,go-k8s-io-component-base)
       ("go-k8s-io-client-go" ,go-k8s-io-client-go)
       ("go-k8s-io-apimachinery" ,go-k8s-io-apimachinery)
       ;("go-k8s-io-apiextensions-apiserver" ,go-k8s-io-apiextensions-apiserver)
       ("go-k8s-io-api" ,go-k8s-io-api)
       ;("go-gomodules-xyz-jsonpatch-v2" ,go-gomodules-xyz-jsonpatch-v2)
       ("go-golang-org-x-time" ,go-golang-org-x-time)
       ("go-golang-org-x-sys" ,go-golang-org-x-sys)
       ("go-go-uber-org-zap" ,go-go-uber-org-zap)
       ;("go-go-uber-org-goleak" ,go-go-uber-org-goleak)
       ("go-github-com-prometheus-client-model" ,go-github-com-prometheus-client-model)
       ("go-github-com-prometheus-client-golang" ,go-github-com-prometheus-client-golang)
       ("go-github-com-onsi-gomega" ,go-github-com-onsi-gomega)
       ("go-github-com-onsi-ginkgo-v2" ,go-github-com-onsi-ginkgo-v2)
       ("go-github-com-google-go-cmp" ,go-github-com-google-go-cmp)
       ;("go-github-com-go-logr-zapr" ,go-github-com-go-logr-zapr)
       ("go-github-com-go-logr-logr" ,go-github-com-go-logr-logr)
       ("go-github-com-fsnotify-fsnotify" ,go-github-com-fsnotify-fsnotify)
       ("go-github-com-evanphx-json-patch-v5" ,go-github-com-evanphx-json-patch-v5)))
    (home-page "https://sigs.k8s.io/controller-runtime")
    (synopsis "Kubernetes controller-runtime Project")
    (description
     "Package controllerruntime provides tools to construct Kubernetes-style
controllers that manipulate both Kubernetes CRDs and aggregated/built-in
Kubernetes APIs.")
    (license license:asl2.0)))

(define-public go-stathat-com-c-ramcache
  (package
    (name "go-stathat-com-c-ramcache")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/stathat/ramcache")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "00ncfrq3dp1krhr8360y6n0v3vx3mcgmi2f7gpmc5hlprcamsbxl"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "stathat.com/c/ramcache"))
    (home-page "https://stathat.com/c/ramcache")
    (synopsis "ramcache")
    (description #f)
    (license license:bsd-3)))

(define-public go-strk-kbt-io-projects-go-libravatar
  (package
    (name "go-strk-kbt-io-projects-go-libravatar")
    (version "0.0.0-20191008002943-06d1c002b251")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://strk.kbt.io/git/go-libravatar")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1s5q8iyx0sy5fav7qbivmza755pv5yjjjqh51vhs03pcxcz8fbi1"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f      ; TODO: Fix
       #:import-path "strk.kbt.io/projects/go/libravatar"))
    (home-page "https://strk.kbt.io/projects/go/libravatar")
    (synopsis "Federated avatars using Go")
    (description "This package provides a library library for serving
@url{https://www.libravatar.org, federated avatars}.")
    (license license:expat)))

(define-public go-xorm-io-builder
  (package
    (name "go-xorm-io-builder")
    (version "0.3.9")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gitea.com/xorm/builder")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0b91wibpzxhi038hqp709aqvwipqd20khgis3ngk84dd7xmv6k83"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f      ; Tests try to connect to the Internet.
       #:import-path "xorm.io/builder"))
    (propagated-inputs
     (list go-gitea-com-xorm-sqlfiddle))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://xorm.io/builder")
    (synopsis "SQL builder")
    (description
      "Package builder is a simple and powerful sql builder for Go.")
    (license license:bsd-3)))

(define-public go-xorm-io-xorm
  (package
    (name "go-xorm-io-xorm")
    (version "1.2.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gitea.com/xorm/xorm")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0k94bimi91h12cwjbqjcrj1ybaasj9lsf9qr5w46774pjkyq948h"))))
    (build-system go-build-system)
    (arguments '(#:import-path "xorm.io/xorm"))
    (propagated-inputs
     (list go-xorm-io-builder
           ;go-modernc-org-sqlite
           ;go-github-com-ziutek-mymysql
           go-github-com-syndtr-goleveldb
           ;go-github-com-stretchr-testify
           ;go-github-com-shopspring-decimal
           ;go-github-com-mattn-go-sqlite3
           ;go-github-com-lib-pq
           ;go-github-com-json-iterator-go
           ;go-github-com-jackc-pgx-v4
           ;go-github-com-goccy-go-json
           ;go-github-com-go-sql-driver-mysql
           ;go-github-com-denisenkom-go-mssqldb
           ))
    (home-page "https://xorm.io/xorm")
    (synopsis "xorm")
    (description "Package xorm is a simple and powerful ORM for Go.")
    (license license:bsd-3)))
