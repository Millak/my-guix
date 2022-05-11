;;; Copyright © 2020, 2021, 2022 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (guix build utils)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system go)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages node)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages syncthing)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages version-control))

;; These packages are sorted alphabetically for lack of a better option.

(define-public go-bazil-org-fuse
  (let ((commit "5a45981690d8c47319ea10f5f1c1ba6e2cc0147b")
        (revision "1"))
    (package
      (name "go-bazil-org-fuse")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/bazil/fuse")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0didws29ca6xqvyqg3pfy4g1hd4ij9xhhxxc5ngvaxyxk5gd0rcy"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "bazil.org/fuse"
         #:tests? #f))  ; Tests require fusermount and a fuse device.
      (propagated-inputs
       `(("go-golang-org-x-sys" ,go-golang-org-x-sys)))
      (native-inputs
       `(("fuse" ,(@ (gnu packages linux) fuse))
         ("go-golang-org-x-net" ,go-golang-org-x-net)))
      (home-page "https://bazil.org/fuse/")
      (synopsis "Go library for writing filesystems")
      (description "This package is a Go library for writing filesystems.  It is
a from-scratch implementation of the kernel-userspace communication protocol,
and does not use the C library from the project called FUSE.")
      (license license:bsd-3))))

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
     (list go-github-com-hashicorp-go-version-1.3.0))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://code.gitea.io/sdk")
    (synopsis "Gitea SDK for Go")
    (description
     "This project acts as a client SDK implementation written in Go to interact
with the Gitea API implementation.  For further informations take a look at the
current @url{https://godoc.org/code.gitea.io/sdk/gitea,documentation}.")
    (license license:expat)))

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
    (version "1.3.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/6543/go-version")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0ags84q6yvdg6c77zadkw45hnjxp221ik3d7374q86jz1hss981v"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/6543/go-version"))
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

(define-public go-github-com-anmitsu-go-shlex
  (package
    (name "go-github-com-anmitsu-go-shlex")
    (version "0.0.0-20200514113438-38f4b401e2be")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/anmitsu/go-shlex")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "17iz68yzbnr7y4s493asbagbv79qq8hvl2pkxvm6bvdkgphj8w1g"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/anmitsu/go-shlex"))
    (home-page "https://github.com/anmitsu/go-shlex")
    (synopsis "go-shlex")
    (description
      "Package shlex provides a simple lexical analysis like Unix shell.")
    (license license:expat)))

(define-public go-github-com-antchfx-htmlquery
  (package
    (name "go-github-com-antchfx-htmlquery")
    (version "1.2.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/antchfx/htmlquery")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1g24619hys0k014ga445xzvh51g1yp07b08jjgwkfycjrjldxp46"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/antchfx/htmlquery"))
    (propagated-inputs
     (list go-github-com-antchfx-xpath
           go-github-com-golang-groupcache-lru
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
       ("go-github-com-golang-groupcache-lru" ,go-github-com-golang-groupcache-lru)
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

(define-public go-github-com-antihax-optional
  (package
    (name "go-github-com-antihax-optional")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/antihax/optional")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1ix08vl49qxr58rc6201cl97g1yznhhkwvqldslawind99js4rj0"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/antihax/optional"))
    (home-page "https://github.com/antihax/optional")
    (synopsis #f)
    (description #f)
    (license license:expat)))

(define-public go-github-com-araddon-dateparse
  (package
    (name "go-github-com-araddon-dateparse")
    (version "0.0.0-20210207001429-0eec95c9db7e")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/araddon/dateparse.git")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "10lgc2clpzbz3g260pq213m4id3pqww4da0azz9ldxjhy14an3s8"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/araddon/dateparse"))
    (native-inputs
     `(("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))
    (home-page "https://github.com/araddon/dateparse")
    (synopsis "Go Date Parser")
    (description #f)
    (license license:expat)))

(define-public go-github-com-armon-go-socks5
  (package
    (name "go-github-com-armon-go-socks5")
    (version "0.0.0-20160902184237-e75332964ef5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/armon/go-socks5")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "104w10jf0wlxyxi35hf6frndgf0ybz21h54xjmnkivpb6slycpyq"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/armon/go-socks5"))
    (propagated-inputs
     (list go-golang-org-x-net))
    (home-page "https://github.com/armon/go-socks5")
    (synopsis "go-socks5")
    (description
     "This package provides the @code{socks5} package that implements a
@url{http://en.wikipedia.org/wiki/SOCKS,SOCKS5 server}.  SOCKS (Secure Sockets)
is used to route traffic between a client and server through an intermediate
proxy layer.  This can be used to bypass firewalls or NATs.")
    (license license:expat)))

(define-public go-github-com-asaskevich-govalidator
  (package
    (name "go-github-com-asaskevich-govalidator")
    (version "11.0.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/asaskevich/govalidator.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0aab1pym5c6di8vidynp6ly5j4kcqv6lp2737gw0a07zng0nn8lw"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/asaskevich/govalidator"
       ;; Email validation is hard. TestIsExistingEmail has multiple bug reports.
       #:tests? #f))
    (home-page "https://github.com/asaskevich/govalidator")
    (synopsis "govalidator")
    (description
     "Package govalidator is package of validators and sanitizers for strings, structs
and collections.")
    (license license:expat)))

(define-public go-github-com-bits-and-blooms-bitset
  (package
    (name "go-github-com-bits-and-blooms-bitset")
    (version "1.2.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/bits-and-blooms/bitset")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0p1g98klqwbdilqk0fkg8n7x8rjncqc52cva95rh7jl0k3q9d9x4"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/bits-and-blooms/bitset"))
    (home-page "https://github.com/bits-and-blooms/bitset")
    (synopsis "bitset")
    (description
     "Package bitset implements bitsets, a mapping between non-negative integers and
boolean values.  It should be more efficient than map[uint] bool.")
    (license license:bsd-3)))

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
       ("go-github-com-golang-protobuf-proto" ,go-github-com-golang-protobuf-proto)
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
           go-github-com-golang-protobuf-proto
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
instead depends only on the indepenent interface modules:")
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
instead depends only on the indepenent interface modules:")
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
instead depends only on the indepenent interface modules:")
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
instead depends only on the indepenent interface modules:")
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
instead depends only on the indepenent interface modules:")
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

(define-public go-github-com-caddyserver-certmagic
  (package
    (name "go-github-com-caddyserver-certmagic")
    (version "0.14.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/caddyserver/certmagic")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0fda736d9787bw1zirs7ns5chsalgzbn6dsb8sagl1hy1kj55w4s"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f      ; Tests require network access.
       #:import-path "github.com/caddyserver/certmagic"))
    (propagated-inputs
     (list go-github-com-klauspost-cpuid-v2
           go-github-com-libdns-libdns
           go-github-com-mholt-acmez
           go-github-com-miekg-dns
           go-go-uber-org-zap
           go-golang-org-x-crypto
           go-golang-org-x-net))
    (home-page "https://github.com/caddyserver/certmagic")
    (synopsis "Fully-managed TLS certificate issuance and renewal")
    (description
     "Package certmagic automates the obtaining and renewal of TLS certificates,
including TLS & HTTPS best practices such as robust OCSP stapling, caching,
HTTP->HTTPS redirects, and more.")
    (license license:asl2.0)))

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

(define-public go-github-com-cespare-xxhash-v2
  (package
    (name "go-github-com-cespare-xxhash-v2")
    (version "2.1.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/cespare/xxhash")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1f3wyr9msnnz94szrkmnfps9wm40s5sp9i4ak0kl92zcrkmpy29a"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/cespare/xxhash/v2"))
    (home-page "https://github.com/cespare/xxhash")
    (synopsis "xxhash")
    (description
      "Package xxhash implements the 64-bit variant of xxHash (XXH64) as described at
@url{http://cyan4973.github.io/xxHash/,http://cyan4973.github.io/xxHash/}.")
    (license license:expat)))

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

(define-public go-github-com-chzyer-logex
  (package
    (name "go-github-com-chzyer-logex")
    (version "1.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/chzyer/logex")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "07ksz43a8kvx0hm8qji6kb1xm7fbwmwapcvcq9zpc8v337jggs4g"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f      ; https://github.com/chzyer/logex/issues/4
       #:import-path "github.com/chzyer/logex"))
    (home-page "https://github.com/chzyer/logex")
    (synopsis "Enhanced logging library")
    (description
     "A golang log lib, supports tracing and level, wrapped by the standard log lib.")
    (license license:expat)))

(define-public go-github-com-chzyer-readline
  (package
    (name "go-github-com-chzyer-readline")
    (version "0.0.0-20180603132655-2972be24d48e")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/chzyer/readline")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "104q8dazj8yf6b089jjr82fy9h1g80zyyzvp3g8b44a7d8ngjj6r"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/chzyer/readline"))
    (native-inputs
     (list go-github-com-chzyer-test))
    (home-page "https://github.com/chzyer/readline")
    (synopsis "Golang readline implementation")
    (description
     "Readline is a pure go implementation for GNU-Readline kind library.")
    (license license:expat)))

(define-public go-github-com-chzyer-test
  (package
    (name "go-github-com-chzyer-test")
    (version "0.0.0-20210722231415-061457976a23")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/chzyer/test")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1jjskijacwzz0qxzrbwsglpg5vil7v4xaq8l40r2fhd2icl9hz7a"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f      ; nil pointer dereference‽
       #:import-path "github.com/chzyer/test"))
    (propagated-inputs
     (list go-github-com-chzyer-logex))
    (home-page "https://github.com/chzyer/test")
    (synopsis "Golang test library")
    (description "This package provides a testing library for
go-github-com-chzyer packages.")
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

(define-public go-github-com-cockroachdb-apd
  (package
    (name "go-github-com-cockroachdb-apd")
    (version "1.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/cockroachdb/apd")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "14jnnqpdsa3vxh2zpznd2dpnychcrlkljppfplrigrs245slyh72"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/cockroachdb/apd"))
    (home-page "https://github.com/cockroachdb/apd")
    (synopsis "apd")
    (description "Package apd implements arbitrary-precision decimals.")
    (license license:asl2.0)))

(define-public go-github-com-coreos-go-oidc
  (package
    (name "go-github-com-coreos-go-oidc")
    (version "2.2.1+incompatible")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/coreos/go-oidc")
               (commit (string-append "v" (go-version->git-ref version)))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "11m6slbpi33ynffml7812piq4anhjlf1qszjlsf26f5y7x3qh8n5"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/coreos/go-oidc"))
    (propagated-inputs
     (list go-github-com-pquerna-cachecontrol
           go-golang-org-x-oauth2
           go-gopkg-in-square-go-jose-v2))
    (home-page "https://github.com/coreos/go-oidc")
    (synopsis "go-oidc")
    (description
      "Package oidc implements OpenID Connect client logic for the golang.org/x/oauth2
package.")
    (license license:asl2.0)))

(define-public go-github-com-coreos-go-systemd-activation
  (package
    (name "go-github-com-coreos-go-systemd-activation")
    (version "0.0.0-20170731111925-d21964639418")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/coreos/go-systemd")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1kcgnsm9l1kk8nfv1lkvkrjghy1aavhzv7d9f6l5qi2wqyvj48dw"))))
    (build-system go-build-system)
    (arguments
     '(#:unpack-path "github.com/coreos/go-systemd"
       #:import-path "github.com/coreos/go-systemd/activation"))
    (home-page "https://github.com/coreos/go-systemd")
    (synopsis "go-systemd")
    (description #f)
    (license license:asl2.0)))

(define-public go-github-com-coreos-go-systemd-daemon
  (package
    (inherit go-github-com-coreos-go-systemd-activation)
    (name "go-github-com-coreos-go-systemd-daemon")
    (arguments
     '(#:unpack-path "github.com/coreos/go-systemd"
       #:import-path "github.com/coreos/go-systemd/daemon"))
    (synopsis "go-systemd")
    (description #f)))

(define-public go-github-com-coreos-go-systemd-util
  (package
    (inherit go-github-com-coreos-go-systemd-activation)
    (name "go-github-com-coreos-go-systemd-util")
    (arguments
     '(#:unpack-path "github.com/coreos/go-systemd"
       #:import-path "github.com/coreos/go-systemd/util"))
    (propagated-inputs
     `(("go-github-com-coreos-pkg-dlopen" ,go-github-com-coreos-pkg-dlopen)))
    (synopsis "go-systemd")
    (description #f)))

(define-public go-github-com-coreos-go-systemd-v22
  (package
    (name "go-github-com-coreos-go-systemd-v22")
    (version "22.3.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/coreos/go-systemd")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1ndi86b8va84ha93njqgafypz4di7yxfd5r5kf1r0s3y3ghcjajq"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/coreos/go-systemd/v22"))
    (propagated-inputs
     (list go-github-com-godbus-dbus-v5))
    (home-page "https://github.com/coreos/go-systemd")
    (synopsis "go-systemd")
    (description "Go bindings to systemd.  The project has several packages:")
    (license license:asl2.0)))

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
       ("go-github-com-willf-bitset" ,go-github-com-willf-bitset)
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

(define-public go-github-com-dgryski-go-rendezvous
  (package
    (name "go-github-com-dgryski-go-rendezvous")
    (version "0.0.0-20200823014737-9f7001d12a5f")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/dgryski/go-rendezvous")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0hhdbsm5k19kh1fyxs4aibza9jylils4p3555lr8xalhj2iz3zlz"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/dgryski/go-rendezvous"))
    (home-page "https://github.com/dgryski/go-rendezvous")
    (synopsis #f)
    (description #f)
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

(define-public go-github-com-dsnet-compress
  (package
    (name "go-github-com-dsnet-compress")
    (version "0.0.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/dsnet/compress")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1wwjaymzb1xxq3ybch3nwn72xhi2s40cvz0cl986yad3w1xwzj91"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/dsnet/compress"))
    (propagated-inputs
     (list go-github-com-ulikunitz-xz
           go-github-com-klauspost-cpuid
           go-github-com-klauspost-compress
           go-github-com-dsnet-golib))
    (home-page "https://github.com/dsnet/compress")
    (synopsis "Collection of compression libraries for Go")
    (description "Package compress is a collection of compression libraries.")
    (license license:bsd-3)))

(define-public go-github-com-dsnet-golib
  (package
    (name "go-github-com-dsnet-golib")
    (version "1.0.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/dsnet/golib")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1f314wzr16w6ix3bs7ginjkizgyl3b1r3j2gvvqzr8dv53r4s5cq"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/dsnet/golib"))
    (home-page "https://github.com/dsnet/golib")
    (synopsis "Collection of helper libraries for Go")
    (description "Package golib is a collection of unrelated libraries.")
    (license license:bsd-3)))

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

(define-public go-github-com-edsrzf-mmap-go
  (package
    (name "go-github-com-edsrzf-mmap-go")
    (version "1.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/edsrzf/mmap-go")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "11xpfcacfvmrkbp0pv4j8pg2gyjnxpfp7l93j42h0svwxywhjmrc"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/edsrzf/mmap-go"))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://github.com/edsrzf/mmap-go")
    (synopsis "Portable mmap package for Go")
    (description
     "Package mmap allows mapping files into memory.  It tries to provide a simple,
reasonably portable interface, but doesn't go out of its way to abstract away
every little platform detail.")
    (license license:bsd-3)))

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

(define-public go-github-com-fatih-structs
  (package
    (name "go-github-com-fatih-structs")
    (version "1.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/fatih/structs")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1wrhb8wp8zpzggl61lapb627lw8yv281abvr6vqakmf569nswa9q"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/fatih/structs"))
    (home-page "https://github.com/fatih/structs")
    (synopsis "Structs")
    (description #f)
    (license license:expat)))

(define-public go-github-com-fatih-structtag
  (package
    (name "go-github-com-fatih-structtag")
    (version "1.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/fatih/structtag")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "09a9pycvkf384v5f47ff4q33bjbzpx6kbkn23za1gcwc96466sk3"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/fatih/structtag"))
    (home-page "https://github.com/fatih/structtag")
    (synopsis "structtag")
    (description
      "structtag provides an easy way of parsing and manipulating struct tag fields.
Please vendor the library as it might change in future versions.")
    (license license:bsd-3)))

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

(define-public go-github-com-gliderlabs-ssh
  (package
    (name "go-github-com-gliderlabs-ssh")
    (version "0.3.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/gliderlabs/ssh")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "111xgg8gx9cd0wigml84zzysxywiygjr2vl934j8sy97hywchwvl"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/gliderlabs/ssh"))
    (propagated-inputs
     (list go-golang-org-x-sys
           go-golang-org-x-crypto
           go-github-com-anmitsu-go-shlex))
    (home-page "https://github.com/gliderlabs/ssh")
    (synopsis "gliderlabs/ssh")
    (description
      "Package ssh wraps the crypto/ssh package with a higher-level API for building
SSH servers.  The goal of the API was to make it as simple as using net/http, so
the API is very similar.")
    (license license:bsd-3)))

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
           go-github.com-jtolds-gls))
    (home-page "https://github.com/glycerine/go-unsnap-stream")
    (synopsis "go-unsnap-stream")
    (description
     "This is a small golang library for decoding and encoding the snappy  format,
specified here:
@url{https://github.com/google/snappy/blob/master/framing_format.txt,https://github.com/google/snappy/blob/master/framing_format.txt}")
    (license license:expat)))

(define-public go-github-com-goccy-go-json
  (package
    (name "go-github-com-goccy-go-json")
    (version "0.8.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/goccy/go-json")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1g18bzlwyg3hq3jizjm6b2b8b4hghmiacid16adlxz4h8g7bp4jh"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/goccy/go-json"))
    (home-page "https://github.com/goccy/go-json")
    (synopsis "go-json")
    (description
     "Fast JSON encoder/decoder compatible with encoding/json for Go.")
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

(define-public go-github-com-godbus-dbus-v5
  (package
    (name "go-github-com-godbus-dbus-v5")
    (version "5.0.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/godbus/dbus")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0d7740bjprd6mhs1wmhd53fb3kf61dz0hp1a0dda3dc28w2z84pp"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/godbus/dbus/v5"))
    (home-page "https://github.com/godbus/dbus")
    (synopsis "dbus")
    (description
      "Package dbus implements bindings to the D-Bus message bus system.")
    (license license:bsd-2)))

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

(define-public go-github-com-go-errors-errors
  (package
    (name "go-github-com-go-errors-errors")
    (version "1.1.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/go-errors/errors")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1g9wwirsdddkxlqj6ymmy3dkh7xavkh3ybsvsnvyy4jyf0fw9fw8"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/go-errors/errors"))
    (home-page "https://github.com/go-errors/errors")
    (synopsis "go-errors/errors")
    (description #f)
    (license license:expat)))

(define-public go-github-com-gofrs-uuid
  (package
    (name "go-github-com-gofrs-uuid")
    (version "4.2.0+incompatible")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/gofrs/uuid")
               (commit (string-append "v" (go-version->git-ref version)))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "04h6lz1h4jp0cdzq9qa8zmvb2fy84wwl32smqsjk9k62bxg78ivq"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/gofrs/uuid"))
    (home-page "https://github.com/gofrs/uuid")
    (synopsis "UUID")
    (description
      "Package uuid provides implementations of the Universally Unique Identifier
(UUID), as specified in RFC-4122 and the Peabody RFC Draft (revision 02).")
    (license license:expat)))

(define-public go-github-com-go-git-go-billy-v5
  (package
    (name "go-github-com-go-git-go-billy-v5")
    (version "5.3.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/go-git/go-billy")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0x5z649f47lwsqw8578a5hvgjaxp0zz6cg184s0n68xdqhza3m2i"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/go-git/go-billy/v5"))
    (propagated-inputs
     (list go-gopkg-in-check-v1
           go-golang-org-x-sys
           ;; These two are indirect and don't seem to be needed?
           ;go-github-com-niemeyer-pretty
           ;go-github-com-kr-text
           ))
    (home-page "https://github.com/go-git/go-billy")
    (synopsis "go-billy")
    (description
      "The missing interface filesystem abstraction for Go.  Billy implements an
interface based on the @code{os} standard library, allowing to develop
applications without dependency on the underlying storage.  Makes it virtually
free to implement mocks and testing over filesystem operations.")
    (license license:asl2.0)))

(define-public go-github-com-go-git-go-git-fixtures-v4
  (package
    (name "go-github-com-go-git-go-git-fixtures-v4")
    (version "4.3.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/go-git/go-git-fixtures")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0vxsyfrw4c0kqjbiycdx4xxpz8zjq69pwbsfxmcvkizpj87ywf9d"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/go-git/go-git-fixtures/v4"))
    (propagated-inputs
     (list go-gopkg-in-check-v1
           go-github-com-go-git-go-billy-v5))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/go-git/go-git-fixtures")
    (synopsis "go-git-fixtures")
    (description
      "git repository fixtures used by @url{https://github.com/go-git/go-git,go-git}")
    (license license:asl2.0)))

(define-public go-github-com-go-git-go-git-v5
  (package
    (name "go-github-com-go-git-go-git-v5")
    (version "5.4.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/go-git/go-git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "08kdknkrh9qks8qykmd1hmc573cb6qbb4b10f57k3kik4ygq2frj"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f      ; Unlabeled test fails.
       #:import-path "github.com/go-git/go-git/v5"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-network-tests
           (lambda _
             (setenv "GIT_AUTHOR_NAME" "Your Name")
             (setenv "GIT_COMMITTER_NAME" "Your Name")
             (setenv "GIT_AUTHOR_EMAIL" "you@example.com")
             (setenv "GIT_COMMITTER_EMAIL" "you@example.com")
             (substitute* "src/github.com/go-git/go-git/v5/blame_test.go"
               (("TestBlame\\(" all) (string-append "Disable" all)))
             (substitute* "src/github.com/go-git/go-git/v5/references_test.go"
               (("TestEquivalent\\(" all) (string-append "Disable" all))
               (("TestRevList\\(" all) (string-append "Disable" all)))
             (substitute* "src/github.com/go-git/go-git/v5/remote_test.go"
               (("TestFetchExactSHA1\\(" all) (string-append "Disable" all))
               (("TestList\\(" all) (string-append "Disable" all)))
             (substitute* "src/github.com/go-git/go-git/v5/repository_test.go"
               (("TestConfigScoped\\(" all) (string-append "Disable" all))          ; Cannot find User.Email
               (("TestCreateTagAnnotatedBadOpts\\(" all) (string-append "Disable" all))
               (("TestPlainCloneWithRecurseSubmodules\\(" all) (string-append "Disable" all))
               (("TestPushWithProgress\\(" all) (string-append "Disable" all)))
             (substitute* "src/github.com/go-git/go-git/v5/submodule_test.go"
               (("TestUpdate\\(" all) (string-append "Disable" all))
               (("TestUpdateWithInitAndUpdate\\(" all) (string-append "Disable" all))
               (("TestUpdateWithRecursion\\(" all) (string-append "Disable" all)))
             (substitute* "src/github.com/go-git/go-git/v5/worktree_commit_test.go"
               (("TestCommitEmptyOptions\\(" all) (string-append "Disable" all)))   ; Cannot find author field.
             (substitute* "src/github.com/go-git/go-git/v5/worktree_test.go"
               (("TestCheckoutRelativePathSubmoduleInitialized\\(" all) (string-append "Disable" all))
               (("TestCheckoutSubmoduleInitialized\\(" all) (string-append "Disable" all))
               (("TestPullProgressWithRecursion\\(" all) (string-append "Disable" all))))))))
    (propagated-inputs
     (list go-gopkg-in-warnings-v0
           go-gopkg-in-check-v1
           go-golang-org-x-text
           go-golang-org-x-sys-next
           go-golang-org-x-net
           go-golang-org-x-crypto
           go-github-com-xanzy-ssh-agent
           go-github-com-sergi-go-diff
           go-github-com-mitchellh-go-homedir
           go-github-com-kevinburke-ssh-config
           go-github.com-jessevdk-go-flags
           go-github-com-jbenet-go-context
           go-github-com-imdario-mergo
           go-github-com-google-go-cmp
           go-github-com-go-git-go-git-fixtures-v4
           go-github-com-go-git-go-billy-v5
           go-github-com-go-git-gcfg
           go-github-com-gliderlabs-ssh
           go-github-com-flynn-go-shlex
           go-github-com-emirpasic-gods
           go-github-com-armon-go-socks5
           go-github-com-anmitsu-go-shlex
           go-github-com-acomagu-bufpipe
           go-github-com-protonmail-go-crypto
           go-github-com-microsoft-go-winio))
    (native-inputs
     (list git-minimal))
    (home-page "https://github.com/go-git/go-git")
    (synopsis "Project Status")
    (description
      "This package provides a highly extensible git implementation in pure Go.")
    (license license:asl2.0)))

(define-public go-github-com-gogs-chardet
  (package
    (name "go-github-com-gogs-chardet")
    (version "0.0.0-20211120154057-b7413eaefb8f")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/gogs/chardet")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "12j8q5wc9m4n51v2j2m40nahqdl9bh3hzpdp26clzq91kc2amiz0"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/gogs/chardet"))
    (home-page "https://github.com/gogs/chardet")
    (synopsis "chardet")
    (description "Package chardet ports character set detection from ICU.")
    (license license:expat)))

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

(define-public go-github-com-go-kit-log
  (package
    (name "go-github-com-go-kit-log")
    (version "0.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/go-kit/log")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "13gbqrwvqy9j903j44x0kix5gnn34a8hl8skbdijy7arpkxpj6h3"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/go-kit/log"))
    (propagated-inputs
     (list go-github-com-go-logfmt-logfmt))
    (home-page "https://github.com/go-kit/log")
    (synopsis "package log")
    (description "Package log provides a structured logger.")
    (license license:expat)))

(define-public go-github-com-golang-glog
  (package
    (name "go-github-com-golang-glog")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/golang/glog")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0vm206qrvhn3d571bqcman6fnavw4y3a31ffrmv2xkk0li74h2bf"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/golang/glog"))
    (home-page "https://github.com/golang/glog")
    (synopsis "glog")
    (description
      "Package glog implements logging analogous to the Google-internal C++
INFO/ERROR/V setup.  It provides functions Info, Warning, Error, Fatal, plus
formatting variants such as Infof.  It also provides V-style logging controlled
by the -v and -vmodule=file=2 flags.")
    (license license:asl2.0)))

(define-public go-github-com-golang-groupcache
  (package
    (name "go-github-com-golang-groupcache")
    (version "0.0.0-20210331224755-41bb18bfe9da")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/golang/groupcache")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "07amgr8ji4mnq91qbsw2jlcmw6hqiwdf4kzfdrj8c4b05w4knszc"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/golang/groupcache"))
    (home-page "https://github.com/golang/groupcache")
    (synopsis "groupcache")
    (description
      "Package groupcache provides a data loading mechanism with caching and
de-duplication that works across a set of peer processes.")
    (license license:asl2.0)))

(define-public go-github-com-golang-groupcache-singleflight
  (package
    (inherit go-github-com-golang-groupcache-lru)
    (name "go-github-com-golang-groupcache-singleflight")
    (arguments
     '(#:unpack-path "github.com/golang/groupcache"
       #:import-path "github.com/golang/groupcache/singleflight"))
    (synopsis "groupcache")
    (description #f)))

(define-public go-github-com-golang-jwt-jwt
  (package
    (name "go-github-com-golang-jwt-jwt")
    (version "3.2.2+incompatible")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/golang-jwt/jwt")
               (commit (string-append "v" (go-version->git-ref version)))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0hq8wz11g6kddx9ab0icl5h3k4lrivk1ixappnr5db2ng2wjks9c"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/golang-jwt/jwt"))
    (home-page "https://github.com/golang-jwt/jwt")
    (synopsis "jwt-go")
    (description
      "Package jwt is a Go implementation of JSON Web Tokens:
@url{http://self-issued.info/docs/draft-jones-json-web-token.html,http://self-issued.info/docs/draft-jones-json-web-token.html}")
    (license license:expat)))

(define-public go-github-com-golang-mock-gomock
  (package
    (name "go-github-com-golang-mock-gomock")
    (version "1.5.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/golang/mock")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "12l7p08pwwk3xn70w7rlm28nz6jf4szlzgjxjfmbssyirxxxy8v1"))))
    (build-system go-build-system)
    (arguments
     '(#:unpack-path "github.com/golang/mock"
       #:import-path "github.com/golang/mock/gomock"))
    (home-page "https://github.com/golang/mock")
    (synopsis "gomock")
    (description #f)
    (license license:asl2.0)))

(define-public go-github-com-golang-protobuf
  (deprecated-package "go-github-com-golang-protobuf" go-github-com-golang-protobuf-proto))
;  (package
;    (name "go-github-com-golang-protobuf")
;    (version "1.5.2")
;    (source
;      (origin
;        (method git-fetch)
;        (uri (git-reference
;               (url "https://github.com/golang/protobuf")
;               (commit (string-append "v" version))))
;        (file-name (git-file-name name version))
;        (sha256
;          (base32 "1mh5fyim42dn821nsd3afnmgscrzzhn3h8rag635d2jnr23r1zhk"))))
;    (build-system go-build-system)
;    (arguments '(#:import-path "github.com/golang/protobuf"))
;    (propagated-inputs
;      `(;("go-google-golang-org-protobuf" ,go-google-golang-org-protobuf)
;        ("go-github-com-google-go-cmp" ,go-github-com-google-go-cmp)))
;    (home-page "https://github.com/golang/protobuf")
;    (synopsis "Go support for Protocol Buffers")
;    (description
;      "This module (@url{https://pkg.go.dev/mod/github.com/golang/protobuf,(code
;github.com/golang/protobuf)}) contains Go bindings for protocol buffers.")
;    (license license:bsd-3)))

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

(define-public go-github-com-go-ldap-ldap-v3
  (deprecated-package "go-github-com-go-ldap-ldap-v3" go-github-com-go-ldap-ldap))
  ;(package
  ;  (name "go-github-com-go-ldap-ldap-v3")
  ;  (version "3.4.1")
  ;  (source
  ;    (origin
  ;      (method git-fetch)
  ;      (uri (git-reference
  ;             (url "https://github.com/go-ldap/ldap")
  ;             (commit (string-append "v" version))))
  ;      (file-name (git-file-name name version))
  ;      (sha256
  ;        (base32 "1xf2jrwhgr06jy4liba48hrz4b7j27r7m9dnl7fj95vazsx2n5br"))))
  ;  (build-system go-build-system)
  ;  (arguments
  ;   '(#:tests? #f      ; Tests require network access.
  ;     #:import-path "github.com/go-ldap/ldap/v3"))
  ;  (propagated-inputs
  ;   (list go-golang-org-x-crypto
  ;         go-github-com-go-asn1-ber-asn1-ber
  ;         go-github-com-azure-go-ntlmssp))
  ;  (home-page "https://github.com/go-ldap/ldap")
  ;  (synopsis #f)
  ;  (description "Package ldap provides basic LDAP v3 functionality.")
  ;  (license license:expat)))

(define-public go-github-com-go-logfmt-logfmt
  (package
    (name "go-github-com-go-logfmt-logfmt")
    (version "0.5.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/go-logfmt/logfmt")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "01fs4x2aqw2qcsz18s4nfvyqv3rcwz5xmgpk3bic6nzgyzsjd7dp"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/go-logfmt/logfmt"))
    (home-page "https://github.com/go-logfmt/logfmt")
    (synopsis "logfmt")
    (description
      "Package logfmt implements utilities to marshal and unmarshal data in the logfmt
format.  The logfmt format records key/value pairs in a way that balances
readability for humans and simplicity of computer parsing.  It is most commonly
used as a more human friendly alternative to JSON for structured logging.")
    (license license:expat)))

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

(define-public go-github-com-google-go-cmp
  (deprecated-package "go-github-com-google-go-cmp" go-github-com-google-go-cmp-cmp))

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

(define-public go-github-com-go-openapi-analysis
  (package
    (name "go-github-com-go-openapi-analysis")
    (version "0.21.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/go-openapi/analysis")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1plnyj0lmjn2x3r4isvc1rr957i7i4xkc8mmf5mrrc102g6dpfzn"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/go-openapi/analysis"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-network-tests
           (lambda _
             (substitute* "src/github.com/go-openapi/analysis/flatten_test.go"
               (("TestFlatten_RemoteAbsolute") "DisableTestFlatten_RemoteAbsolute")))))))
    (propagated-inputs
     (list go-gopkg-in-yaml-v3
           go-github-com-mitchellh-mapstructure
           go-github-com-go-openapi-swag
           go-github-com-go-openapi-strfmt
           go-github-com-go-openapi-spec
           go-github-com-go-openapi-jsonpointer
           go-github-com-go-openapi-errors))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/go-openapi/analysis")
    (synopsis "OpenAPI initiative analysis")
    (description
      "Package analysis provides methods to work with a Swagger specification document
from package go-openapi/spec.")
    (license license:asl2.0)))

(define-public go-github-com-go-openapi-errors
  ;; Fixes test error related to a previous api change.
  (let ((commit "b45e39dc92fa5448ebf1564c0f4e680f2067cdf5")
        (revision "1"))
    (package
      (name "go-github-com-go-openapi-errors")
      (version (git-version "0.20.1" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/go-openapi/errors")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32 "0c669p3g0bfcmnmxvc382j5amvah9m5psp59rmgyp6gryci0pxwy"))))
      (build-system go-build-system)
      (arguments '(#:import-path "github.com/go-openapi/errors"))
      (propagated-inputs
        `(;("go-gopkg-in-yaml-v3" ,go-gopkg-in-yaml-v3)
          ;("go-gopkg-in-check-v1" ,go-gopkg-in-check-v1)
          ;("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)
          ;("go-github-com-niemeyer-pretty" ,go-github-com-niemeyer-pretty)
          ;("go-github-com-kr-text" ,go-github-com-kr-text)
          ;("go-github-com-davecgh-go-spew" ,go-github-com-davecgh-go-spew)
          ))
      (native-inputs
       (list go-github-com-stretchr-testify))
      (home-page "https://github.com/go-openapi/errors")
      (synopsis "OpenAPI errors")
      (description
        "Package errors provides an Error interface and several concrete types
implementing this interface to manage API errors and JSON-schema validation
errors.")
      (license license:asl2.0))))

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

(define-public go-github-com-go-openapi-jsonpointer
  (package
    (name "go-github-com-go-openapi-jsonpointer")
    (version "0.19.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/go-openapi/jsonpointer")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0azic9nfwywlz4qxvacyi4g668fbbrkcyv15bag02yfcsi8szg5c"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/go-openapi/jsonpointer"))
    (propagated-inputs
     (list ;go-github-com-stretchr-testify
           ;go-github-com-mailru-easyjson
           go-github-com-go-openapi-swag))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/go-openapi/jsonpointer")
    (synopsis "gojsonpointer")
    (description
     "An implementation of JSON Pointer - Go language")
    (license license:asl2.0)))

(define-public go-github-com-go-openapi-jsonreference
  (package
    (name "go-github-com-go-openapi-jsonreference")
    (version "0.19.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/go-openapi/jsonreference")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1b4xg5ghzlzd6ricc3jwmlp71hxz1mgg54mgywzzqwyzvpbwn7n9"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/go-openapi/jsonreference"))
    (propagated-inputs
     (list ;go-golang-org-x-net
        go-github-com-go-openapi-jsonpointer
        ;go-github-com-puerkitobio-urlesc
        go-github-com-puerkitobio-purell))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/go-openapi/jsonreference")
    (synopsis "gojsonreference")
    (description
      "An implementation of JSON Reference - Go language")
    (license license:asl2.0)))

(define-public go-github-com-go-openapi-loads
  (package
    (name "go-github-com-go-openapi-loads")
    (version "0.21.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/go-openapi/loads")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0n2bwrc00dn5a4vyni60h820qv9w2r1jpy8g1d4n0l5z8cfdmmp2"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f      ; Test requirements introduce circular dependencies.
       #:import-path "github.com/go-openapi/loads"))
    (propagated-inputs
     (list ;go-gopkg-in-yaml-v2
        go-github-com-go-openapi-swag
        go-github-com-go-openapi-strfmt
        go-github-com-go-openapi-spec
        go-github-com-go-openapi-analysis))
    ;(native-inputs
    ; (list go-github-com-go-openapi-validate
    ;       go-github-com-stretchr-testify))
    (home-page "https://github.com/go-openapi/loads")
    (synopsis "Loads OAI specs")
    (description
      "Package loads provides document loading methods for swagger (OAI)
specifications.")
    (license license:asl2.0)))

(define-public go-github-com-go-openapi-runtime
  (package
    (name "go-github-com-go-openapi-runtime")
    (version "0.21.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/go-openapi/runtime")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0kb7n45a8b3h2fxf50wmw2bpp4zlz830gx44jvr97qhrbv6m7hv2"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/go-openapi/runtime"))
    (propagated-inputs
     (list ;go-gopkg-in-yaml-v2
        ;go-github-com-opentracing-opentracing-go
        ;go-github-com-go-openapi-validate
        go-github-com-go-openapi-swag
        go-github-com-go-openapi-strfmt
        ;go-github-com-go-openapi-spec
        ;go-github-com-go-openapi-loads
        go-github-com-go-openapi-errors
        ;go-github-com-go-openapi-analysis
        ;go-github-com-docker-go-units
        ))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/go-openapi/runtime")
    (synopsis "runtime")
    (description
      "The runtime component for use in codegeneration or as untyped usage.")
    (license license:asl2.0)))

(define-public go-github-com-go-openapi-spec
  (package
    (name "go-github-com-go-openapi-spec")
    (version "0.20.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/go-openapi/spec")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1gc92i6y9rcnzx1pz7q0k3s16pgpgmzgjyqvqzlyrds59jxc165s"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/go-openapi/spec"))
    (propagated-inputs
     (list ;go-gopkg-in-yaml-v2
        ;go-golang-org-x-text
        go-github-com-go-openapi-swag
        go-github-com-go-openapi-jsonreference
        go-github-com-go-openapi-jsonpointer))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/go-openapi/spec")
    (synopsis "OAI object model")
    (description "The object model for OpenAPI specification documents.")
    (license license:asl2.0)))

(define-public go-github-com-go-openapi-strfmt
  (package
    (name "go-github-com-go-openapi-strfmt")
    (version "0.21.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/go-openapi/strfmt")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0vq9ri0ycy345nygswwsy8y0xyf1alcqjwb8s5kllnvjv8z1rch9"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/go-openapi/strfmt"))
    (propagated-inputs
     (list go-go-mongodb-org-mongo-driver
           go-github-com-oklog-ulid
           go-github-com-mitchellh-mapstructure
           go-github-com-google-uuid
           go-github-com-go-openapi-errors
           go-github-com-asaskevich-govalidator))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/go-openapi/strfmt")
    (synopsis "Strfmt")
    (description "Package strfmt contains custom string formats")
    (license license:asl2.0)))

(define-public go-github-com-go-openapi-swag
  (package
    (name "go-github-com-go-openapi-swag")
    (version "0.19.15")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/go-openapi/swag")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0fyz1wclcc776qwh6qnnajgfa3l8rwp3y6dzyrfb8cfnc3j0jqim"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f      ; Test suite not ready for go-1.17+
       #:import-path "github.com/go-openapi/swag"))
    (propagated-inputs
     (list ;go-github-com-sourcegraph-go-diff
        ;go-golang-org-x-lint
        ;go-gopkg-in-yaml-v3
        go-gopkg-in-yaml-v2
        ;go-gopkg-in-check-v1
        ;go-github-com-niemeyer-pretty
        go-github-com-mailru-easyjson
        ;go-github-com-kr-text
        ;go-github-com-davecgh-go-spew
        ))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/go-openapi/swag")
    (synopsis "Swag")
    (description
      "Package swag contains a bunch of helper functions for go-openapi and go-swagger
projects.")
    (license license:asl2.0)))

(define-public go-github-com-go-openapi-validate
  (package
    (name "go-github-com-go-openapi-validate")
    (version "0.20.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/go-openapi/validate")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1arhy8ri87pnckqxvv8ny824jgnvga24ihz097112nbfxvgr3gml"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/go-openapi/validate"))
    (propagated-inputs
     (list ;go-gopkg-in-yaml-v3
        ;go-gopkg-in-yaml-v2
        go-github-com-go-openapi-swag
        go-github-com-go-openapi-strfmt
        go-github-com-go-openapi-spec
        go-github-com-go-openapi-runtime
        go-github-com-go-openapi-loads
        go-github-com-go-openapi-jsonpointer
        go-github-com-go-openapi-errors
        go-github-com-go-openapi-analysis))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/go-openapi/validate")
    (synopsis "Validation helpers")
    (description
     "Package validate provides methods to validate a swagger specification, as well
as tools to validate data against their schema.")
    (license license:asl2.0)))

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
           go-github-com-cespare-xxhash-v2))
    (home-page "https://github.com/go-redis/redis")
    (synopsis "Redis client for Golang")
    (description "Package redis implements a Redis client.")
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

(define-public go-github-com-gorilla-schema
  (package
    (name "go-github-com-gorilla-schema")
    (version "1.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/gorilla/schema")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0lbpncv6p7xqf1rb52b6rlxsib6l795bzsqy4hh6012c7dhl6hvw"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/gorilla/schema"))
    (home-page "https://github.com/gorilla/schema")
    (synopsis "schema")
    (description #f)
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

;; ready to upstream
(define-public go-github-com-go-stack-stack
  (package
    (name "go-github-com-go-stack-stack")
    (version "1.8.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/go-stack/stack")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "01m6l9w84yq2yyly8bdfsgc386hla1gn9431c7vr3mfa3bchj5wb"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/go-stack/stack"))
    (home-page "https://github.com/go-stack/stack")
    (synopsis "Utilities to capture, manipulate, and format call stacks")
    (description
     "Stack implements utilities to capture, manipulate, and format call stacks.
It provides a simpler API than package @code{runtime}.")
    (license license:expat)))

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
           go-gopkg-in-square-go-jose-v2
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
           go-github.com-jessevdk-go-flags
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

(define-public go-github-com-go-task-slim-sprig
  (package
    (name "go-github-com-go-task-slim-sprig")
    (version "2.20.0+incompatible")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/go-task/slim-sprig")
               (commit (string-append "v" (go-version->git-ref version)))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0p14axjkiznjrhl7gbmlc1fliq125xkckn1y9vy2jalslzrgprvv"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/go-task/slim-sprig"))
    (propagated-inputs
     (list go-github-com-masterminds-goutils
           go-github-com-masterminds-semver
           go-github-com-google-uuid
           go-github-com-huandu-xstrings
           go-github-com-imdario-mergo
           go-golang-org-x-crypto))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/go-task/slim-sprig")
    (synopsis "Sprig: Template functions for Go templates")
    (description "Sprig: Template functions for Go.")
    (license license:expat)))

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
        ;("go-github-com-rogpeppe-fastuuid" ,go-github-com-rogpeppe-fastuuid)
        ;("go-github-com-golang-protobuf" ,go-github-com-golang-protobuf)
        ;("go-github-com-golang-glog" ,go-github-com-golang-glog)
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

(define-public go-github-com-hashicorp-go-cleanhttp
  (package
    (name "go-github-com-hashicorp-go-cleanhttp")
    (version "0.5.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/hashicorp/go-cleanhttp")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1i5xslizzwd966w81bz6dxjwzgml4q9bwqa186bsxd1vi8lqxl9p"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/hashicorp/go-cleanhttp"))
    (home-page "https://github.com/hashicorp/go-cleanhttp")
    (synopsis "cleanhttp")
    (description
      "Package cleanhttp offers convenience utilities for acquiring \"clean\"
http.Transport and http.Client structs.")
    (license license:mpl2.0)))

(define-public go-github-com-hashicorp-go-hclog
  (package
    (name "go-github-com-hashicorp-go-hclog")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/hashicorp/go-hclog")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1bhpqrjjfsr97wkr8dkwzxsvfvxbbmwq6z4cfpgq7zaccda76n9r"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/hashicorp/go-hclog"))
    (propagated-inputs
     (list go-github-com-pmezard-go-difflib
           go-github-com-mattn-go-isatty
           go-github-com-mattn-go-colorable
           go-github-com-fatih-color
           go-github-com-davecgh-go-spew))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/hashicorp/go-hclog")
    (synopsis "go-hclog")
    (description
      "@code{go-hclog} is a package for Go that provides a simple key/value logging
interface for use in development and production environments.")
    (license license:expat)))

(define-public go-github-com-hashicorp-golang-lru
  (package
    (name "go-github-com-hashicorp-golang-lru")
    (version "0.5.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/hashicorp/golang-lru")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1sdbymypp9vrnzp8ashw0idlxvaq0rb0alwxx3x8g27yjlqi9jfn"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/hashicorp/golang-lru"))
    (home-page "https://github.com/hashicorp/golang-lru")
    (synopsis "golang-lru")
    (description
     "Package lru provides three different LRU caches of varying sophistication.")
    (license license:mpl2.0)))

(define-public go-github-com-hashicorp-go-retryablehttp
  (package
    (name "go-github-com-hashicorp-go-retryablehttp")
    (version "0.7.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/hashicorp/go-retryablehttp")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1pq6a7qyb4yvbvbpkqw8qq2qnk3i3mfalfg61g4lnddf33wad8yl"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f      ; Tests require network access
       #:import-path "github.com/hashicorp/go-retryablehttp"))
    (propagated-inputs
     (list go-github-com-hashicorp-go-hclog
           go-github-com-hashicorp-go-cleanhttp))
    (home-page "https://github.com/hashicorp/go-retryablehttp")
    (synopsis "go-retryablehttp")
    (description
      "Package retryablehttp provides a familiar HTTP client interface with automatic
retries and exponential backoff.  It is a thin wrapper over the standard
net/http client library and exposes nearly the same public API.  This makes
retryablehttp very easy to drop into existing programs.")
    (license license:mpl2.0)))

;; update in (gnu packages golang)
(define-public go-github-com-hashicorp-go-version-1.3.0
  (package
    (inherit go-github-com-hashicorp-go-version)
    (name "go-github-com-hashicorp-go-version")
    (version "1.3.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/hashicorp/go-version")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "15ygnddqh4wq1l866cicc9cmbcqvym16ai8dj71i0wqyknnfxr3v"))))
    (arguments '(#:import-path "github.com/hashicorp/go-version"))))

(define-public go-github-com-iancoleman-strcase
  (package
    (name "go-github-com-iancoleman-strcase")
    (version "0.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/iancoleman/strcase")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0rgfn6zz1r9h7yic3b0dcqq900bi638d6qgcyy9jhvk00f4dlg5j"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/iancoleman/strcase"))
    (home-page "https://github.com/iancoleman/strcase")
    (synopsis "strcase")
    (description
      "Package strcase converts strings to various cases.  See the conversion table
below:")
    (license license:expat)))

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

(define-public go-github-com-jackc-chunkreader-v2
  (package
    (name "go-github-com-jackc-chunkreader-v2")
    (version "2.0.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jackc/chunkreader")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0fj585hp3s4cjfzncr5gmim96p0b956pqdf4nm7yan1ipfch9l1c"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/jackc/chunkreader/v2"))
    (home-page "https://github.com/jackc/chunkreader")
    (synopsis "chunkreader")
    (description
      "Package chunkreader provides an @code{io.Reader} wrapper that minimizes IO reads and
memory allocations.")
    (license license:expat)))

(define-public go-github-com-jackc-pgconn
  (package
    (name "go-github-com-jackc-pgconn")
    (version "1.10.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jackc/pgconn")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "06ibz1yjjw4vfzlw32rf1i8pv898vaa0awwnxmk7pvf38ss7gwib"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/jackc/pgconn"))
    (propagated-inputs
     (list go-golang-org-x-text
           go-golang-org-x-crypto
           go-github-com-jackc-pgservicefile
           go-github-com-jackc-pgproto3-v2
           go-github-com-jackc-pgpassfile
           go-github-com-jackc-pgmock
           go-github-com-jackc-pgio
           go-github-com-jackc-chunkreader-v2))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/jackc/pgconn")
    (synopsis "pgconn")
    (description "Package pgconn is a low-level PostgreSQL database driver.")
    (license license:expat)))

(define-public go-github-com-jackc-pgio
  (package
    (name "go-github-com-jackc-pgio")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jackc/pgio")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0l17gpn11wf6jm5kbfmxh8j00n5zpmwck3wr91f1cv34k4chyvg1"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/jackc/pgio"))
    (home-page "https://github.com/jackc/pgio")
    (synopsis "pgio")
    (description
      "Package pgio is a low-level toolkit building messages in the PostgreSQL wire
protocol.")
    (license license:expat)))

(define-public go-github-com-jackc-pgmock
  (package
    (name "go-github-com-jackc-pgmock")
    (version "0.0.0-20210724152146-4ad1a8207f65")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jackc/pgmock")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "189hp5fkvavwgg7z0z9b9xj88ypsphvb7s4dpwa5aj42jm39nqha"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f      ; Circular imports
       #:import-path "github.com/jackc/pgmock"))
    (propagated-inputs
     (list go-github-com-jackc-pgproto3-v2))
    ;(native-inputs
    ; (list go-github-com-jackc-pgconn))
    (home-page "https://github.com/jackc/pgmock")
    (synopsis "pgmock")
    (description
      "Package pgmock provides the ability to mock a PostgreSQL server.")
    (license license:expat)))

(define-public go-github-com-jackc-pgpassfile
  (package
    (name "go-github-com-jackc-pgpassfile")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jackc/pgpassfile")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1crw06lzksgimbmr1a3sr00azg2v7l4qkvjra1cpmzzq5mncaj8z"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/jackc/pgpassfile"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/jackc/pgpassfile")
    (synopsis "pgpassfile")
    (description "Package pgpassfile is a parser PostgreSQL .pgpass files.")
    (license license:expat)))

(define-public go-github-com-jackc-pgproto3-v2
  (package
    (name "go-github-com-jackc-pgproto3-v2")
    (version "2.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jackc/pgproto3")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1qm4k96pbvx52axlnyc7xic18r7n5hi3l50bf2qsy5cgz850mcvz"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/jackc/pgproto3/v2"))
    (propagated-inputs
     (list go-github-com-jackc-pgio
           go-github-com-jackc-chunkreader-v2))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/jackc/pgproto3")
    (synopsis "pgproto3")
    (description
      "Package pgproto3 is a encoder and decoder of the PostgreSQL wire protocol
version 3.")
    (license license:expat)))

(define-public go-github-com-jackc-pgservicefile
  (package
    (name "go-github-com-jackc-pgservicefile")
    (version "0.0.0-20200714003250-2b9c44734f2b")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jackc/pgservicefile")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "13gbi0ad58rm3rcgj8bssc7hgrqwva0q015fw57vx5cxb4rcrmxh"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/jackc/pgservicefile"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/jackc/pgservicefile")
    (synopsis "pgservicefile")
    (description
      "Package pgservicefile is a parser for PostgreSQL service files (e.g.
.pg_service.conf).")
    (license license:expat)))

(define-public go-github-com-jackc-pgtype
  (package
    (name "go-github-com-jackc-pgtype")
    (version "1.9.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jackc/pgtype")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1ajxmrh2ipf6gm6id0hnc2ijqdwv00m66jnrs5cr3jshj12m60r0"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/jackc/pgtype"))
    (propagated-inputs
     (list go-github-com-shopspring-decimal
           go-github-com-lib-pq
           go-github-com-jackc-pgx-v4
           go-github-com-jackc-pgio
           go-github-com-jackc-pgconn
           go-github-com-gofrs-uuid))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/jackc/pgtype")
    (synopsis "pgtype")
    (description
      "pgtype implements Go types for over 70 PostgreSQL types.  pgtype is the type
system underlying the
@url{https://github.com/jackc/pgx,https://github.com/jackc/pgx} PostgreSQL
driver.  These types support the binary format for enhanced performance with
pgx.  They also support the database/sql @code{Scan} and @code{Value} interfaces
and can be used with @url{https://github.com/lib/pq,https://github.com/lib/pq}.")
    (license license:expat)))

(define-public go-github-com-jackc-pgx-v4
  (package
    (name "go-github-com-jackc-pgx-v4")
    (version "4.14.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jackc/pgx")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1waxm7mvj5n2141y8cxgrq4bia7nydwc0a7xq8r48va6vqpcja3c"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/jackc/pgx/v4"))
    (propagated-inputs
      `(;("go-gopkg-in-inconshreveable-log15-v2" ,go-gopkg-in-inconshreveable-log15-v2)
        ;("go-go-uber-org-zap" ,go-go-uber-org-zap)
        ;("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)
        ;("go-github-com-sirupsen-logrus" ,go-github-com-sirupsen-logrus)
        ;("go-github-com-shopspring-decimal" ,go-github-com-shopspring-decimal)
        ;("go-github-com-rs-zerolog" ,go-github-com-rs-zerolog)
        ;("go-github-com-jackc-puddle" ,go-github-com-jackc-puddle)
        ;("go-github-com-jackc-pgtype" ,go-github-com-jackc-pgtype)
        ;("go-github-com-jackc-pgproto3-v2" ,go-github-com-jackc-pgproto3-v2)
        ;("go-github-com-jackc-pgio" ,go-github-com-jackc-pgio)
        ;("go-github-com-jackc-pgconn" ,go-github-com-jackc-pgconn)
        ;("go-github-com-gofrs-uuid" ,go-github-com-gofrs-uuid)
        ;("go-github-com-go-kit-log" ,go-github-com-go-kit-log)
        ;("go-github-com-cockroachdb-apd" ,go-github-com-cockroachdb-apd)
        ;("go-github-com-masterminds-semver-v3" ,go-github-com-masterminds-semver-v3)
        ))
    (home-page "https://github.com/jackc/pgx")
    (synopsis "pgx - PostgreSQL Driver and Toolkit")
    (description "Package pgx is a PostgreSQL database driver.")
    (license license:expat)))

(define-public go-github-com-jackc-puddle
  (package
    (name "go-github-com-jackc-puddle")
    (version "1.2.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jackc/puddle")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1avwqiwn1pri262vscpy686li8cadm6avxqgypj9hylgdyibj33z"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/jackc/puddle"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/jackc/puddle")
    (synopsis "Puddle")
    (description "Package puddle is a generic resource pool.")
    (license license:expat)))

(define-public go-github-com-jarcoal-httpmock
  (package
    (name "go-github-com-jarcoal-httpmock")
    (version "1.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jarcoal/httpmock")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1nvlmr2skpxinvwd5gvc68k9saix8iajq136mwcfqmk6dl25hf4l"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/jarcoal/httpmock"))
    (home-page "https://github.com/jarcoal/httpmock")
    (synopsis "httpmock")
    (description #f)
    (license license:expat)))

(define-public go-github-com-jaytaylor-html2text
  (package
    (name "go-github-com-jaytaylor-html2text")
    (version "0.0.0-20211013000000-90c08c5027d3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jaytaylor/html2text")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1zxb2ym0rz087hwzcgd6ja717930k2clhs7995yvbspncswidmgb"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/jaytaylor/html2text"))
    (propagated-inputs
     (list go-github-com-olekukonko-tablewriter
           go-github-com-ssor-bom
           go-golang-org-x-net))
    (home-page "https://github.com/jaytaylor/html2text")
    (synopsis "html2text")
    (description "Ensure your emails are readable by all!")
    (license license:expat)))

(define-public go-github-com-joho-godotenv
  (package
    (name "go-github-com-joho-godotenv")
    (version "1.4.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/joho/godotenv")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1036h59vyhb58n817az6yg0zw5wa87yb86i7fnbdq8cw46mnjgw8"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/joho/godotenv"))
    (home-page "https://github.com/joho/godotenv")
    (synopsis "GoDotEnv")
    (description
      "Package godotenv is a go port of the ruby dotenv library
(@url{https://github.com/bkeepers/dotenv,https://github.com/bkeepers/dotenv})")
    (license license:expat)))

(define-public go-github-com-josharian-intern
  (package
    (name "go-github-com-josharian-intern")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/josharian/intern")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1za48ppvwd5vg8vv25ldmwz1biwpb3p6qhf8vazhsfdg9m07951c"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/josharian/intern"))
    (home-page "https://github.com/josharian/intern")
    (synopsis #f)
    (description
      "Package intern interns strings.  Interning is best effort only.  Interned
strings may be removed automatically at any time without notification.  All
functions may be called concurrently with themselves and each other.")
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

(define-public go-github-com-json-iterator-go
  (package
    (name "go-github-com-json-iterator-go")
    (version "1.1.12")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/json-iterator/go")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1c8f0hxm18wivx31bs615x3vxs2j3ba0v6vxchsjhldc8kl311bz"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/json-iterator/go"))
    (propagated-inputs
     (list go-github-com-modern-go-reflect2
           go-github-com-modern-go-concurrent
           go-github-com-google-gofuzz
           go-github-com-davecgh-go-spew))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/json-iterator/go")
    (synopsis "Benchmark")
    (description
      "Package jsoniter implements encoding and decoding of JSON as defined in
@url{https://rfc-editor.org/rfc/rfc4627.html,RFC 4627} and provides interfaces
with identical syntax of standard lib encoding/json.  Converting from
encoding/json to jsoniter is no more than replacing the package with jsoniter
and variable type declarations (if any).  jsoniter interfaces gives 100%
compatibility with code using standard lib.")
    (license license:expat)))

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

(define-public go-github-com-klauspost-cpuid-v2
  (package
    (name "go-github-com-klauspost-cpuid-v2")
    (version "2.0.9")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/klauspost/cpuid")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "12bx0kip3yv2416f4ilafk5674m69mn873gnadhyv473cy4jy499"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/klauspost/cpuid/v2"))
    (home-page "https://github.com/klauspost/cpuid")
    (synopsis "cpuid")
    (description
      "Package cpuid provides information about the CPU running the current program.")
    (license license:expat)))

(define-public go-github-com-klauspost-pgzip
  (package
    (name "go-github-com-klauspost-pgzip")
    (version "1.2.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/klauspost/pgzip")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0m66jcsz27076qvi5qzagzlbyd1sdzh6kbf1njj0sswx86026rx3"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/klauspost/pgzip"))
    (propagated-inputs
     (list go-github-com-klauspost-compress))
    (home-page "https://github.com/klauspost/pgzip")
    (synopsis "pgzip")
    (description
     "Package pgzip implements reading and writing of gzip format compressed files, as
specified in @url{https://rfc-editor.org/rfc/rfc1952.html,RFC 1952}.")
    (license license:expat)))

(define-public go-github-com-kljensen-snowball
  (package
    (name "go-github-com-kljensen-snowball")
    (version "0.6.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/kljensen/snowball")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1frx5m042zgys7fkv6wj4pcg2ba63486v82pv4vlqaw9c3bxgibv"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/kljensen/snowball"))
    (home-page "https://github.com/kljensen/snowball")
    (synopsis "Snowball")
    (description
     "This package provides a @url{http://golang.org,Go (golang)} implementation of
the @url{http://snowball.tartarus.org/,Snowball stemmer} for natural language
processing.")
    (license license:expat)))

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
      "Package backoff implments backoff algorithms for retrying operations.")
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
           ;go-github-com-decred-dcrd-dcrec-secp256k1-v4)     ; TODO: import
           ))
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

(define-public go-github-com-libdns-libdns
  (package
    (name "go-github-com-libdns-libdns")
    (version "0.2.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/libdns/libdns")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1ix668h4n2n9iph4xiznzdfw7hy0ijy906mvnys4kq9f0v9ih4bg"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/libdns/libdns"))
    (home-page "https://github.com/libdns/libdns")
    (synopsis "libdns - Universal DNS provider APIs for Go")
    (description
      "Package libdns defines core interfaces that should be implemented by DNS
provider clients.  They are small and idiomatic Go interfaces with well-defined
semantics.")
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

(define-public go-github-com-mailru-easyjson
  (package
    (name "go-github-com-mailru-easyjson")
    (version "0.7.7")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/mailru/easyjson")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0clifkvvy8f45rv3cdyv58dglzagyvfcqb63wl6rij30c5j2pzc1"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/mailru/easyjson"))
    (propagated-inputs
     (list go-github-com-josharian-intern))
    (home-page "https://github.com/mailru/easyjson")
    (synopsis "easyjson")
    (description
      "Package easyjson contains marshaler/unmarshaler interfaces and helper functions.")
    (license license:expat)))

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

(define-public go-github-com-masterminds-semver-v3
  (package
    (name "go-github-com-masterminds-semver-v3")
    (version "3.1.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/Masterminds/semver")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0dsqa585ixz6pbff60p0pk709kp3kksh668mjwrlxgqiammxa1p8"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/Masterminds/semver/v3"))
    (home-page "https://github.com/Masterminds/semver")
    (synopsis "SemVer")
    (description
      "Package semver provides the ability to work with Semantic Versions
(@url{http://semver.org,http://semver.org}) in Go.")
    (license license:expat)))

(define-public go-github-com-matryer-is
  (package
    (name "go-github-com-matryer-is")
    (version "1.4.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/matryer/is")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0bs452ba2qh2a9ir7r8ixpf2slc6vpyg0041v9kn5c0d1zbb5wsc"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/matryer/is"))
    (home-page "https://github.com/matryer/is")
    (synopsis "is")
    (description
      "Package is provides a lightweight extension to the standard library's testing
capabilities.")
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

(define-public go-github-com-mholt-acmez
  (package
    (name "go-github-com-mholt-acmez")
    (version "1.0.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/mholt/acmez")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1lylggivc4l4qlrl81lz7f7z3s893pzxplpkq8dmciywj8pwx4bw"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/mholt/acmez"))
    (propagated-inputs
     (list go-golang-org-x-net
           go-golang-org-x-text
           go-go-uber-org-zap))
    (home-page "https://github.com/mholt/acmez")
    (synopsis "acmez - ACME client library for Go")
    (description
     "Package acmez implements the higher-level flow of the ACME specification,
@url{https://rfc-editor.org/rfc/rfc8555.html,RFC 8555}:
@url{https://tools.ietf.org/html/rfc8555,https://tools.ietf.org/html/rfc8555},
specifically the sequence in Section 7.1 (page 21).")
    (license license:asl2.0)))

(define-public go-github-com-mholt-archiver-v3
  (package
    (name "go-github-com-mholt-archiver-v3")
    (version "3.5.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/mholt/archiver")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1py186hfy4p69wghqmbsyi1r3xvw1nyl55pz8f97a5qhmwxb3mwp"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/mholt/archiver/v3"))
    (propagated-inputs
     (list go-github-com-xi2-xz
           go-github-com-ulikunitz-xz
           go-github-com-pierrec-lz4-v4
           go-github-com-nwaples-rardecode
           go-github-com-klauspost-pgzip
           go-github-com-klauspost-compress
           go-github-com-golang-snappy
           go-github-com-dsnet-compress
           go-github-com-andybalholm-brotli))
    (home-page "https://github.com/mholt/archiver")
    (synopsis "archiver")
    (description
      "Package archiver facilitates convenient, cross-platform, high-level archival and
compression operations for a variety of formats and compression algorithms.")
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

(define-public go-github-com-miekg-dns
  (package
    (name "go-github-com-miekg-dns")
    (version "1.1.45")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/miekg/dns")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0qb19y97g5wrj5m5qzzswznrpwg64q5w979w618j423wn5d7iwqp"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/miekg/dns"))
    (propagated-inputs
     (list go-golang-org-x-tools
           go-golang-org-x-sys
           go-golang-org-x-sync
           go-golang-org-x-net))
    (home-page "https://github.com/miekg/dns")
    (synopsis "Alternative (more granular) approach to a DNS library")
    (description
      "Package dns implements a full featured interface to the Domain Name System.
Both server- and client-side programming is supported.  The package allows
complete control over what is sent out to the DNS.  The API follows the
less-is-more principle, by presenting a small, clean interface.")
    (license license:bsd-3)))

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

(define-public go-github-com-minio-minio-go-v7
  (package
    (name "go-github-com-minio-minio-go-v7")
    (version "7.0.20")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/minio/minio-go")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "082rsdnaqx36685h55lj4kl97x5cjaifjpdci0cg6bhbdl6isd1h"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/minio/minio-go/v7"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-network-tests
           (lambda _
             (substitute* "src/github.com/minio/minio-go/v7/core_test.go"
               (("TestGet") "DisabledTestGet")
               (("TestCore") "DisabledTestCore")))))))
    (propagated-inputs
     (list go-gopkg-in-yaml-v2
           go-golang-org-x-text
           go-golang-org-x-sys
           go-github-com-smartystreets-goconvey
           go-github-com-modern-go-reflect2
           go-github-com-modern-go-concurrent
           go-github-com-klauspost-cpuid
           go-gopkg-in-ini-v1
           go-golang-org-x-net
           go-golang-org-x-crypto
           go-github-com-sirupsen-logrus
           go-github-com-rs-xid
           go-github-com-mitchellh-go-homedir
           go-github-com-minio-sha256-simd
           go-github-com-minio-md5-simd
           go-github-com-klauspost-compress
           go-github-com-json-iterator-go
           go-github-com-google-uuid
           go-github-com-dustin-go-humanize))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/minio/minio-go")
    (synopsis "MinIO Go Client SDK for Amazon S3 Compatible Cloud Storage")
    (description
      "The MinIO Go Client SDK provides simple APIs to access any Amazon S3 compatible
object storage.")
    (license license:asl2.0)))

(define-public go-github-com-modern-go-concurrent
  (package
    (name "go-github-com-modern-go-concurrent")
    (version "0.0.0-20180306012644-bacd9c7ef1dd")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/modern-go/concurrent")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0s0fxccsyb8icjmiym5k7prcqx36hvgdwl588y0491gi18k5i4zs"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/modern-go/concurrent"))
    (home-page "https://github.com/modern-go/concurrent")
    (synopsis "concurrent")
    (description
      "because sync.Map is only available in go 1.9, we can use concurrent.Map to make
code portable")
    (license license:asl2.0)))

(define-public go-github-com-modern-go-reflect2
  (package
    (name "go-github-com-modern-go-reflect2")
    (version "1.0.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/modern-go/reflect2")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "05a89f9j4nj8v1bchfkv2sy8piz746ikj831ilbp54g8dqhl8vzr"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/modern-go/reflect2"))
    (home-page "https://github.com/modern-go/reflect2")
    (synopsis "reflect2")
    (description "reflect api that avoids runtime reflect.Value cost")
    (license license:asl2.0)))

;; ready to upstream
(define-public go-github-com-montanaflynn-stats
  (package
    (name "go-github-com-montanaflynn-stats")
    (version "0.6.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/montanaflynn/stats")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0r0ad2275saw79kgh3ywafii8f6rja2z6mzm9izs11k2lvkqpz6z"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/montanaflynn/stats"))
    (home-page "https://github.com/montanaflynn/stats")
    (synopsis "Golang statistics library")
    (description "Stats is a comprehensive Golang statistics library package
with no dependencies.")
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

(define-public go-github-com-niklasfasching-go-org
  (package
    (name "go-github-com-niklasfasching-go-org")
    (version "1.6.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/niklasfasching/go-org")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "04ml4w5ip7kzqj9fv92rzcqlxarbpxwm5vsv7238cdmrcqv3q96g"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/niklasfasching/go-org"))
    (propagated-inputs
     (list go-golang-org-x-net
           go-github-com-danwakefield-fnmatch
           go-github-com-dlclark-regexp2
           go-github-com-shurcool-sanitized-anchor-name
           go-github-com-russross-blackfriday
           go-github-com-pmezard-go-difflib
           ;go-github-com-chaseadamsio-goorgeous
           go-github-com-alecthomas-chroma))
    (home-page "https://github.com/niklasfasching/go-org")
    (synopsis #f)
    (description #f)
    (license license:expat)))

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

(define-public go-github-com-nxadm-tail
  (package
    (name "go-github-com-nxadm-tail")
    (version "1.4.8")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/nxadm/tail")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1j2gi485fhwdpmyzn42wk62103fclwbfywg42p275z1qv2bsz1rc"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (delete-file-recursively "vendor")))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/nxadm/tail"))
    (propagated-inputs
     (list go-gopkg-in-tomb-v1
           go-github-com-fsnotify-fsnotify))
    (home-page "https://github.com/nxadm/tail")
    (synopsis "tail functionality in Go")
    (description
      "nxadm/tail provides a Go library that emulates the features of the BSD `tail`
program.  The library comes with full support for truncation/move detection as
it is designed to work with log rotation tools.  The library works on all
operating systems supported by Go, including POSIX systems like Linux and *BSD,
and MS Windows.  Go 1.9 is the oldest compiler release supported.")
    (license license:expat)))

(define-public go-github-com-nytimes-gziphandler
  (package
    (name "go-github-com-nytimes-gziphandler")
    (version "1.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/nytimes/gziphandler")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0rhrjlw220hnymzfccm0yir3pc9dpj7h3gwzhzq2cbsb3hhsqvyy"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/NYTimes/gziphandler"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/NYTimes/gziphandler")
    (synopsis "Gzip Handler")
    (description
     "This is a tiny Go package which wraps HTTP handlers to transparently gzip the
response body, for clients which support it.  Although it's usually simpler to
leave that to a reverse proxy (like nginx or Varnish), this package is useful
when that's undesirable.")
    (license license:asl2.0)))

(define-public go-github-com-oklog-ulid
  (package
    (name "go-github-com-oklog-ulid")
    (version "1.3.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/oklog/ulid")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0hybwyid820n80axrk863k2py93hbqlq6hxhf84ppmz0qd0ys0gq"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (delete-file-recursively "vendor")))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/oklog/ulid"))
    (home-page "https://github.com/oklog/ulid")
    (synopsis "Universally Unique Lexicographically Sortable Identifier")
    (description "This package provides a Go port of
@url{https://github.com/alizain/ulid,alizain/ulid} with binary format
implemented.")
    (license license:asl2.0)))

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
        ;go-github-com-smartystreets-gunit
        ;go-github-com-smartystreets-go-aws-auth
        ;go-github-com-smartystreets-assertions
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

(define-public go-github-com-onsi-ginkgo
  (package
    (name "go-github-com-onsi-ginkgo")
    (version "1.16.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/onsi/ginkgo")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1hh6n7q92y0ai8k6rj2yzw6wwxikhyiyk4j92zgvf1zad0gmqqmz"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/onsi/ginkgo"))
    (propagated-inputs
     (list go-golang-org-x-tools
           go-golang-org-x-sys
           go-github-com-onsi-gomega
           go-github-com-nxadm-tail
           go-github-com-go-task-slim-sprig))
    (home-page "https://github.com/onsi/ginkgo")
    (synopsis "Ginkgo 2.0 Release Candidate is available!")
    (description "Ginkgo is a BDD-style testing framework for Golang")
    (license license:expat)))

(define-public go-github-com-onsi-gomega
  (package
    (name "go-github-com-onsi-gomega")
    (version "1.17.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/onsi/gomega")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "01lxf1ai4grd7akdgrc50rb2g2c5drrc067acndccxzxidi43grh"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/onsi/gomega"))
    (propagated-inputs
     (list go-gopkg-in-yaml-v2
           go-golang-org-x-net
           go-golang-org-x-text))
    (home-page "https://github.com/onsi/gomega")
    (synopsis ": a BDD Testing Framework for Golang")
    (description
      "Gomega is the Ginkgo BDD-style testing framework's preferred matcher library.")
    (license license:expat)))

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

(define-public go-github-com-philhofer-fwd
  (package
    (name "go-github-com-philhofer-fwd")
    (version "1.1.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/philhofer/fwd")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1z88ry83lf01mv69kd2jwbhngh24qwhhknj3l5jn2kz5nycq6bkx"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/philhofer/fwd"))
    (home-page "https://github.com/philhofer/fwd")
    (synopsis "fwd")
    (description
      "The `fwd` package provides a buffered reader and writer.  Each has methods that
help improve the encoding/decoding performance of some binary protocols.")
    (license license:expat)))

(define-public go-github-com-pierrec-lz4-v4
  (package
    (name "go-github-com-pierrec-lz4-v4")
    (version "4.1.12")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/pierrec/lz4")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "19xl67xbwfcd7bd85a3s1c1ybc3khvbihhfrskhbblmc3bi6cgdj"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/pierrec/lz4/v4"))
    (home-page "https://github.com/pierrec/lz4")
    (synopsis "lz4 : LZ4 compression in pure Go")
    (description
      "Package lz4 implements reading and writing lz4 compressed data.")
    (license license:bsd-3)))

;; ready to upstream
(define-public go-github-com-pquerna-cachecontrol
  (package
    (name "go-github-com-pquerna-cachecontrol")
    (version "0.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/pquerna/cachecontrol")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1ds4wgk6hm1sd6037dww5zm59syzs7vrdzh9q5x78s1lfrznsi3k"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/pquerna/cachecontrol"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/pquerna/cachecontrol")
    (synopsis "HTTP caching parser and interpretation")
    (description
     "Cachecontrol implements @url{http://tools.ietf.org/html/rfc7234, RFC 7234}
Hypertext Transfer Protocol (HTTP/1.1): Caching.  It does this by parsing the
@code{Cache-Control} and other headers, providing information about requests and
responses.")
    (license license:asl2.0)))

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

(define-public go-github-com-protonmail-go-crypto
  (package
    (name "go-github-com-protonmail-go-crypto")
    (version "0.0.0-20211221144345-a4f6767435ab")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ProtonMail/go-crypto")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "15bzf3abjy1s4mdqja574vxa70ap9inbfwfq4yz5jd5rb6dhin7j"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/ProtonMail/go-crypto"
       #:tests? #f      ; Not ready for go-1.17
       #:phases
       (modify-phases %standard-phases
         (delete 'build)
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; from .travis.yml
               (invoke "go" "test" "-short" "./...")
               (invoke "go" "test" "./..." "-run" "RandomizeFast" "-count=512")
               (invoke "go" "test" "./..." "-run" "RandomizeSlow" "-count=32")))))))
    (propagated-inputs
     (list go-golang-org-x-crypto))
    (native-inputs
     (list go-golang-org-x-net
           go-golang-org-x-sync
           go-golang-org-x-text
           go-golang-org-x-tools))
    (home-page "https://github.com/ProtonMail/go-crypto")
    (synopsis #f)
    (description
     "This module is backwards compatible with x/crypto/openpgp, so you can simply
replace all imports of @code{golang.org/x/crypto/openpgp} with
@code{github.com/ProtonMail/go-crypto/openpgp}.")
    (license license:bsd-3)))

;; ready to upstream
(define-public go-github-com-puerkitobio-purell
  (package
    (name "go-github-com-puerkitobio-purell")
    (version "1.1.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/PuerkitoBio/purell")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0c525frsxmalrn55hzzsxy17ng8avkd40ga0wxfw9haxsdjgqdqy"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/PuerkitoBio/purell"))
    (propagated-inputs
     (list go-github-com-puerkitobio-urlesc
           go-golang-org-x-net
           go-golang-org-x-text))
    (home-page "https://github.com/PuerkitoBio/purell")
    (synopsis "URL normalization in Go")
    (description "Purell is a tiny Go library to normalize URLs.")
    (license license:bsd-3)))

;; ready to upstream
(define-public go-github-com-puerkitobio-urlesc
  (package
    (name "go-github-com-puerkitobio-urlesc")
    (version "0.0.0-20170810143723-de5bf2ad4578")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/PuerkitoBio/urlesc")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0n0srpqwbaan1wrhh2b7ysz543pjs1xw2rghvqyffg9l0g8kzgcw"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/PuerkitoBio/urlesc"))
    (home-page "https://github.com/PuerkitoBio/urlesc")
    (synopsis "Proper URL escaping as per RFC3986")
    (description "Package urlesc implements query escaping as per
@url{https://rfc-editor.org/rfc/rfc3986.html,RFC 3986}.  It contains some parts
of the @code{net/url} package, modified so as to allow some reserved characters
incorrectly escaped by @code{net/url}.")
    (license license:bsd-3)))

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

(define-public go-github-com-remyoudompheng-bigfft
  (package
    (name "go-github-com-remyoudompheng-bigfft")
    (version "0.0.0-20200410134404-eec4a21b6bb0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/remyoudompheng/bigfft")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0x01k3caqmyjivfgydkhg2m4y823xlhnzknp0xdlapap0rn298yp"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/remyoudompheng/bigfft"))
    (home-page "https://github.com/remyoudompheng/bigfft")
    (synopsis #f)
    (description
      "Package bigfft implements multiplication of big.Int using FFT.")
    (license license:bsd-3)))

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

(define-public go-github-com-rogpeppe-fastuuid
  (package
    (name "go-github-com-rogpeppe-fastuuid")
    (version "1.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/rogpeppe/fastuuid")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "028acdg63zkxpjz3l639nlhki2l0canr2v5jglrmwa1wpjqcfff8"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/rogpeppe/fastuuid"))
    (home-page "https://github.com/rogpeppe/fastuuid")
    (synopsis "fastuuid")
    (description
      "Package fastuuid provides fast UUID generation of 192 bit universally unique
identifiers.")
    (license license:bsd-3)))

(define-public go-github-com-rs-xid
  (package
    (name "go-github-com-rs-xid")
    (version "1.3.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/rs/xid")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0w2hva6ymn16yn6zrwb6nx3kxaffva95w7gj2fwg0xx39fyfslbb"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/rs/xid"))
    (home-page "https://github.com/rs/xid")
    (synopsis "Globally Unique ID Generator")
    (description
      "Package xid is a globally unique id generator suited for web scale")
    (license license:expat)))

(define-public go-github-com-rs-zerolog
  (package
    (name "go-github-com-rs-zerolog")
    (version "1.26.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/rs/zerolog")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "027fwbaavn58h94053rzwv9y42jvil4jfdjppq10vjw0qq0q4q04"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/rs/zerolog"))
    (propagated-inputs
     (list go-golang-org-x-tools
           go-golang-org-x-crypto
           go-github-com-rs-xid
           go-github-com-pkg-errors
           go-github-com-coreos-go-systemd-v22))
    (home-page "https://github.com/rs/zerolog")
    (synopsis "Zero Allocation JSON Logger")
    (description
      "Package zerolog provides a lightweight logging library dedicated to JSON
logging.")
    (license license:expat)))

(define-public go-github-com-russross-blackfriday-v2
  (package
    (name "go-github-com-russross-blackfriday-v2")
    (version "2.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/russross/blackfriday")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0d1rg1drrfmabilqjjayklsz5d0n3hkf979sr3wsrw92bfbkivs7"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/russross/blackfriday/v2"))
    (home-page "https://github.com/russross/blackfriday")
    (synopsis "Blackfriday")
    (description "Package blackfriday is a markdown processor.")
    (license license:bsd-2)))

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

(define-public go-github-com-shopspring-decimal
  (package
    (name "go-github-com-shopspring-decimal")
    (version "1.3.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/shopspring/decimal")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1w1wjv2aqyqp22s8gc2nxp8gk4h0dxvp15xsn5lblghaqjcd239h"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/shopspring/decimal"))
    (home-page "https://github.com/shopspring/decimal")
    (synopsis "decimal")
    (description
      "Package decimal implements an arbitrary precision fixed-point decimal.")
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

(define-public go-github-com-shurcool-httpfs
  (package
    (name "go-github-com-shurcool-httpfs")
    (version "0.0.0-20190707220628-8d4bc4ba7749")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/shurcooL/httpfs")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0qjkbjnp86kjr7r0xjwp39blnk1ggkzy6zm3xphr5dpin4jkgfa1"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/shurcooL/httpfs"
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key import-path build-flags #:allow-other-keys)
             (for-each
               (lambda (directory)
                 ((assoc-ref %standard-phases 'build)
                  #:build-flags build-flags
                  #:import-path (string-append "github.com/shurcooL/httpfs/" directory)))
               (list "filter"
                     "html/vfstemplate"
                     ;"httputil"        ; Wants github.com/shurcooL/httpgzip
                     "path/vfspath"
                     "text/vfstemplate"
                     "union"
                     "vfsutil"))))
         (replace 'check
           (lambda* (#:key tests? import-path #:allow-other-keys)
             (for-each
               (lambda (directory)
                 ((assoc-ref %standard-phases 'check)
                  #:tests? tests?
                  #:import-path (string-append "github.com/shurcooL/httpfs/" directory)))
               (list "filter"
                     "html/vfstemplate"
                     ;"httputil"
                     "path/vfspath"
                     "text/vfstemplate"
                     "union"
                     "vfsutil")))))))
    (native-inputs
     (list go-golang-org-x-tools))
    (home-page "https://github.com/shurcooL/httpfs")
    (synopsis "httpfs")
    (description
     "Collection of Go packages for working with the
@url{https://godoc.org/net/http#FileSystem,(code http.FileSystem)} interface.")
    (license license:expat)))

(define-public go-github-com-shurcool-vfsgen
  (package
    (name "go-github-com-shurcool-vfsgen")
    (version "0.0.0-20200824052919-0d455de96546")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/shurcooL/vfsgen")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0md1vgaq95x1jmxpnsfv6s9xf3v8gqi7lcl7mkxpf6274rf1n2pk"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/shurcooL/vfsgen"))
    (propagated-inputs
     (list go-github-com-shurcool-httpfs))
    (native-inputs
     (list go-golang-org-x-tools))
    (home-page "https://github.com/shurcooL/vfsgen")
    (synopsis "vfsgen")
    (description
      "Package vfsgen takes an http.FileSystem (likely at `go generate` time) and
generates Go code that statically implements the provided http.FileSystem.")
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
                         (not (string-contains file "go/websocket"))    ; wants go-github-com-gorilla-websocket
                         (not (string-contains file "go/rpc"))          ; wants network access
                         (not (null?
                                (filter-map
                                  (lambda (test-entry)
                                    (not (string-contains test-entry file-name-separator-string)))
                                  (map (lambda (entry)
                                         (string-drop entry (1+ (string-length file))))
                                       files))))))))
                 #:directories? #t)))))))
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

(define-public go-github-com-smartystreets-assertions
  (package
    (name "go-github-com-smartystreets-assertions")
    (version "1.2.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/smartystreets/assertions")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1iyminxmipvddm0hz8v69is4mga6ghif5ilmfz9s0d9kwmirbcn0"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/smartystreets/assertions"))
    (home-page "https://github.com/smartystreets/assertions")
    (synopsis #f)
    (description
      "Package assertions contains the implementations for all assertions which are
referenced in goconvey's `convey` package
(github.com/smartystreets/goconvey/convey) and gunit
(github.com/smartystreets/gunit) for use with the So(...) method.  They can also
be used in traditional Go test functions and even in applications.")
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

(define-public go-github-com-smartystreets-goconvey
  (package
    (name "go-github-com-smartystreets-goconvey")
    (version "1.7.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/smartystreets/goconvey")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0gwr0d6gb0jzqam76xpan279r2dnifsnhr4px8l6a84bavslqgv1"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/smartystreets/goconvey"))
    (propagated-inputs
     (list go-golang-org-x-tools
           go-github-com-smartystreets-assertions
           go-github-com-jtolds-gls
           go-github-com-gopherjs-gopherjs))
    (home-page "https://github.com/smartystreets/goconvey")
    (synopsis "GoConvey is awesome Go testing")
    (description
      "This executable provides an HTTP server that watches for file system changes to
.go files within the working directory (and all nested go packages).  Navigating
to the configured host and port in a web browser will display the latest results
of running `go test` in each go package.")
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
     `(("go-gopkg-in-warnings" ,go-gopkg-in-warnings)))
    (native-inputs
     `(("go-github-com-pkg-errors" ,go-github-com-pkg-errors)))
    (home-page "https://github.com/src-d/gcfg")
    (synopsis #f)
    (description #f)
    (license license:bsd-3)))

(define-public go-github-com-ssor-bom
  (package
    (name "go-github-com-ssor-bom")
    (version "0.0.0-20170718123548-6386211fdfcf")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ssor/bom")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "09g5496ifwqxqclh2iw58plcwcz0sczlnxwqxzwmnl4shdl371ld"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/ssor/bom"))
    (home-page "https://github.com/ssor/bom")
    (synopsis "bom")
    (description "small tools for cleaning bom from byte array or reader")
    (license license:expat)))

(define (go-github-com-stellar-go-package suffix)
  (package
    (name (string-append "go-github-com-stellar-go-"
                         (string-replace-substring suffix "/" "-")))
    (version "0.0.0-20210402164147-6145dda1700f")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/stellar/go")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name "go-github-com-stellar-go" version))
        (sha256
         (base32
          "1fp2y9lbxi05k214q8zcp6fwmngld4qa7abrvg7www209xpxq4p1"))))
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
       ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)))
    (native-inputs
     `(("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))))

(define-public go-github-com-stellar-go-amount
  (package (inherit (go-github-com-stellar-go-package "amount"))
    (propagated-inputs
     `(("go-github-com-lib-pq" ,go-github-com-lib-pq)
       ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
       ("go-github-com-stellar-go-xdr-xdr3" ,go-github-com-stellar-go-xdr-xdr3)))))

(define-public go-github-com-stellar-go-clients-federation
  (package (inherit (go-github-com-stellar-go-package "clients/federation"))
    (propagated-inputs
     `(("go-github-com-asaskevich-govalidator" ,go-github-com-asaskevich-govalidator)
       ("go-github-com-burntsushi-toml" ,go-github-com-burntsushi-toml)
       ("go-github-com-go-chi-chi" ,go-github-com-go-chi-chi)
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
       ("go-github-com-stellar-go-xdr-xdr3" ,go-github-com-stellar-go-xdr-xdr3)))
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
(define-public go-github-com-tidwall-pretty
  (package
    (name "go-github-com-tidwall-pretty")
    (version "1.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/tidwall/pretty")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "11zi5hxb75yapgxq67r4lmv8n910iqmw7994ig1fy4gnr4d51i3s"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/tidwall/pretty"))
    (home-page "https://github.com/tidwall/pretty")
    (synopsis "JSON formatter in go")
    (description
     "Pretty is a Go package that provides methods for formatting JSON for human
readability, or to compact JSON for smaller payloads.")
    (license license:expat)))

(define-public go-github-com-tinylib-msgp
  (package
    (name "go-github-com-tinylib-msgp")
    (version "1.1.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/tinylib/msgp")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "04s5wkl0qiihl729d1sc10pxnqi0x4xdq6v2dbdgly4j910qsgdd"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/tinylib/msgp"))
    (propagated-inputs
     (list go-golang-org-x-tools
           go-github-com-philhofer-fwd))
    (home-page "https://github.com/tinylib/msgp")
    (synopsis "MessagePack Code Generator")
    (description
      "msgp is a code generation tool for creating methods to serialize and
de-serialize Go data structures to and from MessagePack.")
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

(define-public go-github-com-ugorji-go-codec
  (package
    (name "go-github-com-ugorji-go-codec")
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
    (arguments
     '(#:import-path "github.com/ugorji/go/codec"
       #:unpack-path "github.com/ugorji/go"))
    (home-page "https://github.com/ugorji/go")
    (synopsis "Package Documentation for github.com/ugorji/go/codec")
    (description
     "Package codec provides a High Performance, Feature-Rich Idiomatic Go 1.4+
codec/encoding library for binc, msgpack, cbor, json.")
    (license license:expat)))

(define-public go-github-com-ulikunitz-xz
  (package
    (name "go-github-com-ulikunitz-xz")
    (version "0.5.10")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ulikunitz/xz")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "07vynk0sh8i8g7x9p9x04dj8wylvxaf8ypbi43yvcv7j6zd63c72"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/ulikunitz/xz"))
    (home-page "https://github.com/ulikunitz/xz")
    (synopsis "Package xz")
    (description
      "Package xz supports the compression and decompression of xz files.  It supports
version 1.0.4 of the specification without the non-LZMA2 filters.  See
@url{http://tukaani.org/xz/xz-file-format-1.0.4.txt,http://tukaani.org/xz/xz-file-format-1.0.4.txt}")
    (license license:bsd-3)))

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
             go-github-com-smartystreets-assertions
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

(define-public go-github-com-xanzy-go-gitlab
  (package
    (name "go-github-com-xanzy-go-gitlab")
    (version "0.52.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/xanzy/go-gitlab")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1717154zrpcxc5s3vi1fckxqav82afgn1plwns206ilp9h07g1y6"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/xanzy/go-gitlab"))
    (propagated-inputs
     (list go-google-golang-org-appengine
           go-golang-org-x-time
           go-golang-org-x-sync
           go-golang-org-x-oauth2
           go-golang-org-x-net
           go-github-com-hashicorp-go-retryablehttp
           go-github-com-hashicorp-go-cleanhttp
           go-github-com-google-go-querystring))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/xanzy/go-gitlab")
    (synopsis "go-gitlab")
    (description "Package gitlab implements a GitLab API client.")
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

(define-public go-github-com-xi2-xz
  (package
    (name "go-github-com-xi2-xz")
    (version "0.0.0-20171230120015-48954b6210f8")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/xi2/xz")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "178r0fa2dpzxf0sabs7dn0c8fa7vs87zlxk6spkn374ls9pir7nq"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/xi2/xz"))
    (home-page "https://github.com/xi2/xz")
    (synopsis #f)
    (description #f)
    ;; same license as xz?
    (license #f)))

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

;; ready to upstream
(define-public go-github-com-youmark-pkcs8
  (package
    (name "go-github-com-youmark-pkcs8")
    (version "0.0.0-20201027041543-1326539a0a0a")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/youmark/pkcs8")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1bk20x279iiafxh39v75hrmxncbkmw17603g8xw5b59cqzzpnrmv"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/youmark/pkcs8"))
    (propagated-inputs
     (list go-golang-org-x-crypto))
    (home-page "https://github.com/youmark/pkcs8")
    (synopsis "Go implemention to parse and convert PKCS#8 format keys")
    (description
     "Package pkcs8 implements functions to parse and convert private keys in
PKCS#8 format, as defined in RFC5208 and RFC5958.")
    (license license:expat)))

(define-public go-github-com-yudai-gojsondiff
  (package
    (name "go-github-com-yudai-gojsondiff")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/yudai/gojsondiff")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0qnymi0027mb8kxm24mmd22bvjrdkc56c7f4q3lbdf93x1vxbbc2"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (delete-file-recursively "vendor")))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/yudai/gojsondiff"))
    (propagated-inputs
     `(("go-github-com-sergi-go-diff" ,go-github-com-sergi-go-diff)
       ("go-github-com-yudai-golcs" ,go-github-com-yudai-golcs)))
    (native-inputs
     `(("go-github-com-onsi-ginkgo" ,go-github-com-onsi-ginkgo)
       ("go-github-com-onsi-gomega" ,go-github-com-onsi-gomega)
       ("go-github-com-yudai-pp" ,go-github-com-yudai-pp)))
    (home-page "https://github.com/yudai/gojsondiff")
    (synopsis "Go JSON Diff (and Patch)")
    (description #f)
    (license license:expat)))

(define-public go-github-com-yudai-golcs
  (package
    (name "go-github-com-yudai-golcs")
    (version "0.0.0-20170316035057-ecda9a501e82")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/yudai/golcs")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0mx6wc5fz05yhvg03vvps93bc5mw4vnng98fhmixd47385qb29pq"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/yudai/golcs"
       ;; https://github.com/yudai/golcs/issues/3
       ;; Tests are known to be flakey and to fail on 32-bit systems or fast systems.
       #:tests? #f))
    (home-page "https://github.com/yudai/golcs")
    (synopsis "Go Longest Common Subsequence (LCS)")
    (description #f)
    (license license:expat)))

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

(define-public go-github-com-yuin-gopher-lua
  (package
    (name "go-github-com-yuin-gopher-lua")
    (version "0.0.0-20210529063254-f4c35e4016d9")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/yuin/gopher-lua")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1knkabycjvjkjb7vdj1cm0g856fsc8yzm3yqlwhlbzbpkfq4xjcf"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/yuin/gopher-lua"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-tests
           (lambda _
             (with-directory-excursion "src/github.com/yuin/gopher-lua"
               (substitute* "script_test.go"
                 ;; Procduces stack overflow.
                 ((".*files\\.lua.*") ""))))))))
    (propagated-inputs
     (list go-github-com-chzyer-readline))
    (home-page "https://github.com/yuin/gopher-lua")
    (synopsis "Lua compiler in Go")
    (description "GopherLua is a VM and compiler for Lua in Go.")
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
    (arguments '(#:import-path "github.com/ziutek/mymysql"))
    (home-page "https://github.com/ziutek/mymysql")
    (synopsis "MyMySQL v1.5.4 (2015-01-08)")
    (description
      "Sorry for my poor English.  If you can help with improving the English in this
documentation, please contact me.")
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

(define-public go-golang-org-x-crypto-next
  (package
    (name "go-golang-org-x-crypto")
    (version "0.0.0-20211215153901-e495a2d5b3d3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://go.googlesource.com/crypto")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "06xw3x5sf6aq6gxdh24jlhfzi26zpym5jc4g43s63v8gh1fghg6m"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "golang.org/x/crypto"
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
               (find-files "src/golang.org/x/crypto"
                 (lambda (file stat)
                   (and
                     (eq? (stat:type stat) 'directory)
                     (let ((files (find-files file "\\.go$")))
                       (and
                         (not (null? files))
                         (not (string-contains file "internal"))
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
               (find-files "src/golang.org/x/crypto"
                 (lambda (file stat)
                   (and
                     (eq? (stat:type stat) 'directory)
                     (let ((files (find-files file "\\.go$")))
                       (and
                         (not (null? files))
                         (not (string-contains file "internal"))
                         (not (null?
                                (filter-map
                                  (lambda (test-entry)
                                    (not (string-contains test-entry file-name-separator-string)))
                                  (map (lambda (entry)
                                         (string-drop entry (1+ (string-length file))))
                                       files))))))))
                 #:directories? #t)))))))
    (propagated-inputs
     (list go-golang-org-x-text-next
           go-golang-org-x-term-next
           go-golang-org-x-sys-next
           go-golang-org-x-net-next))
    (home-page "https://golang.org/x/crypto")
    (synopsis "Go Cryptography")
    (description
      "This repository holds supplementary Go cryptography libraries.")
    (license license:bsd-3)))

(define-public go-golang-org-x-image-next
  (package
    (name "go-golang-org-x-image")
    (version "0.0.0-20211028202545-6944b10bf410")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://go.googlesource.com/image")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0sjbdd6dxvwpxksw9w7i2f6kg9vrpha9qgi5az5gmy09hwv53f9m"))))
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
    (propagated-inputs
     (list go-golang-org-x-text-next))
    (home-page "https://golang.org/x/image")
    (synopsis "Go Images")
    (description "This repository holds supplementary Go image libraries.")
    (license license:bsd-3)))

(define-public go-golang-org-x-lint-next
  (package
    (name "go-golang-org-x-lint")
    (version "0.0.0-20210508222113-6edffad5e616")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://go.googlesource.com/lint")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1n7lrr3282q3li4f06afms444qy13rfd316za0drqihakwyki2jk"))))
    (build-system go-build-system)
    (arguments
     '(#:tests? #f      ; Tests fail
       #:import-path "golang.org/x/lint"))
    (propagated-inputs
     (list go-golang-org-x-tools-bootstrap))
    (home-page "https://golang.org/x/lint")
    (synopsis "Installation")
    (description "Package lint contains a linter for Go source code.")
    (license license:bsd-3)))

(define-public go-golang-org-x-mod-next
  (package
    (name "go-golang-org-x-mod")
    (version "0.5.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://go.googlesource.com/mod")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "11ck0f35pa91hhxpf98igmj6gg0lms3b3pjm1y7sna1zz52m8f09"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "golang.org/x/mod"
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key import-path build-flags #:allow-other-keys)
             (for-each
               (lambda (directory)
                 ((assoc-ref %standard-phases 'build)
                  #:build-flags build-flags
                  #:import-path (string-append "golang.org/x/mod/" directory)))
               (list "gosumcheck"
                     "modfile"
                     "module"
                     "semver"
                     "sumdb"
                     "zip"))))
         (replace 'check
           (lambda* (#:key tests? import-path #:allow-other-keys)
             (for-each
               (lambda (directory)
                 ((assoc-ref %standard-phases 'check)
                  #:tests? tests?
                  #:import-path (string-append "golang.org/x/mod/" directory)))
               (list "gosumcheck"
                     "modfile"
                     "module"
                     "semver"
                     "sumdb"
                     "zip")))))))
    (propagated-inputs
     (list go-golang-org-x-xerrors-next
           go-golang-org-x-crypto-next))
    (inputs
     (list go-golang-org-x-tools-bootstrap))
    (home-page "https://golang.org/x/mod")
    (synopsis "mod")
    (description
      "This repository holds packages for writing tools that work directly with Go
module mechanics.  That is, it is for direct manipulation of Go modules
themselves.")
    (license license:bsd-3)))

(define-public go-golang-org-x-net-next
  (package
    (name "go-golang-org-x-net")
    (version "0.0.0-20211216030914-fe4d6282115f")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://go.googlesource.com/net")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "16p3fqc0nb00gn8csiz5y2416r22yma4mg44f6zp0l7ra0a800qq"))))
    (build-system go-build-system)
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
                         (not (string-contains file "net/lif"))
                         (not (string-contains file "net/route"))
                         (not (null?
                                (filter-map
                                  (lambda (test-entry)
                                    (not (string-contains test-entry file-name-separator-string)))
                                  (map (lambda (entry)
                                         (string-drop entry (1+ (string-length file))))
                                       files))))))))
                 #:directories? #t)))))))
    (propagated-inputs
     (list go-golang-org-x-text-next
           go-golang-org-x-term-next
           go-golang-org-x-sys-next))
    (home-page "https://golang.org/x/net")
    (synopsis "Go Networking")
    (description
     "This repository holds supplementary Go networking libraries.")
    (license license:bsd-3)))

(define-public go-golang-org-x-sync-next
  (package
    (name "go-golang-org-x-sync")
    (version "0.0.0-20210220032951-036812b2e83c")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://go.googlesource.com/sync")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1gl202py3s4gl6arkaxlf8qa6f0jyyg2f95m6f89qnfmr416h85b"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "golang.org/x/sync"
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key import-path build-flags #:allow-other-keys)
             (for-each
               (lambda (directory)
                 ((assoc-ref %standard-phases 'build)
                  #:build-flags build-flags
                  #:import-path (string-append "golang.org/x/sync/" directory)))
               (list "errgroup"
                     "semaphore"
                     "singleflight"
                     "syncmap"))))
         (replace 'check
           (lambda* (#:key tests? import-path #:allow-other-keys)
             (for-each
               (lambda (directory)
                 ((assoc-ref %standard-phases 'check)
                  #:tests? tests?
                  #:import-path (string-append "golang.org/x/sync/" directory)))
               (list "errgroup"
                     "semaphore"
                     "singleflight"
                     "syncmap")))))))
    (home-page "https://golang.org/x/sync")
    (synopsis "Go Sync")
    (description
      "This repository provides Go concurrency primitives in addition to the ones
provided by the language and \"sync\" and \"sync/atomic\" packages.")
    (license license:bsd-3)))

(define-public go-golang-org-x-sync-semaphore
  (package
    (inherit go-golang.org-x-sync-errgroup)
    (name "go-golang-org-x-sync-semaphore")
    (arguments
     '(#:import-path "golang.org/x/sync/semaphore"
       #:unpack-path "golang.org/x/sync"))))

(define-public go-golang-org-x-sys-next
  (package
    (name "go-golang-org-x-sys")
    (version "0.0.0-20211216021012-1d35b9e2eb4e")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://go.googlesource.com/sys")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "09xmnw6hhpqnakm99xxigg0znbx46f084lpacz67p5rbcdngjxis"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "golang.org/x/sys"
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key import-path build-flags #:allow-other-keys)
             (for-each
               (lambda (directory)
                 ((assoc-ref %standard-phases 'build)
                  #:build-flags build-flags
                  #:import-path (string-append "golang.org/x/sys/" directory)))
               (list "cpu"
                     "execabs"
                     "unix"))))
         (replace 'check
           (lambda* (#:key tests? import-path #:allow-other-keys)
             (for-each
               (lambda (directory)
                 ((assoc-ref %standard-phases 'check)
                  #:tests? tests?
                  #:import-path (string-append "golang.org/x/sys/" directory)))
               (list "cpu"
                     "execabs"
                     "unix")))))))
    (home-page "https://golang.org/x/sys")
    (synopsis "sys")
    (description
      "This repository holds supplemental Go packages for low-level interactions with
the operating system.")
    (license license:bsd-3)))

(define-public go-golang-org-x-term-next
  (package
    (name "go-golang-org-x-term")
    (version "0.0.0-20210927222741-03fcf44c2211")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://go.googlesource.com/term")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0aw5lgwq5w5kvwfa3jl7l83p9c827ksy4a99dqzzsqxvmk2zdi8f"))))
    (build-system go-build-system)
    (arguments '(#:import-path "golang.org/x/term"))
    (propagated-inputs
     (list go-golang-org-x-sys-next))
    (home-page "https://golang.org/x/term")
    (synopsis "Go terminal/console support")
    (description
      "Package term provides support functions for dealing with terminals, as commonly
found on UNIX systems.")
    (license license:bsd-3)))

(define-public go-golang-org-x-text-next
  (package
    (name "go-golang-org-x-text")
    (version "0.3.7")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://go.googlesource.com/text")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0xkw0qvfjyifdqd25y7nxdqkdh92inymw3q7841nricc9s01p4jy"))))
    (build-system go-build-system)
    (arguments '(#:import-path "golang.org/x/text"))
    (inputs
     (list go-golang-org-x-tools-bootstrap))
    (home-page "https://golang.org/x/text")
    (synopsis "Go Text")
    (description
      "text is a repository of text-related packages related to internationalization
(i18n) and localization (l10n), such as character encodings, text
transformations, and locale-specific text handling.")
    (license license:bsd-3)))

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
    ;       go-golang-org-x-xerrors-next
    ;       go-golang-org-x-text-next
    ;       go-golang-org-x-sys-next
    ;       go-golang-org-x-sync-next
    ;       go-golang-org-x-net-next
    ;       go-golang-org-x-mod-next
    ;       go-github-com-yuin-goldmark
    ;       ))
    (properties `((hidden? . #t)))
    ))

(define-public go-golang-org-x-tools-next
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
    (arguments
     '(#:import-path "golang.org/x/tools"
       #:modules ((guix build go-build-system)
                  (guix build utils)
                  (srfi srfi-1))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-filepaths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* (find-files "src/golang.org/x/tools" "\\.go$")
               (("/usr/bin/diff") (search-input-file inputs "/bin/diff")))))
         (replace 'build
           (lambda* (#:key import-path build-flags #:allow-other-keys)
             (for-each
               (lambda (directory)
                 ((assoc-ref %standard-phases 'build)
                  #:build-flags build-flags
                  #:import-path (string-drop directory 4)))
               (find-files "src/golang.org/x/tools"
                 (lambda (file stat)
                   (and
                     (eq? (stat:type stat) 'directory)
                     (let ((files (find-files file "\\.go$")))
                       (and
                         (not (null? files))
                         (not (string-contains file "copyright"))
                         (not (string-contains file "gopls"))
                         (not (string-contains file "testdata"))
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
               (find-files "src/golang.org/x/tools"
                 (lambda (file stat)
                   (and
                     (eq? (stat:type stat) 'directory)
                     (let ((files (find-files file "\\.go$")))
                       (and
                         (not (null? files))
                         (not (string-contains file "copyright"))
                         (not (string-contains file "cmd/stringer"))    ; Tries to 'go get'.
                         (not (string-contains file "gcexportdata"))
                         (not (string-contains file "go/packages"))
                         (not (string-contains file "go/ssa"))
                         (not (string-contains file "godoc"))
                         (not (string-contains file "gopls"))
                         (not (string-contains file "internal"))
                         (not (string-contains file "testdata"))
                         (not (null?
                                (filter-map
                                  (lambda (test-entry)
                                    (not (string-contains test-entry file-name-separator-string)))
                                  (map (lambda (entry)
                                         (string-drop entry (1+ (string-length file))))
                                       files))))))))
                 #:directories? #t)))))))
    (propagated-inputs
     (list go-github-com-yuin-goldmark
           go-golang-org-x-mod-next
           go-golang-org-x-net-next
           go-golang-org-x-sync-next
           go-golang-org-x-sys-next
           go-golang-org-x-text-next
           go-golang-org-x-xerrors-next))))

(define-public go-golang-org-x-xerrors-next
  (package
    (name "go-golang-org-x-xerrors")
    (version "0.0.0-20200804184101-5ec99f83aff1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://go.googlesource.com/xerrors")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1dbzc3gmf2haazpv7cgmv97rq40g2xzwbglc17vas8dwhgwgwrzb"))))
    (build-system go-build-system)
    (arguments '(#:import-path "golang.org/x/xerrors"))
    (home-page "https://golang.org/x/xerrors")
    (synopsis #f)
    (description "Package xerrors implements functions to manipulate errors.")
    (license license:bsd-3)))

;; ready to upstream
(define-public go-go-mongodb-org-mongo-driver
  (package
    (name "go-go-mongodb-org-mongo-driver")
    (version "1.8.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/mongodb/mongo-go-driver")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "057h3r7bh8ymfqs5idfbrx7gvs2zcjzfcl5zi2lsx5j7xqykbm0x"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (delete-file-recursively "vendor")))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "go.mongodb.org/mongo-driver"
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key import-path build-flags #:allow-other-keys)
             (for-each
               (lambda (directory)
                 ((assoc-ref %standard-phases 'build)
                  #:build-flags build-flags
                  #:import-path (string-append
                                  "go.mongodb.org/mongo-driver/"
                                  directory)))
               (list "bson"
                     "internal"
                     "mongo"
                     "x/bsonx"
                     "x/mongo/driver"))))
         (replace 'check
           (lambda* (#:key tests? import-path #:allow-other-keys)
             (for-each
               (lambda (directory)
                 ((assoc-ref %standard-phases 'check)
                  #:tests? tests?
                  #:import-path (string-append
                                  "go.mongodb.org/mongo-driver/"
                                  directory)))
               (list "bson"
                     "internal"
                     ;"mongo"       ; Wants a running mongodb server.
                     "x/bsonx"
                     "x/mongo/driver")))))))
    (propagated-inputs
     (list go-gopkg-in-check-v1
           go-golang-org-x-tools
           go-golang-org-x-sync
           go-golang-org-x-crypto
           go-github-com-youmark-pkcs8
           go-github-com-xdg-go-stringprep
           go-github-com-xdg-go-scram
           go-github-com-tidwall-pretty
           go-github-com-pkg-errors
           go-github-com-montanaflynn-stats
           go-github-com-kr-pretty
           go-github-com-klauspost-compress
           go-github-com-google-go-cmp
           go-github-com-golang-snappy
           go-github-com-go-stack-stack
           go-github-com-davecgh-go-spew))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://go.mongodb.org/mongo-driver")
    (synopsis "MongoDB Go driver")
    (description "This package provides the MongoDB supported driver for Go.")
    (license license:asl2.0)))

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

(define-public go-google-golang-org-appengine
  (package
    (name "go-google-golang-org-appengine")
    (version "1.6.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/golang/appengine")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wkipg7xxc0ha5p6c3bj0vpgq38l18441n5l6zxdhx0gzvz5z1hs"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "google.golang.org/appengine"))
    (propagated-inputs
     `(("go-github-com-golang-protobuf-proto" ,go-github-com-golang-protobuf-proto)
       ("go-golang-org-x-net" ,go-golang-org-x-net)))
    (home-page "https://google.golang.org/appengine")
    (synopsis "Internal support for package appengine")
    (description "This package supports the Go runtime on App Engine standard.
It provides APIs for interacting with App Engine services.")
    (license license:asl2.0)))

(define-public go-google-golang-org-appengine-internal
  (package
    (name "go-google-golang-org-appengine-internal")
    (version "1.6.7")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/golang/appengine")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1wkipg7xxc0ha5p6c3bj0vpgq38l18441n5l6zxdhx0gzvz5z1hs"))))
    (build-system go-build-system)
    (arguments
     '(#:unpack-path "google.golang.org/appengine"
       #:import-path "google.golang.org/appengine/internal"))
    (propagated-inputs
     `(("go-github-com-golang-protobuf-proto" ,go-github-com-golang-protobuf-proto)
       ("go-golang-org-x-net" ,go-golang-org-x-net)))
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
     `(("go-github-com-golang-protobuf-proto" ,go-github-com-golang-protobuf-proto)
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
    (arguments '(#:import-path "google.golang.org/genproto"))
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
    (arguments '(#:import-path "google.golang.org/grpc"))
    (propagated-inputs
     (list ;go-google-golang-org-protobuf
        ;go-google-golang-org-genproto
        ;go-golang-org-x-sys
        ;go-golang-org-x-oauth2
        ;go-golang-org-x-net
        ;go-github-com-google-uuid
        ;go-github-com-google-go-cmp
        ;go-github-com-golang-protobuf
        ;go-github-com-golang-glog
        ;go-github-com-envoyproxy-go-control-plane
        ;go-github-com-cncf-xds-go
        ;go-github-com-cncf-udpa-go
        ;go-github-com-cespare-xxhash-v2
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

(define-public go-google-golang-org-protobuf
  (deprecated-package "go-github-com-golang-org-protobuf" go-github-com-golang-protobuf-proto))
;  (package
;    (name "go-google-golang-org-protobuf")
;    (version "1.27.1")
;    (source
;      (origin
;        (method git-fetch)
;        (uri (git-reference
;               (url "https://go.googlesource.com/protobuf")
;               (commit (string-append "v" version))))
;        (file-name (git-file-name name version))
;        (sha256
;          (base32 "0aszb7cv8fq1m8akgd4kjyg5q7g5z9fdqnry6057ygq9r8r2yif2"))))
;    (build-system go-build-system)
;    (arguments '(#:import-path "google.golang.org/protobuf"))
;    (propagated-inputs
;     (list go-github-com-google-go-cmp
;        ;go-github-com-golang-protobuf)
;        ))
;    (home-page "https://google.golang.org/protobuf")
;    (synopsis "Go support for Protocol Buffers")
;    (description
;      "This project hosts the Go implementation for
;@url{https://developers.google.com/protocol-buffers,protocol buffers}, which is
;a language-neutral, platform-neutral, extensible mechanism for serializing
;structured data.  The protocol buffer language is a language for specifying the
;schema for structured data.  This schema is compiled into language specific
;bindings.  This project provides both a tool to generate Go code for the
;protocol buffer language, and also the runtime implementation to handle
;serialization of messages in Go.  See the
;@url{https://developers.google.com/protocol-buffers/docs/overview,protocol
;buffer developer guide} for more information about protocol buffers themselves.")
;    (license license:bsd-3)))

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
     `(("go-github-com-ajg-form" ,go-github-com-ajg-form)
       ("go-github-com-fatih-structs" ,go-github-com-fatih-structs)
       ("go-github-com-gavv-monotime" ,go-github-com-gavv-monotime)
       ("go-github-com-google-go-querystring" ,go-github-com-google-go-querystring)
       ("go-github-com-imkira-go-interpol" ,go-github-com-imkira-go-interpol)
       ("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)
       ("go-github-com-valyala-fasthttp" ,go-github-com-valyala-fasthttp)
       ("go-golang-org-x-net" ,go-golang-org-x-net)
       ("go-github-com-xeipuuv-gojsonschema" ,go-github-com-xeipuuv-gojsonschema)
       ("go-github-com-yalp-jsonpath" ,go-github-com-yalp-jsonpath)
       ("go-github-com-yudai-gojsondiff" ,go-github-com-yudai-gojsondiff)
       ("go-moul-io-http2curl" ,go-moul-io-http2curl)))
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

(define-public go-gopkg-in-inconshreveable-log15-v2
  (package
    (name "go-gopkg-in-inconshreveable-log15-v2")
    (version "2.0.0-20200109203555-b30bc20e4fd1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gopkg.in/inconshreveable/log15.v2")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "03frzx2ar9fsvdrlq7d1k8askaxkyvml2im39ipcmnz760rv52hx"))))
    (build-system go-build-system)
    (arguments
      '(#:import-path
        "gopkg.in/inconshreveable/log15.v2"
        #:unpack-path
        "gopkg.in/inconshreveable/log15.v2"))
    (home-page "https://gopkg.in/inconshreveable/log15.v2")
    (synopsis "log15")
    (description
      "Package log15 provides an opinionated, simple toolkit for best-practice logging
that is both human and machine readable.  It is modeled after the standard
library's io and net/http packages.")
    (license license:asl2.0)))

;; ready to upstream
(define-public go-gopkg-in-square-go-jose-v2
  (package
    (name "go-gopkg-in-square-go-jose-v2")
    (version "2.6.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gopkg.in/square/go-jose.v2")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1b1nhqxfmhzwrfk7pkvp2w3z3d0pf5ir00vizmy2d4xdbnldn70r"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "gopkg.in/square/go-jose.v2"))
    (propagated-inputs
     (list go-golang-org-x-crypto))
    (native-inputs
     (list go-github-com-google-go-cmp
           go-github-com-stretchr-testify))
    (home-page "https://gopkg.in/square/go-jose.v2")
    (synopsis "JavaScript Object Signing and Encryption in Go")
    (description
     "Package jose aims to provide an implementation of the Javascript Object
Signing and Encryption set of standards.  It implements encryption and signing
based on the JSON Web Encryption and JSON Web Signature standards, with optional
JSON Web Token support available in a sub-package.  The library supports both
the compact and full serialization formats, and has optional support for
multiple recipients.")
    (license license:asl2.0)))

(define-public go-gopkg-in-src-d-go-billy-v4
  (package
    (inherit go-github-com-go-git-go-billy)
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
     `(("go-github-com-alcortesm-tgz" ,go-github-com-alcortesm-tgz)
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

(define-public go-gopkg-in-tomb-v1
  (package
    (name "go-gopkg-in-tomb-v1")
    (version "1.0.0-20141024135613-dd632973f1e7")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gopkg.in/tomb.v1")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1lqmq1ag7s4b3gc3ddvr792c5xb5k6sfn0cchr3i2s7f1c231zjv"))))
    (build-system go-build-system)
    (arguments
      ;; https://github.com/go-tomb/tomb/issues/25
     '(#:tests? #f      ; Fatalf format %q reads arg #2, but call has 1 arg
       #:import-path "gopkg.in/tomb.v1"
       #:unpack-path "gopkg.in/tomb.v1"))
    (home-page "https://gopkg.in/tomb.v1")
    (synopsis "Installation and usage")
    (description
     "The tomb package offers a conventional API for clean goroutine termination.")
    (license license:bsd-3)))

(define-public go-gopkg-in-warnings-v0
  (package
    (name "go-gopkg-in-warnings-v0")
    (version "0.1.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gopkg.in/warnings.v0")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1kzj50jn708cingn7a13c2wdlzs6qv89dr2h4zj8d09647vlnd81"))))
    (build-system go-build-system)
    (arguments
      '(#:import-path
        "gopkg.in/warnings.v0"
        #:unpack-path
        "gopkg.in/warnings.v0"))
    (home-page "https://gopkg.in/warnings.v0")
    (synopsis #f)
    (description
      "Package warnings implements error handling with non-fatal errors (warnings).")
    (license license:bsd-2)))

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

(define-public go-lukechampine-com-uint128
  (package
    (name "go-lukechampine-com-uint128")
    (version "1.1.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/lukechampine/uint128")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "047rcvavn42v638ywlc8zw7gl83y836mgg171qcbjsch7nvp5sh1"))))
    (build-system go-build-system)
    (arguments '(#:import-path "lukechampine.com/uint128"))
    (home-page "https://lukechampine.com/uint128")
    (synopsis "uint128")
    (description
      "@code{uint128} provides a high-performance @code{Uint128} type that supports
standard arithmetic operations.  Unlike @code{math/big}, operations on
@code{Uint128} values always produce new values instead of modifying a pointer
receiver.  A @code{Uint128} value is therefore immutable, just like
@code{uint64} and friends.")
    (license license:expat)))

(define-public go-modernc-org-ccgo-v3
  (package
    (name "go-modernc-org-ccgo-v3")
    (version "3.13.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gitlab.com/cznic/ccgo")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1ad9wvmnbrs4nix2jb0k9xhd1dvl4r2h3ifscjj7wybbbz820wsw"))))
    (build-system go-build-system)
    (arguments
      '(#:import-path "modernc.org/ccgo/v3"
        #:unpack-path "modernc.org/ccgo/v3"))
    (propagated-inputs
      `(;("go-modernc-org-opt" ,go-modernc-org-opt)
        ;("go-modernc-org-mathutil" ,go-modernc-org-mathutil)
        ;("go-modernc-org-libc" ,go-modernc-org-libc)
        ;("go-modernc-org-ccorpus" ,go-modernc-org-ccorpus)
        ;("go-modernc-org-cc-v3" ,go-modernc-org-cc-v3)
        ;("go-golang-org-x-tools" ,go-golang-org-x-tools)
        ;("go-golang-org-x-sys" ,go-golang-org-x-sys)
        ;("go-github-com-pmezard-go-difflib" ,go-github-com-pmezard-go-difflib)
        ;("go-github-com-kballard-go-shellquote" ,go-github-com-kballard-go-shellquote)
        ;("go-github-com-dustin-go-humanize" ,go-github-com-dustin-go-humanize)
        ))
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

(define-public go-modernc-org-cc-v3
  (package
    (name "go-modernc-org-cc-v3")
    (version "3.35.20")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gitlab.com/cznic/cc")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0w3w0dxdq30ls5ihg94wgbyk8ndq22n2sz035p5ayc62gwdznsfr"))))
    (build-system go-build-system)
    (arguments
      '(#:import-path "modernc.org/cc/v3" #:unpack-path "modernc.org/cc/v3"))
    (propagated-inputs
      `(;("go-modernc-org-token" ,go-modernc-org-token)
        ;("go-modernc-org-strutil" ,go-modernc-org-strutil)
        ;("go-modernc-org-mathutil" ,go-modernc-org-mathutil)
        ;("go-lukechampine-com-uint128" ,go-lukechampine-com-uint128)
        ;("go-github-com-google-go-cmp" ,go-github-com-google-go-cmp)
        ;("go-github-com-dustin-go-humanize" ,go-github-com-dustin-go-humanize)
        ))
    (home-page "https://modernc.org/cc/v3")
    (synopsis "cc/v3")
    (description "Package cc is a C99 compiler front end (Work in progress).")
    (license license:bsd-3)))

(define-public go-modernc-org-httpfs
  (package
    (name "go-modernc-org-httpfs")
    (version "1.0.6")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gitlab.com/cznic/httpfs")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "01q5rvhxmrd45h0ljh4185wlly7rxv6vvh28d2shsyan4nj67zf1"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "modernc.org/httpfs"))
    (home-page "https://modernc.org/httpfs")
    (synopsis "httpfs")
    (description
      "Package httpfs implements http.FileSystem on top of a map[string]string.")
    (license license:bsd-3)))

(define-public go-modernc-org-libc
  (package
    (name "go-modernc-org-libc")
    (version "1.12.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gitlab.com/cznic/libc")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1kj6ixc6mwp05349jhqbqv0n2jzdqcjmbj81jw6q71xrf92xs378"))))
    (build-system go-build-system)
    (arguments '(#:import-path "modernc.org/libc"))
    (propagated-inputs
      `(;("go-modernc-org-memory" ,go-modernc-org-memory)
        ;("go-modernc-org-mathutil" ,go-modernc-org-mathutil)
        ;("go-modernc-org-ccgo-v3" ,go-modernc-org-ccgo-v3)
        ;("go-golang-org-x-sys" ,go-golang-org-x-sys)
        ;("go-github-com-mattn-go-isatty" ,go-github-com-mattn-go-isatty)
        ;("go-github-com-google-uuid" ,go-github-com-google-uuid)
        ))
    (home-page "https://modernc.org/libc")
    (synopsis "libc")
    (description
      "Package libc provides run time support for ccgo generated programs and
implements selected parts of the C standard library.")
    (license license:bsd-3)))

(define-public go-modernc-org-mathutil
  (package
    (name "go-modernc-org-mathutil")
    (version "1.4.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gitlab.com/cznic/mathutil")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0xzs3a29djlqfqmdjfblgv1adb4v11z6557mprx6d92a1byamdwv"))))
    (build-system go-build-system)
    (arguments '(#:import-path "modernc.org/mathutil"))
    (propagated-inputs
     (list go-github-com-remyoudompheng-bigfft))
    (home-page "https://modernc.org/mathutil")
    (synopsis #f)
    (description
      "Package mathutil provides utilities supplementing the standard 'math' and
'math/rand' packages.")
    (license license:bsd-3)))

(define-public go-modernc-org-memory
  (package
    (name "go-modernc-org-memory")
    (version "1.0.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gitlab.com/cznic/memory")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "089b7qhz49rm5158icq7f4h0jgnpa6p803z70jsx5y2yxkgh8463"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "modernc.org/memory"))
    (propagated-inputs
     (list go-modernc-org-mathutil))
    (home-page "https://modernc.org/memory")
    (synopsis "memory")
    (description "Package memory implements a memory allocator.")
    (license license:bsd-3)))

(define-public go-modernc-org-opt
  (package
    (name "go-modernc-org-opt")
    (version "0.1.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gitlab.com/cznic/opt")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0s51dhrb2ldxki9mkhc3yb60dvb1ka7j1dyzj2p137y2z8cyw498"))))
    (build-system go-build-system)
    (arguments '(#:import-path "modernc.org/opt"))
    (home-page "https://modernc.org/opt")
    (synopsis "opt")
    (description "Package opt implements command-line flag parsing.")
    (license license:bsd-3)))

(define-public go-modernc-org-sqlite
  (package
    (name "go-modernc-org-sqlite")
    (version "1.14.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gitlab.com/cznic/sqlite")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1ch8ifyp8p87kvikj5h6mhvfzzy9mmdmf8ihzay1dvqd4gqkmq71"))))
    (build-system go-build-system)
    (arguments '(#:import-path "modernc.org/sqlite"))
    (propagated-inputs
      `(;("go-modernc-org-z" ,go-modernc-org-z)
        ;("go-modernc-org-tcl" ,go-modernc-org-tcl)
        ;("go-modernc-org-mathutil" ,go-modernc-org-mathutil)
        ;("go-modernc-org-libc" ,go-modernc-org-libc)
        ;("go-modernc-org-ccgo-v3" ,go-modernc-org-ccgo-v3)
        ;("go-golang-org-x-sys" ,go-golang-org-x-sys)
        ;("go-github-com-mattn-go-sqlite3" ,go-github-com-mattn-go-sqlite3)
        ))
    (home-page "https://modernc.org/sqlite")
    (synopsis "sqlite")
    (description
      "Package sqlite is a sql/database driver using a CGo-free port of the C SQLite3
library.")
    (license license:bsd-3)))

(define-public go-modernc-org-strutil
  (package
    (name "go-modernc-org-strutil")
    (version "1.1.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gitlab.com/cznic/strutil")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1fql70xqpa0v865j6ikj8nz6vmbdwfkpbsnxzz9hk721xzaz8fl0"))))
    (build-system go-build-system)
    (arguments '(#:import-path "modernc.org/strutil"))
    (propagated-inputs
     (list go-modernc-org-mathutil))
    (home-page "https://modernc.org/strutil")
    (synopsis #f)
    (description
      "Package strutil collects utils supplemental to the standard strings package.")
    (license license:bsd-3)))

(define-public go-modernc-org-tcl
  (package
    (name "go-modernc-org-tcl")
    (version "1.9.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gitlab.com/cznic/tcl")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "1j5wcfbzwdq00jfacp75g8rmr430hf56n0a5cwybbx0xplzpcg52"))))
    (build-system go-build-system)
    (arguments '(#:import-path "modernc.org/tcl"))
    (propagated-inputs
      `(;("go-modernc-org-z" ,go-modernc-org-z)
        ;("go-modernc-org-mathutil" ,go-modernc-org-mathutil)
        ;("go-modernc-org-libc" ,go-modernc-org-libc)
        ;("go-modernc-org-httpfs" ,go-modernc-org-httpfs)
        ;("go-modernc-org-ccgo-v3" ,go-modernc-org-ccgo-v3)
        ))
    (home-page "https://modernc.org/tcl")
    (synopsis "tcl")
    (description
      "Package tcl is a CGo-free port of the Tool Command Language (Tcl).")
    (license license:bsd-3)))

(define-public go-modernc-org-token
  (package
    (name "go-modernc-org-token")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gitlab.com/cznic/token")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0gs2231h6i7xmsadyxqcqivkki74bsxwq2n7h0nkbm96fsmgxrd7"))))
    (build-system go-build-system)
    (arguments '(#:import-path "modernc.org/token"))
    (home-page "https://modernc.org/token")
    (synopsis "token")
    (description
      "Package token is variant of the stdlib package token with types FileSet and
Token removed.")
    (license license:bsd-3)))

(define-public go-modernc-org-z
  (package
    (name "go-modernc-org-z")
    (version "1.2.20")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gitlab.com/cznic/z")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0kjnxji065kr0w9jp5rw0awzqpm5hyjcx1kasmh6gmmy0hc2d64k"))))
    (build-system go-build-system)
    (arguments '(#:import-path "modernc.org/z"))
    (propagated-inputs
     (list go-modernc-org-libc
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
     `(("go-github.com-smartystreets-goconvey" ,go-github.com-smartystreets-goconvey)))
    (home-page "https://moul.io/http2curl")
    (synopsis "http2curl")
    (description #f)
    (license license:expat)))

(define-public go-mvdan-cc-xurls-v2
  (package
    (name "go-mvdan-cc-xurls-v2")
    (version "2.3.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/mvdan/xurls")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32 "0l5y320389rwfi049pvv4xijrckaf1jkc0shx59470k2nrdri1gs"))))
    (build-system go-build-system)
    (arguments
      '(#:import-path "mvdan.cc/xurls/v2" #:unpack-path "mvdan.cc/xurls/v2"))
    (propagated-inputs
     (list go-github-com-rogpeppe-go-internal))
    (home-page "https://mvdan.cc/xurls/v2")
    (synopsis "xurls")
    (description
      "Package xurls extracts urls from plain text using regular expressions.")
    (license license:bsd-3)))

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

;;;
;;;
;;;

(define-public newer-go-libraries
  (package-input-rewriting/spec
    `(
      ;; This one is in Guix twice.
      ("go-github.com-mattn-go-runewidth" . ,(const go-github-com-mattn-go-runewidth))
      ;; This isn't picked up for some reason.
      ;("go-github-com-hashicorp-go-version" . ,(const go-github-com-hashicorp-go-version-1.3.0))
      ;; We should use the newer versions.
      ("go-golang-org-x-crypto" . ,(const go-golang-org-x-crypto-next))
      ("go-golang-org-x-image" . ,(const go-golang-org-x-image-next))
      ("go-golang-org-x-lint" . ,(const go-golang-org-x-lint-next))
      ("go-golang-org-x-mod" . ,(const go-golang-org-x-mod-next))
      ("go-golang-org-x-net" . ,(const go-golang-org-x-net-next))
      ;("go-golang-org-x-oauth2" . ,(const go-golang-org-x-oauth2-next))
      ("go-golang-org-x-sync" . ,(const go-golang-org-x-sync-next))
      ("go-golang-org-x-sys" . ,(const go-golang-org-x-sys-next))
      ("go-golang-org-x-term" . ,(const go-golang-org-x-term-next))
      ("go-golang-org-x-text" . ,(const go-golang-org-x-text-next))
      ;("go-golang-org-x-time" . ,(const go-golang-org-x-time-next))
      ("go-golang-org-x-tools" . ,(const go-golang-org-x-tools-next))
      ("go-golang-org-x-xerrors" . ,(const go-golang-org-x-xerrors-next))
      )))
