;;; Copyright © 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg contrib keybase)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build utils)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system go)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages syncthing))

;; TODO: Unbundle all the go dependencies.
(define-public keybase
  (package
    (name "keybase")
    (version "5.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://github.com/keybase/client/releases/download/v"
               version "/keybase-v" version ".tar.xz"))
        (sha256
         (base32
          "10z5bnvkj7w71k4jidjpg7ndisl8kwbayspaaphm9mcyss2kq2nh"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (with-directory-excursion "go/vendor"
              (delete-file-recursively "bazil.org")
              (delete-file-recursively "camlistore.org/pkg/buildinfo")
              (delete-file-recursively "camlistore.org/pkg/images")
              (delete-file-recursively "github.com/BurntSushi")
              (delete-file-recursively "github.com/PuerkitoBio/goquery")
              ;(delete-file-recursively "github.com/RoaringBitmap/roaring")
              ;(delete-file-recursively "github.com/StackExchange/wmi")
              ;(delete-file-recursively "github.com/akavel/rsrc")
              (delete-file-recursively "github.com/andybalholm/cascadia")
              (delete-file-recursively "github.com/antchfx/htmlquery")
              (delete-file-recursively "github.com/antchfx/xmlquery")
              (delete-file-recursively "github.com/antchfx/xpath")
              (delete-file-recursively "github.com/araddon/dateparse")
              (delete-file-recursively "github.com/asaskevich/govalidator")
              (delete-file-recursively "github.com/blang")
              ;(delete-file-recursively "github.com/blevesearch/bleve")
              ;(delete-file-recursively "github.com/blevesearch/go-porterstemmer")
              ;(delete-file-recursively "github.com/blevesearch/segment")
              (delete-file-recursively "github.com/btcsuite/btcutil")
              (delete-file-recursively "github.com/buger/jsonparser")
              ;(delete-file-recursively "github.com/coreos/go-systemd")
              ;(delete-file-recursively "github.com/coreos/pkg")
              ;(delete-file-recursively "github.com/couchbase/vellum")
              (delete-file-recursively "github.com/davecgh/go-spew")
              ;(delete-file-recursively "github.com/deckarep/golang-set")
              (delete-file-recursively "github.com/docopt/docopt-go")
              ;(delete-file-recursively "github.com/dustin/go-humanize")
              ;(delete-file-recursively "github.com/eapache/channels")
              ;(delete-file-recursively "github.com/eapache/queue")
              (delete-file-recursively "github.com/edsrzf/mmap-go")
              (delete-file-recursively "github.com/emirpasic/gods")
              ;(delete-file-recursively "github.com/etcd-io/bbolt") ; go-go-etcd-io-bbolt
              ;(delete-file-recursively "github.com/gammazero/deque")
              ;(delete-file-recursively "github.com/gammazero/workerpool")
              ;(delete-file-recursively "github.com/glycerine/go-unsnap-stream")
              (delete-file-recursively "github.com/go-errors/errors")
              ;(delete-file-recursively "github.com/go-ole/go-ole")
              ;(delete-file-recursively "github.com/go-sql-driver/mysql")
              (delete-file-recursively "github.com/gobwas")
              (delete-file-recursively "github.com/gocolly/colly")
              (delete-file-recursively "github.com/golang/groupcache/lru")
              (delete-file-recursively "github.com/golang/groupcache")
              (delete-file-recursively "github.com/golang/mock")
              (delete-file-recursively "github.com/golang/protobuf")
              (delete-file-recursively "github.com/golang/snappy")
              (delete-file-recursively "github.com/hashicorp/golang-lru")
              (delete-file-recursively "github.com/jbenet/go-context")
              (delete-file-recursively "github.com/josephspurrier/goversioninfo")
              (delete-file-recursively "github.com/kennygrant/sanitize")
              (delete-file-recursively "github.com/kevinburke/ssh_config")
              ;(delete-file-recursively "github.com/keybase/backoff")
              ;(delete-file-recursively "github.com/keybase/cli")
              ;(delete-file-recursively "github.com/keybase/clockwork")
              ;(delete-file-recursively "github.com/keybase/colly")
              ;(delete-file-recursively "github.com/keybase/go-codec")
              ;(delete-file-recursively "github.com/keybase/go-crypto")
              ;(delete-file-recursively "github.com/keybase/go-framed-msgpack-rpc")
              ;(delete-file-recursively "github.com/keybase/go-jsonw")
              ;(delete-file-recursively "github.com/keybase/go-kext")
              ;(delete-file-recursively "github.com/keybase/go-keychain")
              ;(delete-file-recursively "github.com/keybase/go-logging")
              ;(delete-file-recursively "github.com/keybase/go-merkle-tree")
              ;(delete-file-recursively "github.com/keybase/go-porterstemmer")
              ;(delete-file-recursively "github.com/keybase/go-ps")
              ;(delete-file-recursively "github.com/keybase/go-triplesec-insecure")
              ;(delete-file-recursively "github.com/keybase/go-triplesec")
              ;(delete-file-recursively "github.com/keybase/go-updater")
              ;(delete-file-recursively "github.com/keybase/go-winio")
              ;(delete-file-recursively "github.com/keybase/go.dbus")
              ;(delete-file-recursively "github.com/keybase/golang-ico")
              ;(delete-file-recursively "github.com/keybase/gomounts")
              ;(delete-file-recursively "github.com/keybase/keybase-test-vectors")
              ;(delete-file-recursively "github.com/keybase/msgpackzip")
              ;(delete-file-recursively "github.com/keybase/pipeliner")
              ;(delete-file-recursively "github.com/keybase/saltpack")
              ;(delete-file-recursively "github.com/keybase/stellarnet")
              ;(delete-file-recursively "github.com/keybase/xurls")
              (delete-file-recursively "github.com/kr")
              ;(delete-file-recursively "github.com/kyokomi/emoji")
              (delete-file-recursively "github.com/lib")
              (delete-file-recursively "github.com/manucorporat/sse")
              (delete-file-recursively "github.com/mattn")
              (delete-file-recursively "github.com/miekg/dns")
              (delete-file-recursively "github.com/mitchellh")
              ;(delete-file-recursively "github.com/mschoch/smat")
              ;(delete-file-recursively "github.com/nf/cr2")
              (delete-file-recursively "github.com/nfnt/resize")
              ;(delete-file-recursively "github.com/pelletier/go-buffruneio")
              ;(delete-file-recursively "github.com/philhofer/fwd")
              (delete-file-recursively "github.com/pkg/errors")
              (delete-file-recursively "github.com/pmezard")
              ;(delete-file-recursively "github.com/qrtz/nativemessaging")
              ;(delete-file-recursively "github.com/rcrowley")
              (delete-file-recursively "github.com/rwcarlsen/goexif")
              (delete-file-recursively "github.com/saintfish/chardet")
              (delete-file-recursively "github.com/sergi/go-diff")
              (delete-file-recursively "github.com/shirou")
              (delete-file-recursively "github.com/shopspring/decimal")
              ;(delete-file-recursively "github.com/src-d/gcfg")
              (delete-file-recursively "github.com/stathat")
              ;(delete-file-recursively "github.com/stellar/go-xdr")
              ;(delete-file-recursively "github.com/stellar/go")
              ;(delete-file-recursively "github.com/steveyen/gtreap")
              (delete-file-recursively "github.com/stretchr/testify")
              ;(delete-file-recursively "github.com/syndtr/goleveldb")
              (delete-file-recursively "github.com/temoto/robotstxt")
              ;(delete-file-recursively "github.com/tinylib/msgp")
              (delete-file-recursively "github.com/urfave")
              ;(delete-file-recursively "github.com/vividcortex/ewma")
              (delete-file-recursively "github.com/willf")
              (delete-file-recursively "github.com/xanzy/ssh-agent")
              (delete-file-recursively "go.uber.org/zap")
              (delete-file-recursively "golang.org/x")
              (delete-file-recursively "google.golang.org")
              ;(delete-file-recursively "gopkg.in/src-d/go-billy.v4")
              ;(delete-file-recursively "gopkg.in/src-d/go-git.v4")
              (delete-file-recursively "rsc.io/qr/coding")
              (delete-file-recursively "rsc.io/qr/gf256")
              (delete-file-recursively "stathat.com/c/ramcache")
              )

            (for-each delete-file-recursively
                      (list "osx"
                            "shared"
                            "browser"       ; GUI
                            "protocol"      ; protocol generator and tester
                            "pvl-tools"
                            "media"
                            "packaging"))
            ;; Delete everything vendored EXCEPT for keybase code
            ;; Use the bundled code from other keybase repositories.
            ;(rename-file "go/vendor/github.com/keybase" "go-vendor-keybase")
            ;(delete-file-recursively "go/vendor")
            ;(mkdir-p "go/vendor/github.com/keybase")
            ;(rename-file "go-vendor-keybase" "go/vendor/github.com/keybase")
            ;; Replace github.com/keybase/stellarnet
            ;(delete-file-recursively "go/vendor/github.com/keybase/stellarnet")
            ;; Adjust some broken imports?
            ;; Both of these changes break the build.
            ;(substitute* (find-files "go/stellar" "\\.go$")
            ;  ((".*stellar/go/build.*") ""))
            ;(substitute* "go/stellar/global.go"
            ;  (("go/clients/horizon") "go/clients/horizonclient"))
            #t))))
    (build-system go-build-system)
    (arguments
     `(#:install-source? #f
       #:import-path "github.com/keybase/client/go/keybase"
       #:unpack-path "github.com/keybase/client"
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key import-path #:allow-other-keys)
             (for-each
               (lambda (directory)
                 (invoke "go" "install"
                         "-tags" "production"
                         "-v" "-x" "-ldflags=-s -w"
                         directory))
               (list import-path
                     "github.com/keybase/client/go/kbfs/kbfsfuse"
                     "github.com/keybase/client/go/kbfs/kbfsgit/git-remote-keybase"
                     "github.com/keybase/client/go/kbfs/redirector"
                     "github.com/keybase/client/go/kbnm"))
             #t))
         (replace 'check
           (lambda* (#:key import-path #:allow-other-keys)
             (for-each
               (lambda (directory)
                 (invoke "go" "test"
                         "-v" "-x" "-ldflags=-s -w"
                         directory))
               (list import-path
                     "github.com/keybase/client/go/kbfs/kbfsfuse"
                     "github.com/keybase/client/go/kbfs/kbfsgit/git-remote-keybase"
                     "github.com/keybase/client/go/kbfs/redirector"
                     "github.com/keybase/client/go/kbnm"))
             #t))
         (add-after 'install 'install-license
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "src/github.com/keybase/client/LICENSE"
                             (string-append out "/share/doc/"
                                            ,name "-" ,version "/"))
               #t))))))
    (inputs
     `(("go-bazil-org-fuse" ,go-bazil-org-fuse)
       ("go-camlistore-org-pkg-buildinfo" ,go-camlistore-org-pkg-buildinfo)
       ("go-camlistore-org-pkg-images" ,go-camlistore-org-pkg-images)
       ("go-github-com-andybalholm-cascadia" ,go-github-com-andybalholm-cascadia)
       ("go-github-com-antchfx-htmlquery" ,go-github-com-antchfx-htmlquery)
       ("go-github-com-antchfx-xmlquery" ,go-github-com-antchfx-xmlquery)
       ("go-github-com-araddon-dateparse" ,go-github-com-araddon-dateparse)
       ("go-github-com-blang-semver" ,go-github-com-blang-semver)
       ("go-github-com-btcsuite-btcutil" ,go-github-com-btcsuite-btcutil)
       ("go-github-com-buger-jsonparser" ,go-github-com-buger-jsonparser)
       ("go-github-com-burntsushi-toml" ,go-github-com-burntsushi-toml)
       ("go-github-com-coreos-go-systemd-activation" ,go-github-com-coreos-go-systemd-activation)
       ("go-github-com-coreos-go-systemd-daemon" ,go-github-com-coreos-go-systemd-daemon)
       ("go-github-com-coreos-go-systemd-internal-dlopen" ,go-github-com-coreos-go-systemd-internal-dlopen)
       ("go-github-com-coreos-go-systemd-util" ,go-github-com-coreos-go-systemd-util)
       ("go-github-com-davecgh-go-spew" ,go-github-com-davecgh-go-spew)
       ("go-github-com-deckarep-golang-set" ,go-github-com-deckarep-golang-set)
       ("go-github-com-dustin-go-humanize" ,go-github-com-dustin-go-humanize)

       ("go-github-com-edsrzf-mmap-go" ,go-github-com-edsrzf-mmap-go)   ; out
       ("go-github-com-emirpasic-gods" ,go-github-com-emirpasic-gods)   ; out

       ("go-github-com-gobwas-glob" ,go-github-com-gobwas-glob)
       ("go-github-com-gocolly-colly-debug" ,go-github-com-gocolly-colly-debug)
       ("go-github-com-gocolly-colly-storage" ,go-github-com-gocolly-colly-storage)
       ("go-github-com-go-errors-errors" ,go-github-com-go-errors-errors)
       ("go-github-com-golang-groupcache-lru" ,go-github-com-golang-groupcache-lru)

       ;("go-github-com-golang-protobuf-proto" ,go-github-com-golang-protobuf-proto) ; out
       ;("go-github-com-golang-snappy" ,go-github-com-golang-snappy) ; out
       ("go-github-com-jbenet-go-context" ,go-github-com-jbenet-go-context) ; out
       ("go-github-com-kevinburke-ssh-config" ,go-github-com-kevinburke-ssh-config) ; out

       ("go-github-com-golang-groupcache-singleflight" ,go-github-com-golang-groupcache-singleflight)
       ("go-github-com-golang-mock-gomock" ,go-github-com-golang-mock-gomock)
       ("go-github-com-hashicorp-golang-lru" ,go-github-com-hashicorp-golang-lru)
       ("go-github-com-kennygrant-sanitize" ,go-github-com-kennygrant-sanitize)
       ("go-github-com-kr-text" ,go-github-com-kr-text)

       ;("go-github-com-lib-pq" ,go-github-com-lib-pq)  ; out

       ("go-github-com-kyokomi-emoji" ,go-github-com-kyokomi-emoji)
       ("go-github-com-mattn-go-isatty" ,go-github-com-mattn-go-isatty)

       ("go-github-com-mitchellh-go-homedir" ,go-github-com-mitchellh-go-homedir)   ; out

       ("go-github-com-miekg-dns" ,go-github-com-miekg-dns)
       ("go-github-com-nfnt-resize" ,go-github-com-nfnt-resize)
       ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)

       ;("go-github-com-pmezard-go-difflib" ,go-github-com-pmezard-go-difflib)  ; out

       ("go-github-com-puerkitobio-goquery" ,go-github-com-puerkitobio-goquery)

       ;("go-github-com-rcrowley-go-metrics" ,go-github-com-rcrowley-go-metrics)    ; out
       ("go-github-com-sergi-go-diff" ,go-github-com-sergi-go-diff) ; out
       ("go-github-com-shirou-gopsutil" ,go-github-com-shirou-gopsutil) ; out
       ;("go-github-com-stathat-go" ,go-github-com-stathat-go)   ; out

       ("go-github-com-saintfish-chardet" ,go-github-com-saintfish-chardet)
       ("go-github-com-shopspring-decimal" ,go-github-com-shopspring-decimal)
       ("go-github-com-stellar-go-address" ,go-github-com-stellar-go-address)
       ("go-github-com-stellar-go-amount" ,go-github-com-stellar-go-amount)
       ;("go-github-com-stellar-go-build" ,go-github-com-stellar-go-build)  ; Deprecated upstream
       ("go-github-com-stellar-go-clients-federation" ,go-github-com-stellar-go-clients-federation)
       ;("go-github-com-stellar-go-clients-horizon" ,go-github-com-stellar-go-clients-horizon)  ; Deprecated upstream
       ("go-github-com-stellar-go-clients-horizonclient" ,go-github-com-stellar-go-clients-horizonclient)
       ("go-github-com-stellar-go-clients-stellartoml" ,go-github-com-stellar-go-clients-stellartoml)
       ("go-github-com-stellar-go-crc16" ,go-github-com-stellar-go-crc16)
       ("go-github-com-stellar-go-hash" ,go-github-com-stellar-go-hash)
       ("go-github-com-stellar-go-keypair" ,go-github-com-stellar-go-keypair)
       ("go-github-com-stellar-go-network" ,go-github-com-stellar-go-network)
       ("go-github-com-stellar-go-price" ,go-github-com-stellar-go-price)
       ("go-github-com-stellar-go-protocols-federation" ,go-github-com-stellar-go-protocols-federation)
       ("go-github-com-stellar-go-protocols-horizon" ,go-github-com-stellar-go-protocols-horizon)
       ("go-github-com-stellar-go-strkey" ,go-github-com-stellar-go-strkey)
       ("go-github-com-stellar-go-support-clock" ,go-github-com-stellar-go-support-clock)
       ("go-github-com-stellar-go-support-errors" ,go-github-com-stellar-go-support-errors)
       ("go-github-com-stellar-go-support-http-httpdecode" ,go-github-com-stellar-go-support-http-httpdecode)
       ("go-github-com-stellar-go-support-log" ,go-github-com-stellar-go-support-log)
       ("go-github-com-stellar-go-support-render-hal" ,go-github-com-stellar-go-support-render-hal)
       ("go-github-com-stellar-go-support-render-httpjson" ,go-github-com-stellar-go-support-render-httpjson)
       ("go-github-com-stellar-go-support-render-problem" ,go-github-com-stellar-go-support-render-problem)
       ("go-github-com-stellar-go-support-url" ,go-github-com-stellar-go-support-url)
       ("go-github-com-stellar-go-txnbuild" ,go-github-com-stellar-go-txnbuild)
       ("go-github-com-stellar-go-xdr" ,go-github-com-stellar-go-xdr)
       ("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)
       ("go-github-com-syndtr-goleveldb" ,go-github-com-syndtr-goleveldb)

       ;("go-github-com-urfave-cli" ,go-github-com-urfave-cli)   ; out
       ("go-github-com-willf-bitset" ,go-github-com-willf-bitset)   ; out
       ("go-github-com-xanzy-ssh-agent" ,go-github-com-xanzy-ssh-agent) ; out

       ("go-github-com-temoto-robotstxt" ,go-github-com-temoto-robotstxt)
       ("go-golang-org-x-crypto" ,go-golang-org-x-crypto)

       ;("go-golang-org-x-image" ,go-golang-org-x-image)    ; out

       ("go-golang-org-x-net" ,go-golang-org-x-net)
       ("go-golang.org-x-sync-errgroup" ,go-golang.org-x-sync-errgroup)
       ("go-golang.org-x-sync-semaphore" ,go-golang.org-x-sync-semaphore)

       ;("go-golang-org-x-sys" ,go-golang-org-x-sys)    ; out

       ("go-golang-org-x-text" ,go-golang-org-x-text)
       ("go-golang-org-x-time" ,go-golang-org-x-time)

       ;("go-go-etcd-io-bbolt" ,go-go-etcd-io-bbolt)    ; out
       ;("go-go-uber-org-zap" ,go-go-uber-org-zap)   ; out
       ;("go-google-golang-org-appengine" ,go-google-golang-org-appengine)   ; out

       ("go-google-golang-org-appengine-internal" ,go-google-golang-org-appengine-internal)
       ("go-google-golang-org-appengine-urlfetch" ,go-google-golang-org-appengine-urlfetch)
       ("go-rsc-io-qr" ,go-rsc-io-qr)
       ("go-stathat-com-c-ramcache" ,go-stathat-com-c-ramcache)))
    (home-page "https://keybase.io")
    (synopsis "Secure messaging and file-sharing")
    (description "Keybase is a safe, secure, and private app for everything you
do online.")
    (license license:bsd-3)))

(define-public go-golang.org-x-sync-semaphore
  (package
    (inherit go-golang.org-x-sync-errgroup)
    (name "go-golang.org-x-sync-semaphore")
    (arguments
     '(#:import-path "golang.org/x/sync/semaphore"
       #:unpack-path "golang.org/x/sync"))))

(define-public go-google-golang-org-appengine
  (package
    (name "go-google-golang-org-appengine")
    (version "1.6.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/golang/appengine")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "15c38h6fbv06cnkr6yknygfrpibyms2mya4w0l29kaxf42jn1qi5"))))
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
             (for-each make-file-writable (find-files "."))
             #t))))
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

(define-public go-github-com-nf-cr2
  (package
    (name "go-github-com-nf-cr2")
    (version "0.0.0-20180623103828-4699471a17ed")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/nf/cr2.git")
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
    (synopsis #f)
    (description #f)
    (license license:bsd-3)))

(define-public go-github-com-rwcarlsen-goexif
  (package
    (name "go-github-com-rwcarlsen-goexif")
    (version "0.0.0-20190401172101-9e8deecbddbd")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/rwcarlsen/goexif.git")
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
           (lambda* (#:key import-path #:allow-other-keys)
             (for-each
               (lambda (directory)
                 (invoke "go" "install"
                         "-tags" "production"
                         "-v" "-x" "-ldflags=-s -w"
                         directory))
               (list "github.com/rwcarlsen/goexif/exif"
                     "github.com/rwcarlsen/goexif/tiff"))
             #t))
         (replace 'check
           (lambda* (#:key import-path #:allow-other-keys)
             (for-each
               (lambda (directory)
                 (invoke "go" "test"
                         "-v" "-x" "-ldflags=-s -w"
                         directory))
               (list "github.com/rwcarlsen/goexif/exif"
                     "github.com/rwcarlsen/goexif/tiff"))
             #t)))))
    (home-page "https://github.com/rwcarlsen/goexif")
    (synopsis "goexif")
    (description #f)
    (license license:bsd-2)))

(define-public go-github-com-edsrzf-mmap-go
  (let ((commit "0bce6a6887123b67a60366d2c9fe2dfb74289d2e")
        (revision "0"))
    (package
      (name "go-github-com-edsrzf-mmap-go")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/edsrzf/mmap-go")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1am4m2k451bksnbiqj6lxknk4lsgmrhv0q3ajqac818vj0cpfgs9"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/edsrzf/mmap-go"))
      (synopsis "Go implementation of mmap")
      (description "This packages provides a Go implementation of mmap.")
      (home-page "https://github.com/edsrzf/mmap-go")
      (license license:bsd-3))))

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

(define-public go-go-etcd-io-bbolt
  (package
    (name "go-go-etcd-io-bbolt")
    (version "1.3.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/etcd-io/bbolt.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1h64gipvcg7060byv5wjlf524kqwj12p3v08kfh4ygv46vpm8p2r"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "go.etcd.io/bbolt"))
    (propagated-inputs
     `(("go-golang-org-x-sys" ,go-golang-org-x-sys)))
    (home-page "https://github.com/etcd-io/bbolt")
    (synopsis "Embedded key/value database for Go")
    (description "Bolt is a pure Go key/value store inspired by Howard Chu's
LMDB project.  The goal of the project is to provide a simple, fast, and
reliable database for projects that don't require a full database server such as
Postgres or MySQL.")
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

(define-public go-github-com-scylladb-termtables
  (package
    (name "go-github-com-scylladb-termtables")
    (version "0.0.0-20191203121021-c4c0b6d42ff4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/scylladb/termtables.git")
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

(define-public go-github-com-btcsuite-btcutil
  (package
    (name "go-github-com-btcsuite-btcutil")
    (version "1.0.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/btcsuite/btcutil.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0wwykb4cbq8xj2mls2mxma5vaahdgdy3vqw1r2fi4wyj0yr4kyw9"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/btcsuite/btcutil"))
    (inputs
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
       #:import-path "github.com/btcsuite/btcd/wire"))
    (synopsis "btcd")
    (description #f)))

(define-public go-github-com-jrick-logrotate
  (package
    (name "go-github-com-jrick-logrotate")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jrick/logrotate.git")
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

(define-public go-github-com-jessevdk-go-flags
  (package
    (name "go-github-com-jessevdk-go-flags")
    (version "1.5.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jessevdk/go-flags.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "13ixw1yx4bvcj66lkc8zgwf9j7gkvj686g991gycdsafvdvca0lj"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/jessevdk/go-flags"
       ;; Manpage tests fail due to SOURCE_DATE_EPOCH.
       #:tests? #f))
    (inputs
     `(("go-golang-org-x-sys" ,go-golang-org-x-sys)))
    (home-page "https://github.com/jessevdk/go-flags")
    (synopsis
     "go-flags: a go library for parsing command line arguments")
    (description #f)
    (license license:bsd-3)))

(define-public go-github-com-buger-jsonparser
  (package
    (name "go-github-com-buger-jsonparser")
    (version "1.1.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/buger/jsonparser.git")
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

(define-public go-github-com-coreos-go-systemd-activation
  (package
    (name "go-github-com-coreos-go-systemd-activation")
    (version "0.0.0-20191104093116-d3cd4ed1dbcf")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/coreos/go-systemd.git")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "193mgqn7n4gbb8jb5kyn6ml4lbvh4xs55qpjnisaz7j945ik3kd8"))))
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
    (synopsis "go-systemd")
    (description #f)))

(define-public go-github-com-coreos-go-systemd-internal-dlopen
  (package
    (inherit go-github-com-coreos-go-systemd-activation)
    (name "go-github-com-coreos-go-systemd-internal-dlopen")
    (arguments
     '(#:unpack-path "github.com/coreos/go-systemd"
       #:import-path "github.com/coreos/go-systemd/internal/dlopen"
       ;; src/github.com/coreos/go-systemd/internal/dlopen/dlopen_test.go:54:15: undefined: strlen
       #:tests? #f))
    (synopsis "go-systemd")
    (description #f)))

(define-public go-github-com-deckarep-golang-set
  (package
    (name "go-github-com-deckarep-golang-set")
    (version "1.7.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/deckarep/golang-set.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0y64c0p6a7ww5jp6adm6fm97vsni86njw8wkwxfmciy466vhl0lf"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/deckarep/golang-set"))
    (home-page "https://github.com/deckarep/golang-set")
    (synopsis #f)
    (description #f)
    (license license:expat)))

(define-public go-github-com-dustin-go-humanize
  (package
    (name "go-github-com-dustin-go-humanize")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/dustin/go-humanize.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1kqf1kavdyvjk7f8kx62pnm7fbypn9z1vbf8v2qdh3y7z7a0cbl3"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/dustin/go-humanize"))
    (home-page "https://github.com/dustin/go-humanize")
    (synopsis "Humane Units")
    (description #f)
    (license license:expat)))

(define-public go-github-com-go-errors-errors
  (package
    (name "go-github-com-go-errors-errors")
    (version "1.1.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/go-errors/errors.git")
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

(define-public go-github-com-golang-groupcache-singleflight
  (package
    (inherit go-github-com-golang-groupcache-lru)
    (name "go-github-com-golang-groupcache-singleflight")
    (arguments
     '(#:unpack-path "github.com/golang/groupcache"
       #:import-path "github.com/golang/groupcache/singleflight"))
    (synopsis "groupcache")
    (description #f)))

(define-public go-github-com-golang-mock-gomock
  (package
    (name "go-github-com-golang-mock-gomock")
    (version "1.5.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/golang/mock.git")
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

(define-public go-github-com-hashicorp-golang-lru
  (package
    (name "go-github-com-hashicorp-golang-lru")
    (version "0.5.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/hashicorp/golang-lru.git")
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
    (description #f)
    (license license:mpl2.0)))

(define-public go-github-com-antchfx-htmlquery
  (package
    (name "go-github-com-antchfx-htmlquery")
    (version "1.2.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/antchfx/htmlquery.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1s24biqarw2ywv2wndb7192ksqc3gpi7ydsliwgs35mky3firqj1"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/antchfx/htmlquery"))
    (propagated-inputs
     `(("go-golang-org-x-text" ,go-golang-org-x-text)
       ("go-golang-org-x-net" ,go-golang-org-x-net)
       ("go-github-com-golang-groupcache-lru" ,go-github-com-golang-groupcache-lru)
       ("go-github-com-antchfx-xpath" ,go-github-com-antchfx-xpath)))
    (home-page "https://github.com/antchfx/htmlquery")
    (synopsis "htmlquery")
    (description #f)
    (license license:expat)))

(define-public go-github-com-antchfx-xpath
  (package
    (name "go-github-com-antchfx-xpath")
    (version "1.1.11")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/antchfx/xpath.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0xgnqvx6h915q2sxjs6f7wx3d0jnx6865xzgyp01ggcfm99gbkl8"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/antchfx/xpath"))
    (home-page "https://github.com/antchfx/xpath")
    (synopsis "XPath")
    (description #f)
    (license license:expat)))

(define-public go-github-com-antchfx-xmlquery
  (package
    (name "go-github-com-antchfx-xmlquery")
    (version "1.3.5")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/antchfx/xmlquery.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0j32l2zby839xfw82yicsmavzpml3b0smvv6lrjsh6j5z150jqzy"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/antchfx/xmlquery"))
    (propagated-inputs
     `(("go-golang-org-x-text" ,go-golang-org-x-text)
       ("go-golang-org-x-net" ,go-golang-org-x-net)
       ("go-github-com-golang-groupcache-lru" ,go-github-com-golang-groupcache-lru)
       ("go-github-com-antchfx-xpath" ,go-github-com-antchfx-xpath)))
    (home-page "https://github.com/antchfx/xmlquery")
    (synopsis "xmlquery")
    (description #f)
    (license license:expat)))

(define-public go-github-com-gocolly-colly-debug
  (package
    (name "go-github-com-gocolly-colly-debug")
    (version "1.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/gocolly/colly.git")
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

(define-public go-github-com-kennygrant-sanitize
  (package
    (name "go-github-com-kennygrant-sanitize")
    (version "1.2.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/kennygrant/sanitize.git")
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

(define-public go-github-com-kyokomi-emoji
  (package
    (name "go-github-com-kyokomi-emoji")
    (version "1.5.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/kyokomi/emoji.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "005rxyxlqcd2sfjn686xb52l11wn2w0g5jv042ka6pnsx24r812a"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/kyokomi/emoji"))
    (home-page "https://github.com/kyokomi/emoji")
    (synopsis "Emoji")
    (description #f)
    (license license:expat)))

(define-public go-github-com-miekg-dns
  (package
    (name "go-github-com-miekg-dns")
    (version "1.1.41")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/miekg/dns.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "052zx26y62zx0ihpkb0bkynpd5h2s52wldm52ja48iyisd2zhcjb"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/miekg/dns"))
    (propagated-inputs
     `(("go-golang-org-x-net" ,go-golang-org-x-net)
       ("go-golang-org-x-sys" ,go-golang-org-x-sys)))
    (native-inputs
     `(("go-golang-org-x-sync" ,go-golang-org-x-sync)))
    (home-page "https://github.com/miekg/dns")
    (synopsis #f)
    (description #f)
    (license license:bsd-3)))

(define-public go-github-com-nfnt-resize
  (package
    (name "go-github-com-nfnt-resize")
    (version "0.0.0-20180221191011-83c6a9932646")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/nfnt/resize.git")
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
    (description #f)
    (license license:isc)))

(define-public go-github-com-saintfish-chardet
  (package
    (name "go-github-com-saintfish-chardet")
    (version "0.0.0-20120816061221-3af4cd4741ca")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/saintfish/chardet.git")
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

(define-public go-github-com-shopspring-decimal
  (package
    (name "go-github-com-shopspring-decimal")
    (version "1.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/shopspring/decimal.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "062cgjxl8av0bwlnib0psxz5d61q9ip7wv6z4iljkq6jkppa913z"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/shopspring/decimal"))
    (home-page "https://github.com/shopspring/decimal")
    (synopsis "decimal")
    (description #f)
    (license license:expat)))

(define-public go-github-com-temoto-robotstxt
  (package
    (name "go-github-com-temoto-robotstxt")
    (version "1.1.2")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/temoto/robotstxt.git")
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

(define-public go-google-golang-org-appengine-urlfetch
  (package
    (name "go-google-golang-org-appengine-urlfetch")
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
       #:import-path "google.golang.org/appengine/urlfetch"))
    (propagated-inputs
     `(("go-golang-org-x-net" ,go-golang-org-x-net)
       ("go-github-com-golang-protobuf-proto" ,go-github-com-golang-protobuf-proto)))
    (home-page "https://google.golang.org/appengine")
    (synopsis "Go App Engine packages")
    (description #f)
    (license license:asl2.0)))

(define-public go-google-golang-org-appengine-internal
  (package
    (inherit go-google-golang-org-appengine-urlfetch)
    (name "go-google-golang-org-appengine-internal")
    (arguments
     '(#:unpack-path "google.golang.org/appengine"
       #:import-path "google.golang.org/appengine/internal"))
    (propagated-inputs
     `(("go-golang-org-x-net" ,go-golang-org-x-net)
       ("go-github-com-golang-protobuf-proto" ,go-github-com-golang-protobuf-proto)))
    (synopsis "Go App Engine packages")
    (description #f)))

(define (go-github-com-stellar-go-package suffix)
  (package
    (name (string-append "go-github-com-stellar-go-"
                         (string-replace-substring suffix "/" "-")))
    (version "0.0.0-20210402164147-6145dda1700f")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/stellar/go.git")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name "go-github-com-stellar-go" version))
        (sha256
         (base32
          "1fp2y9lbxi05k214q8zcp6fwmngld4qa7abrvg7www209xpxq4p1"))))
    (build-system go-build-system)
    (arguments
     `(#:unpack-path "github.com/stellar/go"
       #:import-path ,(string-append "github.com/stellar/go/" suffix)))
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
  (package (inherit (go-github-com-stellar-go-package "xdr"))
    (propagated-inputs
     `(("go-github-com-lib-pq" ,go-github-com-lib-pq)
       ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
       ("go-github-com-stellar-go-xdr-xdr3" ,go-github-com-stellar-go-xdr-xdr3)))
    (native-inputs
     `(("go-github-com-onsi-ginkgo" ,go-github-com-onsi-ginkgo)
       ("go-github-com-onsi-gomega" ,go-github-com-onsi-gomega)
       ("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))))

(define-public go-github-com-stellar-go-crc16
  (package (inherit (go-github-com-stellar-go-package "crc16"))))

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

(define-public go-github-com-stellar-go-hash
  (package (inherit (go-github-com-stellar-go-package "hash"))
    (native-inputs
     `(("go-github-com-onsi-ginkgo" ,go-github-com-onsi-ginkgo)
       ("go-github-com-onsi-gomega" ,go-github-com-onsi-gomega)))))

(define-public go-github-com-stellar-go-strkey
  (package (inherit (go-github-com-stellar-go-package "strkey"))
    (propagated-inputs
     `(("go-github-com-pkg-errors" ,go-github-com-pkg-errors)))
    (native-inputs
     `(("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))))

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

(define-public go-github-com-stellar-go-support-render-problem
  (package (inherit (go-github-com-stellar-go-package "support/render/problem"))
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

(define-public go-github-com-stellar-go-support-log
  (package (inherit (go-github-com-stellar-go-package "support/log"))
    (propagated-inputs
     `(("go-github-com-go-errors-errors" ,go-github-com-go-errors-errors)
       ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
       ("go-github-com-segmentio-go-loggly" ,go-github-com-segmentio-go-loggly)
       ("go-github-com-sirupsen-logrus" ,go-github-com-sirupsen-logrus)))))

(define-public go-github-com-stellar-go-support-url
  (package (inherit (go-github-com-stellar-go-package "support/url"))
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

(define-public go-github-com-asaskevich-govalidator
  (package
    (name "go-github-com-asaskevich-govalidator")
    (version "0.0.0-20210307081110-f21760c49a8d")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/asaskevich/govalidator.git")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0aab1pym5c6di8vidynp6ly5j4kcqv6lp2737gw0a07zng0nn8lw"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/asaskevich/govalidator"
       ;; TODO: Fix tests.
       #:tests? #f))
    (home-page "https://github.com/asaskevich/govalidator")
    (synopsis "govalidator")
    (description #f)
    (license license:expat)))

(define-public go-github-com-stellar-go-xdr-xdr3
  (package
    (name "go-github-com-stellar-go-xdr-xdr3")
    (version "0.0.0-20201028102745-f80a23dac78a")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/stellar/go-xdr.git")
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

(define-public go-github-com-onsi-ginkgo
  (package
    (name "go-github-com-onsi-ginkgo")
    (version "1.16.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/onsi/ginkgo.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
          (base32
            "1zc298m2lrcv30r3hpd1pmxi7c92zpphqk09apjfd18cm0w6j5d6"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/onsi/ginkgo"))
    (propagated-inputs
     `(("go-golang-org-x-sys" ,go-golang-org-x-sys)
       ("go-github-com-nxadm-tail" ,go-github-com-nxadm-tail)))
    (home-page "https://github.com/onsi/ginkgo")
    (synopsis #f)
    (description #f)
    (license license:expat)))

(define-public go-github-com-nxadm-tail
  (package
    (name "go-github-com-nxadm-tail")
    (version "1.4.8")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/nxadm/tail.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1j2gi485fhwdpmyzn42wk62103fclwbfywg42p275z1qv2bsz1rc"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/nxadm/tail"))
    (home-page "https://github.com/nxadm/tail")
    (synopsis #f)
    (description #f)
    (license license:expat)))

(define-public go-github-com-onsi-gomega
  (package
    (name "go-github-com-onsi-gomega")
    (version "1.11.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/onsi/gomega.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "145yjcjp7z061f4frml2pdm7xga9c7j9j4wlvpya2j55qsf8wlh0"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/onsi/gomega"))
    (propagated-inputs
     `(("go-gopkg-in-yaml-v2" ,go-gopkg-in-yaml-v2)
       ("go-golang-org-x-text" ,go-golang-org-x-text)
       ("go-golang-org-x-net" ,go-golang-org-x-net)))
    (home-page "https://github.com/onsi/gomega")
    (synopsis #f)
    (description #f)
    (license license:expat)))

(define-public go-github-com-go-chi-chi
  (package
    (name "go-github-com-go-chi-chi")
    (version "1.5.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/go-chi/chi.git")
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
    (description #f)
    (license license:expat)))

(define-public go-github-com-gorilla-schema
  (package
    (name "go-github-com-gorilla-schema")
    (version "1.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/gorilla/schema.git")
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

(define-public go-github-com-manucorporat-sse
  (package
    (name "go-github-com-manucorporat-sse")
    (version "0.0.0-20160126180136-ee05b128a739")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/manucorporat/sse.git")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1rvbrq1qvfqpajyd3b0kiczvgwrrffc7fdzw7kvnk53a7sb7gwj4"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/manucorporat/sse"
       ;; TODO: fix tests.
       ;; FAIL: TestEncodeOnlyData (0.00s)
       #:tests? #f))
    (native-inputs
     `(("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))
    (home-page "https://github.com/manucorporat/sse")
    (synopsis #f)
    (description #f)
    (license license:expat)))

(define-public go-github-com-segmentio-go-loggly
  (package
    (name "go-github-com-segmentio-go-loggly")
    (version "0.5.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/segmentio/go-loggly.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "04wgxd5dp7azrlhw9qrv4v5j541khw0kgdp4va0rhfylcyggz6k3"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/segmentio/go-loggly"))
    (home-page "https://github.com/segmentio/go-loggly")
    (synopsis #f)
    (description #f)
    (license #f)))

(define-public go-github-com-jarcoal-httpmock
  (package
    (name "go-github-com-jarcoal-httpmock")
    (version "1.0.8")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/jarcoal/httpmock.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0smyci222rff5nj7knjcavp74y67k29j74d2xfnpxnybv5l8zw5r"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/jarcoal/httpmock"))
    (home-page "https://github.com/jarcoal/httpmock")
    (synopsis "httpmock")
    (description #f)
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
     '(#:import-path "gopkg.in/gavv/httpexpect.v1"))
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

(define-public go-github-com-ajg-form
  (package
    (name "go-github-com-ajg-form")
    (version "1.5.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ajg/form.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1d6sxzzf9yycdf8jm5877y0khmhkmhxfw3sc4xpdcsrdlc7gqh5a"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/ajg/form"))
    (home-page "https://github.com/ajg/form")
    (synopsis "form")
    (description #f)
    (license license:bsd-3)))

(define-public go-github-com-fatih-structs
  (package
    (name "go-github-com-fatih-structs")
    (version "1.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/fatih/structs.git")
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

(define-public go-github-com-gavv-monotime
  (package
    (name "go-github-com-gavv-monotime")
    (version "0.0.0-20190418164738-30dba4353424")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/gavv/monotime.git")
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

(define-public go-github-com-imkira-go-interpol
  (package
    (name "go-github-com-imkira-go-interpol")
    (version "1.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/imkira/go-interpol.git")
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

(define-public go-github-com-valyala-fasthttp
  (package
    (name "go-github-com-valyala-fasthttp")
    (version "1.23.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/valyala/fasthttp.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0wriavpgrn1rfi2xm7gndpnxqqh9378fgl4pwyfrkl7yklsnsn7m"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/valyala/fasthttp"))
    (propagated-inputs
     `(("go-github-com-andybalholm-brotli" ,go-github-com-andybalholm-brotli)
       ("go-github-com-klauspost-compress" ,go-github-com-klauspost-compress)
       ("go-github-com-valyala-bytebufferpool" ,go-github-com-valyala-bytebufferpool)))
    (home-page "https://github.com/valyala/fasthttp")
    (synopsis "fasthttp")
    (description #f)
    (license license:expat)))

(define-public go-github-com-andybalholm-brotli
  (package
    (name "go-github-com-andybalholm-brotli")
    (version "1.0.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/andybalholm/brotli.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1q8h9lx1lcwgkpxffkvmmx2kjq15lnfgvqs4npbq0bim5krqjqxx"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/andybalholm/brotli"))
    (home-page "https://github.com/andybalholm/brotli")
    (synopsis #f)
    (description #f)
    (license license:expat)))

(define-public go-github-com-klauspost-compress
  (package
    (name "go-github-com-klauspost-compress")
    (version "1.11.13")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/klauspost/compress.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "19l2rbh6vg0ly4qp2gn5lf39ijggf7asav7lxziikkhjqqb4223j"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/klauspost/compress"
       #:phases
       (modify-phases %standard-phases
         (delete 'reset-gzip-timestamps))))
    (home-page "https://github.com/klauspost/compress")
    (synopsis "compress")
    (description #f)
    (license license:bsd-3)))

(define-public go-github-com-valyala-bytebufferpool
  (package
    (name "go-github-com-valyala-bytebufferpool")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/valyala/bytebufferpool.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "01lqzjddq6kz9v41nkky7wbgk7f1cw036sa7ldz10d82g5klzl93"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/valyala/bytebufferpool"))
    (home-page "https://github.com/valyala/bytebufferpool")
    (synopsis #f)
    (description #f)
    (license license:expat)))

(define-public go-github-com-xeipuuv-gojsonschema
  (package
    (name "go-github-com-xeipuuv-gojsonschema")
    (version "1.2.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/xeipuuv/gojsonschema.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1mqiq0r8qw4qlfp3ls8073r6514rmzwrmdn4j33rppk3zh942i6l"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/xeipuuv/gojsonschema"))
    (propagated-inputs
     `(("go-github-com-xeipuuv-gojsonreference" ,go-github-com-xeipuuv-gojsonreference)))
    (native-inputs
     `(("go-github-com-stretchr-testify" ,go-github-com-stretchr-testify)))
    (home-page "https://github.com/xeipuuv/gojsonschema")
    (synopsis #f)
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
               (url "https://github.com/xeipuuv/gojsonreference.git")
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

(define-public go-github-com-xeipuuv-gojsonpointer
  (package
    (name "go-github-com-xeipuuv-gojsonpointer")
    (version "0.0.0-20190905194746-02993c407bfb")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/xeipuuv/gojsonpointer.git")
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

(define-public go-github-com-yalp-jsonpath
  (package
    (name "go-github-com-yalp-jsonpath")
    (version "0.0.0-20180802001716-5cc68e5049a0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/yalp/jsonpath.git")
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

(define-public go-github-com-yudai-gojsondiff
  (package
    (name "go-github-com-yudai-gojsondiff")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/yudai/gojsondiff.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0qnymi0027mb8kxm24mmd22bvjrdkc56c7f4q3lbdf93x1vxbbc2"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/yudai/gojsondiff"))
    (propagated-inputs
     `(("go-github-com-sergi-go-diff" ,go-github-com-sergi-go-diff)
       ("go-github-com-yudai-golcs" ,go-github-com-yudai-golcs)))
    (native-inputs
     `(("go-github-com-onsi-ginkgo" ,go-github-com-onsi-ginkgo)
       ("go-github-com-onsi-gomega" ,go-github-com-onsi-gomega)))
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
               (url "https://github.com/yudai/golcs.git")
               (commit (go-version->git-ref version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0mx6wc5fz05yhvg03vvps93bc5mw4vnng98fhmixd47385qb29pq"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/yudai/golcs"))
    (home-page "https://github.com/yudai/golcs")
    (synopsis "Go Longest Common Subsequence (LCS)")
    (description #f)
    (license license:expat)))

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
          "15bpx33d3ygya8dg8hbsn24h7acpajl27006pj8lw1c0bfvbnrl0"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "moul.io/http2curl"
       ;; TODO: Fix tests, unbundle everything
       #:tests? #f))
    (home-page "https://moul.io/http2curl")
    (synopsis "http2curl")
    (description #f)
    (license license:expat)))

(define-public go-github-com-keybase-stellarnet
  (package
    (name "go-github-com-keybase-stellarnet")
    (version "0.0.0-20200318171650-0b120a59a787")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/keybase/stellarnet.git")
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
