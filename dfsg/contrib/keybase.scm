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
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system go)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages syncthing)
  #:use-module (dfsg main golang))

(define-public keybase
  (package
    (name "keybase")
    (version "5.8.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://github.com/keybase/client/releases/download/v"
               version "/keybase-v" version ".tar.xz"))
        (sha256
         (base32
          "0wccxgfmchwhd4zzi0g1wmyxdrxmbl2zispbj8744aff0lhpk2ay"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (with-directory-excursion "go/vendor"
              (delete-file-recursively "bazil.org")
              (delete-file-recursively "camlistore.org/pkg/buildinfo")
              (delete-file-recursively "camlistore.org/pkg/images")
              (delete-file-recursively "github.com/BurntSushi")
              (delete-file-recursively "github.com/PuerkitoBio/goquery")
              (delete-file-recursively "github.com/RoaringBitmap/roaring")
              (delete-file-recursively "github.com/StackExchange/wmi")
              (delete-file-recursively "github.com/akavel/rsrc")
              (delete-file-recursively "github.com/andybalholm/cascadia")
              (delete-file-recursively "github.com/antchfx/htmlquery")
              (delete-file-recursively "github.com/antchfx/xmlquery")
              (delete-file-recursively "github.com/antchfx/xpath")
              (delete-file-recursively "github.com/araddon/dateparse")
              (delete-file-recursively "github.com/asaskevich/govalidator")
              (delete-file-recursively "github.com/blang")
              (delete-file-recursively "github.com/blevesearch/bleve")
              (delete-file-recursively "github.com/blevesearch/go-porterstemmer")
              (delete-file-recursively "github.com/blevesearch/segment")
              (delete-file-recursively "github.com/btcsuite/btcutil")
              (delete-file-recursively "github.com/buger/jsonparser")
              (delete-file-recursively "github.com/coreos/go-systemd")
              (delete-file-recursively "github.com/coreos/pkg")
              (delete-file-recursively "github.com/couchbase/vellum")
              (delete-file-recursively "github.com/davecgh/go-spew")
              (delete-file-recursively "github.com/deckarep/golang-set")
              (delete-file-recursively "github.com/docopt/docopt-go")
              (delete-file-recursively "github.com/dustin/go-humanize")
              (delete-file-recursively "github.com/eapache/channels")
              (delete-file-recursively "github.com/eapache/queue")
              (delete-file-recursively "github.com/edsrzf/mmap-go")
              (delete-file-recursively "github.com/emirpasic/gods")
              (delete-file-recursively "github.com/etcd-io/bbolt")
              (delete-file-recursively "github.com/gammazero/deque")
              (delete-file-recursively "github.com/gammazero/workerpool")
              (delete-file-recursively "github.com/glycerine/go-unsnap-stream")
              (delete-file-recursively "github.com/go-errors/errors")
              (delete-file-recursively "github.com/go-ole/go-ole")
              (delete-file-recursively "github.com/go-sql-driver/mysql")
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
              (delete-file-recursively "github.com/keybase/go-ps")
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
              (delete-file-recursively "github.com/kyokomi/emoji")
              (delete-file-recursively "github.com/lib")
              (delete-file-recursively "github.com/manucorporat/sse")
              (delete-file-recursively "github.com/mattn")
              (delete-file-recursively "github.com/miekg/dns")
              (delete-file-recursively "github.com/mitchellh")
              (delete-file-recursively "github.com/mschoch/smat")
              (delete-file-recursively "github.com/nf/cr2")
              (delete-file-recursively "github.com/nfnt/resize")
              (delete-file-recursively "github.com/pelletier/go-buffruneio")
              (delete-file-recursively "github.com/philhofer/fwd")
              (delete-file-recursively "github.com/pkg/errors")
              (delete-file-recursively "github.com/pmezard")
              (delete-file-recursively "github.com/qrtz/nativemessaging")
              ;(delete-file-recursively "github.com/rcrowley")  ; undefined build failure
              (delete-file-recursively "github.com/rwcarlsen/goexif")
              (delete-file-recursively "github.com/saintfish/chardet")
              (delete-file-recursively "github.com/sergi/go-diff")
              (delete-file-recursively "github.com/shirou")
              (delete-file-recursively "github.com/shopspring/decimal")
              (delete-file-recursively "github.com/src-d/gcfg")
              (delete-file-recursively "github.com/stathat")
              (delete-file-recursively "github.com/stellar/go-xdr")
              ;(delete-file-recursively "github.com/stellar/go")    ; missing deprecated folders
              (delete-file-recursively "github.com/steveyen/gtreap")
              (delete-file-recursively "github.com/stretchr/testify")
              ;(delete-file-recursively "github.com/syndtr/goleveldb")  ; keybase fork, undefined build failure
              (delete-file-recursively "github.com/temoto/robotstxt")
              (delete-file-recursively "github.com/tinylib/msgp")
              (delete-file-recursively "github.com/urfave")
              (delete-file-recursively "github.com/vividcortex/ewma")
              (delete-file-recursively "github.com/willf")
              (delete-file-recursively "github.com/xanzy/ssh-agent")
              (delete-file-recursively "go.uber.org/zap")
              (delete-file-recursively "golang.org/x")
              (delete-file-recursively "google.golang.org")
              (delete-file-recursively "gopkg.in/src-d/go-billy.v4")
              ;(delete-file-recursively "gopkg.in/src-d/go-git.v4") ; keybase fork, undefined build failure
              (delete-file-recursively "rsc.io/qr")
              (delete-file-recursively "stathat.com/c/ramcache"))

            (for-each delete-file-recursively
                      (list "osx"
                            "shared"
                            "browser"       ; GUI
                            "protocol"      ; protocol generator and tester
                            "pvl-tools"
                            "media"
                            "packaging"))
            ;; Delete everything vendored EXCEPT for keybase code.
            ;; Use the bundled code from other keybase repositories.
            (mkdir-p "go-vendor/github.com/stellar")
            (mkdir-p "go-vendor/gopkg.in/src-d")
            (rename-file "go/vendor/github.com/keybase" "go-vendor/github.com/keybase")
            (rename-file "go/vendor/github.com/rcrowley" "go-vendor/github.com/rcrowley")              ; wrong version(?), undefined build failure
            (rename-file "go/vendor/github.com/stellar/go" "go-vendor/github.com/stellar/go")          ; missing deprecated folders
            (rename-file "go/vendor/github.com/syndtr" "go-vendor/github.com/syndtr")                  ; keybase fork, undefined build failure
            (rename-file "go/vendor/gopkg.in/src-d/go-git.v4" "go-vendor/gopkg.in/src-d/go-git.v4")    ; keybase fork; undefined build failure
            (delete-file-recursively "go/vendor")
            (mkdir-p "go/vendor/github.com/stellar")
            (mkdir-p "go/vendor/gopkg.in/src-d")
            (rename-file "go-vendor/github.com/keybase" "go/vendor/github.com/keybase")
            (rename-file "go-vendor/github.com/rcrowley" "go/vendor/github.com/rcrowley")
            (rename-file "go-vendor/github.com/stellar/go" "go/vendor/github.com/stellar/go")
            (rename-file "go-vendor/github.com/syndtr" "go/vendor/github.com/syndtr")
            (rename-file "go-vendor/gopkg.in/src-d/go-git.v4" "go/vendor/gopkg.in/src-d/go-git.v4")
            (delete-file-recursively "go-vendor")))))
    (build-system go-build-system)
    (arguments
     `(#:install-source? #f
       #:import-path "github.com/keybase/client/go/keybase"
       #:unpack-path "github.com/keybase/client"
       #:build-flags '("-tags" "production")
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key import-path build-flags #:allow-other-keys)
             (for-each
               (lambda (directory)
                 ((assoc-ref %standard-phases 'build)
                  #:build-flags build-flags
                  #:import-path directory))
               (list import-path
                     "github.com/keybase/client/go/kbfs/kbfsfuse"
                     "github.com/keybase/client/go/kbfs/kbfsgit/git-remote-keybase"
                     "github.com/keybase/client/go/kbfs/redirector"
                     "github.com/keybase/client/go/kbnm"))))
         (replace 'check
           (lambda* (#:key tests? import-path #:allow-other-keys)
             (for-each
               (lambda (directory)
                 ((assoc-ref %standard-phases 'check)
                  #:tests? tests?
                  #:import-path directory))
               (list import-path
                     "github.com/keybase/client/go/kbfs/kbfsfuse"
                     "github.com/keybase/client/go/kbfs/kbfsgit/git-remote-keybase"
                     "github.com/keybase/client/go/kbfs/redirector"
                     "github.com/keybase/client/go/kbnm"))))
         (add-after 'install 'install-license
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "src/github.com/keybase/client/LICENSE"
                             (string-append out "/share/doc/"
                                            ,name "-" ,version "/"))))))))
    (inputs
     `(("go-bazil-org-fuse" ,go-bazil-org-fuse)
       ("go-camlistore-org-pkg-buildinfo" ,go-camlistore-org-pkg-buildinfo) ; camlistore/pkg/images
       ("go-camlistore-org-pkg-images" ,go-camlistore-org-pkg-images)
       ("go-github-com-antchfx-htmlquery" ,go-github-com-antchfx-htmlquery)
       ("go-github-com-antchfx-xmlquery" ,go-github-com-antchfx-xmlquery)
       ("go-github-com-araddon-dateparse" ,go-github-com-araddon-dateparse)
       ("go-github-com-blang-semver" ,go-github-com-blang-semver)
       ("go-github-com-blevesearch-bleve" ,go-github-com-blevesearch-bleve)
       ("go-github-com-btcsuite-btcutil" ,go-github-com-btcsuite-btcutil)
       ("go-github-com-buger-jsonparser" ,go-github-com-buger-jsonparser)
       ("go-github-com-coreos-go-systemd-activation" ,go-github-com-coreos-go-systemd-activation)
       ("go-github-com-coreos-go-systemd-daemon" ,go-github-com-coreos-go-systemd-daemon)
       ("go-github-com-coreos-go-systemd-util" ,go-github-com-coreos-go-systemd-util)
       ("go-github-com-deckarep-golang-set" ,go-github-com-deckarep-golang-set)
       ("go-github-com-dustin-go-humanize" ,go-github-com-dustin-go-humanize-20150824)
       ("go-github-com-eapache-channels" ,go-github-com-eapache-channels)
       ("go-github-com-gammazero-workerpool" ,go-github-com-gammazero-workerpool)
       ("go-github-com-gobwas-glob" ,go-github-com-gobwas-glob)
       ("go-github-com-gocolly-colly-debug" ,go-github-com-gocolly-colly-debug)
       ("go-github-com-gocolly-colly-storage" ,go-github-com-gocolly-colly-storage)
       ("go-github-com-go-errors-errors" ,go-github-com-go-errors-errors)
       ("go-github-com-golang-groupcache-lru" ,go-github-com-golang-groupcache-lru)
       ("go-github-com-golang-groupcache-singleflight" ,go-github-com-golang-groupcache-singleflight)
       ("go-github-com-golang-mock-gomock" ,go-github-com-golang-mock-gomock)
       ("go-github-com-hashicorp-golang-lru" ,go-github-com-hashicorp-golang-lru)
       ("go-github-com-kennygrant-sanitize" ,go-github-com-kennygrant-sanitize)
       ("go-github-com-keybase-go-ps" ,go-github-com-keybase-go-ps)
       ("go-github-com-kr-text" ,go-github-com-kr-text)
       ("go-github-com-kyokomi-emoji" ,go-github-com-kyokomi-emoji)
       ("go-github-com-mattn-go-isatty" ,go-github-com-mattn-go-isatty)
       ("go-github-com-miekg-dns" ,go-github-com-miekg-dns)
       ("go-github-com-nfnt-resize" ,go-github-com-nfnt-resize)
       ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
       ("go-github-com-puerkitobio-goquery" ,go-github-com-puerkitobio-goquery)
       ("go-github-com-qrtz-nativemessaging" ,go-github-com-qrtz-nativemessaging)
       ("go-github-com-rcrowley-go-metrics" ,go-github-com-rcrowley-go-metrics)
       ("go-github-com-shirou-gopsutil" ,go-github-com-shirou-gopsutil)
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
       ("go-github-com-syndtr-goleveldb" ,go-github-com-syndtr-goleveldb)  ; use keybase fork instead
       ("go-github-com-vividcortex-ewma" ,go-github-com-vividcortex-ewma)
       ("go-github-com-temoto-robotstxt" ,go-github-com-temoto-robotstxt)
       ("go-golang-org-x-crypto" ,go-golang-org-x-crypto)
       ("go-golang-org-x-net" ,go-golang-org-x-net)
       ("go-golang-org-x-sync-errgroup" ,go-golang.org-x-sync-errgroup)
       ("go-golang.org-x-sync-semaphore" ,go-golang.org-x-sync-semaphore)
       ("go-golang-org-x-text" ,go-golang-org-x-text)
       ("go-golang-org-x-time" ,go-golang-org-x-time)
       ("go-google-golang-org-appengine-internal" ,go-google-golang-org-appengine-internal)
       ("go-google-golang-org-appengine-urlfetch" ,go-google-golang-org-appengine-urlfetch)
       ("go-gopkg-in-src-d-go-billy-v4" ,go-gopkg-in-src-d-go-billy-v4)
       ("go-gopkg-in-src-d-go-git-v4" ,go-gopkg-in-src-d-go-git-v4) ; use keybase fork instead
       ("go-rsc-io-qr" ,go-rsc-io-qr)
       ("go-stathat-com-c-ramcache" ,go-stathat-com-c-ramcache)))
    (home-page "https://keybase.io")
    (synopsis "Secure messaging and file-sharing")
    (description "Keybase is a safe, secure, and private app for everything you
do online.")
    (properties
      '((release-monitoring-url . "https://github.com/keybase/client/releases")))
    (license license:bsd-3)))

(define-public keybase-with-newer-go-libraries
  (package
    (inherit (newer-go-libraries keybase))
    (name "keybase-with-newer-go-libraries")))
