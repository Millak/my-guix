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

(define-module (dfsg main keybase)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build utils)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system go)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages syncthing))

;; TODO: Unbundle all the go dependencies.
(define-public keybase
  (package
    (name "keybase")
    (version "5.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://github.com/keybase/client/releases/download/v"
               version "/keybase-v" version ".tar.xz"))
        (sha256
         (base32
          "040jn7g3nq4qpf8kvizs40gc2cbdxsy6nkx4qpsc8agk032cgnlc"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (with-directory-excursion "go/vendor"
              (delete-file-recursively "bazil.org")
              ;(delete-file-recursively "camlistore.org/pkg/buildinfo")
              ;(delete-file-recursively "camlistore.org/pkg/images")
              (delete-file-recursively "github.com/BurntSushi")
              ;(delete-file-recursively "github.com/PuerkitoBio/goquery")
              ;(delete-file-recursively "github.com/RoaringBitmap/roaring")
              ;(delete-file-recursively "github.com/StackExchange/wmi")
              ;(delete-file-recursively "github.com/akavel/rsrc")
              ;(delete-file-recursively "github.com/andybalholm/cascadia")
              ;(delete-file-recursively "github.com/antchfx/htmlquery")
              ;(delete-file-recursively "github.com/antchfx/xmlquery")
              ;(delete-file-recursively "github.com/antchfx/xpath")
              ;(delete-file-recursively "github.com/araddon/dateparse")
              ;(delete-file-recursively "github.com/asaskevich/govalidator")
              (delete-file-recursively "github.com/blang")
              ;(delete-file-recursively "github.com/blevesearch/bleve")
              ;(delete-file-recursively "github.com/blevesearch/go-porterstemmer")
              ;(delete-file-recursively "github.com/blevesearch/segment")
              ;(delete-file-recursively "github.com/btcsuite/btcutil")
              ;(delete-file-recursively "github.com/buger/jsonparser")
              ;(delete-file-recursively "github.com/coreos/go-systemd")
              ;(delete-file-recursively "github.com/coreos/pkg")
              ;(delete-file-recursively "github.com/couchbase/vellum")
              ;(delete-file-recursively "github.com/davecgh/go-spew")
              ;(delete-file-recursively "github.com/deckarep/golang-set")
              ;(delete-file-recursively "github.com/docopt/docopt-go")
              ;(delete-file-recursively "github.com/dustin/go-humanize")
              ;(delete-file-recursively "github.com/eapache/channels")
              ;(delete-file-recursively "github.com/eapache/queue")
              ;(delete-file-recursively "github.com/edsrzf/mmap-go")
              ;(delete-file-recursively "github.com/emirpasic/gods")
              ;(delete-file-recursively "github.com/etcd-io/bbolt")
              ;(delete-file-recursively "github.com/gammazero/deque")
              ;(delete-file-recursively "github.com/gammazero/workerpool")
              ;(delete-file-recursively "github.com/glycerine/go-unsnap-stream")
              ;(delete-file-recursively "github.com/go-errors/errors")
              ;(delete-file-recursively "github.com/go-ole/go-ole")
              (delete-file-recursively "github.com/gobwas")
              ;(delete-file-recursively "github.com/gocolly/colly")
              (delete-file-recursively "github.com/golang/groupcache/lru")
              ;(delete-file-recursively "github.com/golang/groupcache")
              ;(delete-file-recursively "github.com/golang/mock")
              (delete-file-recursively "github.com/golang/protobuf")
              (delete-file-recursively "github.com/golang/snappy")
              ;(delete-file-recursively "github.com/hashicorp/golang-lru")
              ;(delete-file-recursively "github.com/jbenet/go-context")
              ;(delete-file-recursively "github.com/josephspurrier/goversioninfo")
              ;(delete-file-recursively "github.com/kennygrant/sanitize")
              ;(delete-file-recursively "github.com/kevinburke/ssh_config")
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
              ;(delete-file-recursively "github.com/manucorporat/sse")
              (delete-file-recursively "github.com/mattn")
              ;(delete-file-recursively "github.com/miekg/dns")
              (delete-file-recursively "github.com/mitchellh")
              ;(delete-file-recursively "github.com/mschoch/smat")
              ;(delete-file-recursively "github.com/nf/cr2")
              ;(delete-file-recursively "github.com/nfnt/resize")
              ;(delete-file-recursively "github.com/pelletier/go-buffruneio")
              ;(delete-file-recursively "github.com/philhofer/fwd")
              (delete-file-recursively "github.com/pkg/errors")
              ;(delete-file-recursively "github.com/pkg/xattrs")
              (delete-file-recursively "github.com/pmezard")
              ;(delete-file-recursively "github.com/qrtz/nativemessaging")
              ;(delete-file-recursively "github.com/rcrowley")
              ;(delete-file-recursively "github.com/rwcarlsen/goexif")
              ;(delete-file-recursively "github.com/saintfish/chardet")
              ;(delete-file-recursively "github.com/sergi/go-diff")
              (delete-file-recursively "github.com/shirou")
              ;(delete-file-recursively "github.com/shopspring/decimal")
              ;(delete-file-recursively "github.com/src-d/gcfg")
              (delete-file-recursively "github.com/stathat")
              ;(delete-file-recursively "github.com/stellar/go-xdr")
              ;(delete-file-recursively "github.com/stellar/go")
              ;(delete-file-recursively "github.com/steveyen/gtreap")
              ;(delete-file-recursively "github.com/stretchr/testify")
              ;(delete-file-recursively "github.com/syndtr")
              ;(delete-file-recursively "github.com/temoto/robotstxt")
              ;(delete-file-recursively "github.com/tinylib/msgp")
              (delete-file-recursively "github.com/urfave")
              (delete-file-recursively "github.com/willf")
              ;(delete-file-recursively "github.com/xanzy/ssh-agent")
              ;(delete-file-recursively "go.uber.org/zap/buffer")
              ;(delete-file-recursively "go.uber.org/zap/internal")
              ;(delete-file-recursively "go.uber.org/zap/zapcore")
              (delete-file-recursively "golang.org/x")
              (delete-file-recursively "google.golang.org")
              ;(delete-file-recursively "gopkg.in/src-d/go-billy.v4")
              ;(delete-file-recursively "gopkg.in/src-d/go-git.v4")
              ;(delete-file-recursively "rsc.io/qr/coding")
              ;(delete-file-recursively "rsc.io/qr/gf256")
              ;(delete-file-recursively "stathat.com/c/ramcache")
              )
            ;; Lets smallerize the source to audit less code and licenses.
            (delete-file-recursively "osx")
            (delete-file-recursively "shared/ios")
            ;; Delete everything for the GUI
            (delete-file-recursively "browser")
            ;; Delete the protocol generator and tester
            (delete-file-recursively "protocol")
            (delete-file-recursively "pvl-tools")
            ;; Remove non-free fonts.
            (with-directory-excursion "shared/fonts"
              (for-each (lambda (file)
                          (delete-file file))
                        (find-files "." "keybase.*ttf")))
            ;; Apparently we don't need any of this
            (delete-file-recursively "shared")
            (delete-file-recursively "media")
            (delete-file-recursively "packaging")
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
       ("go-github-com-blang-semver" ,go-github-com-blang-semver)
       ("go-github-com-burntsushi-toml" ,go-github-com-burntsushi-toml)
       ("go-github-com-gobwas-glob" ,go-github-com-gobwas-glob)
       ("go-github-com-golang-groupcache-lru" ,go-github-com-golang-groupcache-lru)
       ("go-github-com-golang-protobuf-proto" ,go-github-com-golang-protobuf-proto)
       ("go-github-com-golang-snappy" ,go-github-com-golang-snappy)
       ("go-github-com-kr-text" ,go-github-com-kr-text)
       ("go-github-com-lib-pq" ,go-github-com-lib-pq)
       ("go-github-com-mattn-go-isatty" ,go-github-com-mattn-go-isatty)
       ("go-github-com-mitchellh-go-homedir" ,go-github-com-mitchellh-go-homedir)
       ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
       ("go-github-com-pmezard-go-difflib" ,go-github-com-pmezard-go-difflib)
       ;("go-github-com-rcrowley-go-metrics" ,go-github-com-rcrowley-go-metrics)
       ("go-github-com-shirou-gopsutil" ,go-github-com-shirou-gopsutil)
       ("go-github-com-stathat-go" ,go-github-com-stathat-go)
       ;("go-github-com-syndtr-goleveldb" ,go-github-com-syndtr-goleveldb)
       ("go-github-com-urfave-cli" ,go-github-com-urfave-cli)
       ("go-github-com-willf-bitset" ,go-github-com-willf-bitset)
       ("go-golang-org-x-crypto" ,go-golang-org-x-crypto)
       ("go-golang-org-x-image" ,go-golang-org-x-image)
       ("go-golang-org-x-net" ,go-golang-org-x-net)
       ("go-golang.org-x-sync-errgroup" ,go-golang.org-x-sync-errgroup)
       ("go-golang.org-x-sync-semaphore" ,go-golang.org-x-sync-semaphore)
       ("go-golang-org-x-sys" ,go-golang-org-x-sys)
       ("go-golang-org-x-text" ,go-golang-org-x-text)
       ("go-golang-org-x-time" ,go-golang-org-x-time)
       ("go-google-golang-org-appengine" ,go-google-golang-org-appengine)))
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

(define-public go-github-com-tv42-httpunix
  (let ((commit "2ba4b9c3382c77e7b9ea89d00746e6111d142a22")
        (revision "1"))
    (package
      (name "go-github-com-tv42-httpunix")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tv42/httpunix")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0xbwpip2hsfhd2kd878jn5ndl8y1i9658lggha4x3xb5m1rsds9w"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/tv42/httpunix"))
      (home-page "https://github.com/tv42/httpunix")
      (synopsis "Go library to talk HTTP over Unix domain sockets")
      (description "Go library to talk HTTP over Unix domain sockets/")
      (license license:expat))))

(define-public go-camlistore-org-pkg
  (let ((commit "c55c8602d3cea4511081630e17bca7ed601abc44")
        (revision "1"))
    (package
      (name "go-camlistore-org-pkg")
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
             #t))))
      (build-system go-build-system)
      (arguments
       '(;#:unpack-path "camlistore.org"
         #:import-path "camlistore.org/pkg/images"))
      (home-page "https://perkeep.org/pkg/")
      (synopsis "Go library for personal storage system")
      (description "Camlistore is your personal storage system for life: a way
of storing, syncing, sharing, modelling and backing up content.")
      (license license:asl2.0))))
