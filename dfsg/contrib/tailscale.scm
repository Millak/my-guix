;;; Copyright © 2022, 2023 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg contrib tailscale)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build utils)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix search-paths)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system go)
  #:use-module (gnu packages base)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages syncthing)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (dfsg main golang))

(define computed-origin-method (@@ (guix packages) computed-origin-method))
#;(define tailscale-vendored-sources
  (let* ((version "1.56.1")
         (upstream-source
           (origin
             (method git-fetch)
             (uri (git-reference
                    (url "https://github.com/tailscale/tailscale")
                    (commit (string-append "v" version))))
             (file-name (git-file-name "tailscale" version))
             (sha256
              (base32 "0h4b153vl7gslfyf6f842i9s0vq6m74hqjvkhhlcnddgy91kkjch")))))
    (computed-file (string-append "tailscale-vendored-sources-" version ".tar.xz")
      (with-imported-modules '((guix build utils))
        #~(begin
            (use-modules (guix build utils)
                         ;(ice-9 ftw)
                         (ice-9 match))
            (set-path-environment-variable
              "PATH" '("bin")
              (list #+(canonical-package gzip)
                    #+(canonical-package tar)
                    #+curl
                    #+git-minimal
                    #+nss-certs
                    #+go))

            (for-each (match-lambda
                        ((env-var (files ...) separator type pattern)
                         (set-path-environment-variable env-var files
                                                        input-directories
                                                        #:separator separator
                                                        #:type type
                                                        #:pattern pattern)))
                      '#$(map search-path-specification->sexp
                              (package-transitive-native-search-paths
                                go)))

            (setenv "SOURCE_DATE_EPOCH" "1")
            ;(setenv "GOINSECURE" "*")
            (setenv "HOME" (getcwd))
            (setenv "USER" "homeless-shelter")
            ;(setenv "GOPROXY" "direct")
            ;(setenv "GO111MODULE" "on")
            ;(setenv "GOCACHE" (string-append (getcwd) "/go-cache"))
            (copy-recursively #+upstream-source
                              (string-append "tailscale-" #$version))
            (with-directory-excursion (string-append "tailscale-" #$version)
              (begin
                (setenv "CURL_CA_BUNDLE"
                        (string-append #+nss-certs "/etc/ssl/certs/ca-bundle.crt"))
                (setenv "GIT_SSL_CAINFO"
                        (string-append #+nss-certs "/etc/ssl/certs/ca-bundle.crt"))
                (setenv "SSL_CERT_DIR"
                        (string-append #+nss-certs "/etc/ssl/certs"))
                (setenv "SSL_CERT_FILE"
                        (string-append #+nss-certs "/etc/ssl/certs/ca-bundle.crt"))
                (setenv "GIT_EXEC_PATH"
                        (string-append #+git-minimal "/libexec/git-core"))
                (invoke "go" "mod" "vendor")

                (format #t "Creating the tarball ...~%")
                (force-output)
                (with-directory-excursion "../"
                  (invoke "tar" "czf" #$output
                          ;; avoid non-determinism in the archive
                          "--sort=name" "--mtime=@1"
                          "--owner=root:0" "--group=root:0"
                          (string-append "tailscale-" #$version))))))))
    #;(origin
      (method computed-origin-method)
      (file-name (string-append "tailscale-vendored-sources-" version ".tar.gz"))
      (sha256 #f)
      (uri
        (delay
          (with-imported-modules '((guix build utils))
            #~(begin
                (use-modules (guix build utils))
                (set-path-environment-variable
                  "PATH" '("bin")
                  (list #+(canonical-package gzip)
                        #+(canonical-package tar)
                        #+go
                        ))
                (setenv "HOME" (getcwd))
                (copy-recursively #+upstream-source
                                  (string-append "tailscale-" #$version))
                (with-directory-excursion (string-append "tailscale-" #$version)
                  (begin
                    (setenv "GIT_SSL_CAINFO"
                            (string-append #+nss-certs "/etc/ssl/certs/ca-bundle.crt"))
                    (setenv "SSL_CERT_FILE"
                            (string-append #+nss-certs "/etc/ssl/certs/ca-bundle.crt"))
                    (invoke "go" "mod" "vendor")

                    (format #t "Creating the tarball ...~%")
                    (force-output)
                    (with-directory-excursion "../"
                      (invoke "tar" "czf" #$output
                              ;; avoid non-determinism in the archive
                              "--sort=name" "--mtime=@0"
                              "--owner=root:0" "--group=root:0"
                              (string-append "tailscale-" #$version))))))))))
  ))

(define-public tailscale
  (package
    (name "tailscale")
    (version "1.56.1")
    ;(source tailscale-vendored-sources)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tailscale/tailscale")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0qlcsgzrk4hq9xcprmfkykk14myymzl64qkfkszy2l386im1xfr7"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (substitute* (find-files "." "\\.go$")
              (("github\\.com/tailscale/golang-x-crypto/ssh")
               "golang.org/x/crypto/ssh")
              (("github\\.com/tailscale/netlink")
               "github.com/vishvananda/netlink")
              )
            ))))
    (build-system go-build-system)
    (arguments
     (list
       #:go go-1.20
       #:install-source? #f
       #:import-path "tailscale.com"
       #:phases
       #~(modify-phases %standard-phases
           (replace 'build
             (lambda* (#:key import-path build-flags #:allow-other-keys)
               (for-each
                 (lambda (directory)
                   ((assoc-ref %standard-phases 'build)
                    #:build-flags build-flags
                    #:import-path directory))
                 (list "tailscale.com/cmd/tailscale"
                       "tailscale.com/cmd/tailscaled"))))
           (replace 'check
             (lambda* (#:key tests? import-path #:allow-other-keys)
               (for-each
                 (lambda (directory)
                   ((assoc-ref %standard-phases 'check)
                    #:tests? tests?
                    #:import-path directory))
                 (list "tailscale.com/cmd/tailscale"
                       "tailscale.com/cmd/tailscaled"))))
           )))
    #;(inputs
     (list
       ;software.sslmate.com/src/go-pkcs12
       ;sigs.k8s.io/yaml
       ;sigs.k8s.io/controller-runtime
       go-nhooyr-io-websocket
           go-k8s-io-client-go
           ;k8s.io/apimachinery
           ;k8s.io/api
           ;go-inet-af-wf
           ;inet.af/tcpproxy
           go-inet-af-peercred
           go-honnef-co-go-tools
           go-gvisor-dev-gvisor
           ;go-golang-zx2c4-com-wireguard-windows
           ;golang.zx2c4.com/wintun
           go-golang-org-x-tools
           go-golang-org-x-time
           go-golang-org-x-term
           go-golang-org-x-sys
           go-golang-org-x-sync
           go-golang-org-x-oauth2
           go-golang-org-x-net
           go-golang-org-x-exp
           go-golang-org-x-crypto
           go-go4-org-netipx
           go-go4-org-mem
           ;go.uber.org/zap
           go-github-com-vishvananda-netlink
           go-github-com-u-root-u-root
           go-github-com-toqueteos-webbrowser
           go-github-com-tcnksm-go-httpstat
           ;github.com/tc-hib/winres
           ;;;github.com/tailscale/wireguard-go
           ;go-github-com-tailscale-netlink
           ;go-github-com-tailscale-mkctr
           ;go-github-com-tailscale-hujson
           go-github-com-tailscale-goupnp
           ;go-github-com-tailscale-golang-x-crypto
           ;go-github-com-tailscale-goexpect
           ;go-github-com-tailscale-depaware
           ;go-github-com-tailscale-certstore
           go-github-com-skip2-go-qrcode
           ;github.com/pkg/errors
           go-github-com-pkg-sftp
           go-github-com-peterbourgon-ff-v3
           go-github-com-mitchellh-go-ps
           go-github-com-miekg-dns
           go-github-com-mdlayher-sdnotify
           go-github-com-mdlayher-netlink
           go-github-com-mdlayher-genetlink
           ;github.com/mattn/go-isatty
           ;github.com/mattn/go-colorable
           go-github-com-kortschak-wol
           go-github-com-klauspost-compress
           go-github-com-kballard-go-shellquote
           go-github-com-jsimonetti-rtnetlink
           ;github.com/josharian/native
           go-github-com-insomniacslk-dhcp
           ;;;github.com/illarion/gonotify
           go-github-com-iancoleman-strcase
           go-github-com-hdevalence-ed25519consensus
           ;go-github-com-goreleaser-nfpm           ; Try to do without this one
           go-github-com-google-uuid
           ;github.com/google/nftables
           ;github.com/google/go-containerregistry
           go-github-com-google-go-cmp
           go-github-com-golang-groupcache
           go-github-com-godbus-dbus-v5
           go-github-com-go-ole-go-ole
           ;github.com/go-logr/zapr
           ;github.com/go-json-experiment/json
           go-github-com-fxamacker-cbor-v2
           go-github-com-frankban-quicktest
           esbuild ;go-github-com-evanw-esbuild
           ;github.com/dsnet/try
           ;github.com/dblohm7/wingoes
           go-github-com-dave-jennifer
           go-github-com-creack-pty
           ;go-github-com-coreos-go-systemd try to do without this one
           go-github-com-coreos-go-iptables
           go-github-com-aws-aws-sdk-go-v2-service-ssm
           go-github-com-aws-aws-sdk-go-v2-service-s3
           go-github-com-aws-aws-sdk-go-v2-feature-s3-manager
           go-github-com-aws-aws-sdk-go-v2-config
           go-github-com-aws-aws-sdk-go-v2
           go-github-com-anmitsu-go-shlex
           go-github-com-andybalholm-brotli
           ;go-github-com-alexbrainman-sspi          ; Windows
           go-github-com-akutz-memconn
           ;github.com/Microsoft/go-winio
           go-filippo-io-mkcert))
    (home-page "https://github.com/tailscale/tailscale")
    (synopsis "Tailscale VPN client")
    (description "Tailscale lets you easily manage access to private resources,
quickly SSH into devices on your network, and work securely from anywhere in
the world.")
    (license license:bsd-3)))

;; TODO: Figure out how to do this like (librsvg-for-system).
(define-public tailscale-bin-amd64
  (package
    (name "tailscale-bin-amd64")
    (version "1.68.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://pkgs.tailscale.com/stable/tailscale_"
                                  version "_amd64.tgz"))
              (sha256
               (base32 "1qrprimabssr1y5bgsxhkhhnc1g1jdfv9spzxvbbrgs1gh3c7v8q"))))
    (build-system copy-build-system)
    (arguments
     (list
       #:install-plan
       #~'(("tailscaled" "sbin/")
           ("tailscale" "bin/"))
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'install 'wrap-binary
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (wrap-program
                 (string-append (assoc-ref outputs "out") "/sbin/tailscaled")
                 `("PATH" ":" prefix (,(dirname (search-input-file
                                                  inputs
                                                  "/sbin/iptables"))
                                       ,(dirname (search-input-file
                                                   inputs
                                                   "/sbin/ip"))))))))))
    (inputs (list iproute iptables))
    (home-page "https://github.com/tailscale/tailscale")
    (synopsis "Tailscale VPN client")
    (description "Tailscale lets you easily manage access to private resources,
quickly SSH into devices on your network, and work securely from anywhere in
the world.")
    ;(properties
    ; `(;(hidden? . #t)
    ;   (release-monitoring-url . "https://github.com/tailscale/tailscale/releases")
    ;   (upstream-name . "tailscale")))
    (supported-systems '("x86_64-linux"))
    (license license:bsd-3)))

(define-public tailscale-bin-386
  (package
    (inherit tailscale-bin-amd64)
    (name "tailscale-bin-386")
    (version "1.68.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://pkgs.tailscale.com/stable/tailscale_"
                                  version "_386.tgz"))
              (sha256
               (base32 "0jwkj1nmia2wciyib8fhgv5qjjysmhhlkyy3vqy7j2a8mj1pw21z"))))
    (supported-systems '("i686-linux"))))

(define-public tailscale-bin-arm
  (package
    (inherit tailscale-bin-amd64)
    (name "tailscale-bin-arm")
    (version "1.68.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://pkgs.tailscale.com/stable/tailscale_"
                                  version "_arm.tgz"))
              (sha256
               (base32 "102jwbgidgz7i3hpzsljpbwlww3fhkgdl6ivnqjhby9vyi4y7k8f"))))
    (supported-systems '("armhf-linux"))))

(define-public tailscale-bin-arm64
  (package
    (inherit tailscale-bin-amd64)
    (name "tailscale-bin-arm64")
    (version "1.68.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://pkgs.tailscale.com/stable/tailscale_"
                                  version "_arm64.tgz"))
              (sha256
               (base32 "0d8asxrhy5dywjvh374xdgah00g9vjmycgh8agw6412f89yrjk6j"))))
    (supported-systems '("aarch64-linux"))))

(define-public tailscale-bin-riscv64
  (package
    (inherit tailscale-bin-amd64)
    (name "tailscale-bin-riscv64")
    (version "1.68.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://pkgs.tailscale.com/stable/tailscale_"
                                  version "_riscv64.tgz"))
              (sha256
               (base32 "1nrk5wyagvrg74b8vzvgl7q1k8gwldpdabcrargqjqqm3nxykzqv"))))
    (supported-systems '("riscv64-linux"))))

#;
(define* (tailscale-bin-for-system #:optional
                                   (system (or (%current-system)
                                               (%current-target-system))))
  (cond ((target-x86-64?) tailscale-bin-amd64)
        ((target-x86-32?) tailscale-bin-386)
        ((target-aarch64?) tailscale-bin-arm64)
        ((target-riscv64?) tailscale-bin-riscv64)
        ;; Probably the least overhead for qemu
        (else tailscale-bin-arm)))
;(export tailscale-bin-for-system)

(define-public tailscale-with-newer-go-libraries
  (package
    (inherit (newer-go-libraries tailscale))
    (name "tailscale-with-newer-go-libraries")))

