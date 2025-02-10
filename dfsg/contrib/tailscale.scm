;;; Copyright © 2022-2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2024 umanwizard <brennan@umanwizard.com>
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
  #:use-module (guix records)
  #:use-module (guix modules)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (gnu packages base)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-compression)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages prometheus)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (dfsg main golang)
  #:use-module (ice-9 match))

(define-record-type* <go-git-reference>
  go-git-reference make-go-git-reference
  go-git-reference?
  (url    go-git-reference-url)
  (commit go-git-reference-commit)
  (hash   go-git-reference-sha256))

(define-record-type* <go-url-reference>
  go-url-reference make-go-url-reference
  go-url-reference?
  (url  go-url-reference-url)
  (hash go-url-reference-hash))

(define* (go-fetch-vendored uri hash-algorithm hash-value name #:key system)
  (let ((src
          (match uri
                 (($ <go-git-reference> url commit hash)
                  (origin
                    (method git-fetch)
                    (uri (git-reference
                           (url url)
                           (commit commit)))
                    (sha256 hash)))
                 (($ <go-url-reference> url commit hash)
                  (origin
                    (method url-fetch)
                    (uri url)
                    (sha256 hash)))))
        (name (or name "go-git-checkout")))
    (gexp->derivation
      (string-append name "-vendored.tar.gz")
      (with-imported-modules (append '((guix build utils))
                                     %default-gnu-imported-modules)
        #~(begin
            (use-modules ((guix build gnu-build-system) #:prefix gnu:)
                         (guix build utils))
            (let ((inputs (list
                            #+go-1.23
                            #+tar
                            #+bzip2
                            #+gzip)))
              (set-path-environment-variable "PATH" '("/bin") inputs))
            ((assoc-ref gnu:%standard-phases 'unpack) #:source #$src)

            (setenv "GOCACHE" "/tmp/gc")
            (setenv "GOMODCACHE" "/tmp/gmc")
            (setenv "SSL_CERT_DIR" #+(file-append nss-certs "/etc/ssl/certs/"))

            (invoke "go" "mod" "vendor")

            (invoke "tar" "czvf" #$output
                    ;; Avoid non-determinism in the archive.
                    "--mtime=@0"
                    "--owner=root:0"
                    "--group=root:0"
                    "--sort=name"
                    "--hard-dereference"
                    ".")))
      #:hash hash-value
      #:hash-algo hash-algorithm)))

(define-public tailscale
  (package
    (name "tailscale")
    (version "1.78.3")
    (source (origin
              (method go-fetch-vendored)
              (uri (go-git-reference
                    (url "https://github.com/tailscale/tailscale")
                    (commit (string-append "v" version))
                    (hash
                     (base32
                      "1bn7g6kcmwmig6fl5i747r21x4xcf8xdd659r1d5ycp9w36dwrcz"))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1rb1qji7zw425ik7l5vz97gbdvxg8x3x44c6nnc1a6p0lj1bnfgy"))))
    (build-system go-build-system)
    (arguments
     (list
       #:go go-1.23
       #:install-source? #f
       #:tests? #f
       #:import-path "tailscale.com"
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'adjust-source
             (lambda* (#:key import-path #:allow-other-keys)
               (with-directory-excursion (string-append "src/" import-path)
                 ;; Tries to import "golang.org/x/crypto/ssh"
                 (delete-file "vendor/github.com/tailscale/golang-x-crypto/ssh/doc.go")
                 ;; Adjust the --version output:
                 (substitute* "version/version.go"
                   (("ERR-BuildInfo") "GNU-Guix")))))
           (add-after 'unpack 'adjust-calls-to-binaries
             (lambda* (#:key import-path inputs #:allow-other-keys)
               (with-directory-excursion (string-append "src/" import-path)
                 (define* (substitute-command-block* file command full-command)
                          (substitute* file
                            (((string-append "exec\\.Command\\(\"" command "\""))
                             (string-append "exec.Command(\"" full-command "\""))))
                 (substitute-command-block* "util/linuxfw/iptables.go"
                   "iptables" (search-input-file inputs "sbin/iptables"))
                 (substitute-command-block* "util/linuxfw/iptables.go"
                   "ip6tables" (search-input-file inputs "sbin/ip6tables"))
                 (substitute-command-block* "util/linuxfw/iptables_runner.go"
                   "modprobe" (search-input-file inputs "bin/modprobe"))
                 (substitute-command-block* "net/dns/openresolv.go"
                   "resolvconf" (search-input-file inputs "sbin/resolvconf"))
                 (substitute-command-block* "net/netutil/ip_forward.go"
                   "sysctl" (search-input-file inputs "sbin/sysctl"))
                 (substitute-command-block* "net/tstun/tun_linux.go"
                   "/sbin/modprobe" (search-input-file inputs "bin/modprobe"))
                 (substitute-command-block* "net/tstun/tun_linux.go"
                   "find" (search-input-file inputs "bin/find"))
                 (substitute-command-block* "wgengine/router/router_linux.go"
                   "ip" (search-input-file inputs "sbin/ip")))))
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
           (add-after 'install 'install-shell-completions
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out #$output)
                      (tailscale (string-append out "/bin/tailscale"))
                      (share (string-append out "/share"))
                      (bash (string-append out "/etc/bash_completion.d/tailscale"))
                      (fish (string-append
                              share "/fish/vendor_completions.d/tailscale.fish"))
                      (zsh (string-append share "/zsh/site-functions/_tailscale")))
                 (mkdir-p (dirname bash))
                 (mkdir-p (dirname fish))
                 (mkdir-p (dirname zsh))
                 (with-output-to-file bash
                   (lambda ()
                     (invoke tailscale "completion" "bash")))
                 (with-output-to-file fish
                   (lambda ()
                     (invoke tailscale "completion" "fish")))
                 (with-output-to-file zsh
                   (lambda ()
                     (invoke tailscale "completion" "zsh")))))))))
    (inputs
     (list findutils iproute iptables kmod openresolv procps)
     #;
     (list findutils iproute iptables kmod openresolv procps

           go-k8s-io-utils
           ;go-k8s-io-apiextensions-apiserver
           go-gopkg-in-yaml-v3
           ;go-github-com-tailscale-go-winio
           go-github-com-stretchr-testify
           go-github-com-prometheus-client-model
           go-github-com-mdlayher-socket
           go-github-com-gorilla-csrf
           go-github-com-fsnotify-fsnotify
           ;go-github-com-aleksi-pointer
           go-software-sslmate-com-src-go-pkcs12
           go-sigs-k8s-io-yaml
           ;go-sigs-k8s-io-controller-tools
           go-sigs-k8s-io-controller-runtime
           go-k8s-io-client-go
           ;go-k8s-io-apiserver
           go-k8s-io-apimachinery
           go-k8s-io-api
           go-honnef-co-go-tools
           go-gvisor-dev-gvisor
           ;go-gopkg-in-square-go-jose-v2
           ;go-golang-zx2c4-com-wireguard-windows
           ;go-golang-zx2c4-com-wintun
           go-golang-org-x-tools
           go-golang-org-x-time
           go-golang-org-x-term
           go-golang-org-x-sys
           go-golang-org-x-sync
           go-golang-org-x-oauth2
           go-golang-org-x-net
           go-golang-org-x-mod
           go-golang-org-x-exp
           go-golang-org-x-crypto
           go-go4-org-netipx
           go-go4-org-mem
           go-go-uber-org-zap
           go-github-com-vishvananda-netns
           go-github-com-u-root-u-root
           go-github-com-toqueteos-webbrowser
           go-github-com-tcnksm-go-httpstat
           ;go-github-com-tc-hib-winres
           ;go-github-com-tailscale-xnet
           ;go-github-com-tailscale-wireguard-go
           ;go-github-com-tailscale-wf
           ;go-github-com-tailscale-web-client-prebuilt
           ;go-github-com-tailscale-peercred
           go-github-com-tailscale-netlink
           ;go-github-com-tailscale-mkctr
           ;go-github-com-tailscale-hujson
           go-github-com-tailscale-goupnp
           go-github-com-tailscale-golang-x-crypto
           ;go-github-com-tailscale-goexpect
           ;go-github-com-tailscale-depaware
           ;go-github-com-tailscale-certstore
           ;go-github-com-studio-b12-gowebdav
           go-github-com-skip2-go-qrcode
           go-github-com-safchain-ethtool
           ;go-github-com-prometheus-prometheus
           go-github-com-prometheus-common
           go-github-com-prometheus-client-golang
           go-github-com-prometheus-community-pro-bing
           go-github-com-pkg-sftp
           go-github-com-pkg-errors
           go-github-com-peterbourgon-ff-v3
           go-github-com-mitchellh-go-ps
           go-github-com-miekg-dns
           go-github-com-mdlayher-sdnotify
           go-github-com-mdlayher-netlink
           go-github-com-mdlayher-genetlink
           go-github-com-mattn-go-isatty
           go-github-com-mattn-go-colorable
           go-github-com-kortschak-wol
           go-github-com-klauspost-compress
           go-github-com-kballard-go-shellquote
           go-github-com-jsimonetti-rtnetlink
           go-github-com-josharian-native
           ;go-github-com-jellydator-ttlcache-v3
           go-github-com-insomniacslk-dhcp
           ;go-github-com-inetaf-tcpproxy
           ;go-github-com-illarion-gonotify-v2
           go-github-com-hdevalence-ed25519consensus
           ;go-github-com-goreleaser-nfpm-v2
           go-github-com-google-uuid
           ;go-github-com-google-nftables
           go-github-com-google-gopacket
           ;go-github-com-google-go-containerregistry
           go-github-com-google-go-cmp
           ;go-github-com-golangci-golangci-lint
           go-github-com-golang-snappy
           go-github-com-golang-groupcache
           go-github-com-godbus-dbus-v5
           go-github-com-go-ole-go-ole
           ;go-github-com-go-logr-zapr
           ;go-github-com-go-json-experiment-json
           go-github-com-gaissmai-bart
           go-github-com-fxamacker-cbor-v2
           go-github-com-frankban-quicktest
           go-github-com-fogleman-gg
           esbuild ;go-github-com-evanw-esbuild
           ;go-github-com-elastic-crd-ref-docs
           ;go-github-com-dsnet-try
           go-github-com-djherbis-times
           ;go-github-com-distribution-reference
           ;go-github-com-digitalocean-go-smbios
           ;go-github-com-dblohm7-wingoes
           go-github-com-dave-patsy
           go-github-com-dave-courtney
           go-github-com-creack-pty
           go-github-com-coreos-go-systemd
           go-github-com-coreos-go-iptables
           go-github-com-coder-websocket
           go-github-com-cilium-ebpf
           ;go-github-com-bramvdbogaerde-go-scp
           go-github-com-aws-aws-sdk-go-v2-service-ssm
           go-github-com-aws-aws-sdk-go-v2-service-s3
           go-github-com-aws-aws-sdk-go-v2-feature-s3-manager
           go-github-com-aws-aws-sdk-go-v2-config
           go-github-com-aws-aws-sdk-go-v2
           go-github-com-atotto-clipboard
           go-github-com-anmitsu-go-shlex
           go-github-com-andybalholm-brotli
           go-github-com-alexbrainman-sspi
           go-github-com-akutz-memconn
           ;go-fyne-io-systray
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
    (version "1.80.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://pkgs.tailscale.com/stable/tailscale_"
                                  version "_amd64.tgz"))
              (sha256
               (base32 "0sq1zw657chwqgpy3kw6kydnccp8ns6b5a5dhkiacd3wpk61yfq8"))))
    (build-system copy-build-system)
    (arguments
     (list
       #:install-plan
       #~'(("tailscaled" "sbin/")
           ("tailscale" "bin/"))
       #:phases
       #~(modify-phases %standard-phases
           ;; This uses the host binaries when cross compiling
           #;
           (add-after 'install 'wrap-binary
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (wrap-program
                 (string-append (assoc-ref outputs "out") "/sbin/tailscaled")
                 `("PATH" ":" prefix (,(dirname (search-input-file
                                                  inputs "/sbin/iptables"))
                                      ,(dirname (search-input-file
                                                  inputs "/sbin/ip")))))))
           (add-after 'install 'install-shell-completions
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out #$output)
                      (tailscale (string-append out "/bin/tailscale"))
                      (share (string-append out "/share"))
                      (bash (string-append out "/etc/bash_completion.d/tailscale"))
                      (fish (string-append
                              share "/fish/vendor_completions.d/tailscale.fish"))
                      (zsh (string-append share "/zsh/site-functions/_tailscale")))
                 (mkdir-p (dirname bash))
                 (mkdir-p (dirname fish))
                 (mkdir-p (dirname zsh))
                 (with-output-to-file bash
                   (lambda ()
                     (invoke tailscale "completion" "bash")))
                 (with-output-to-file fish
                   (lambda ()
                     (invoke tailscale "completion" "fish")))
                 (with-output-to-file zsh
                   (lambda ()
                     (invoke tailscale "completion" "zsh")))))))))
    ;(inputs (list iproute iptables))
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
    (version "1.80.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://pkgs.tailscale.com/stable/tailscale_"
                                  version "_386.tgz"))
              (sha256
               (base32 "0hl7qnpqhipg7hqykhgir5njbck06swwcfc0xzz40kpxvhgsrjh5"))))
    (supported-systems '("i686-linux"))))

(define-public tailscale-bin-arm
  (package
    (inherit tailscale-bin-amd64)
    (name "tailscale-bin-arm")
    (version "1.80.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://pkgs.tailscale.com/stable/tailscale_"
                                  version "_arm.tgz"))
              (sha256
               (base32 "0jwsyhn18ixj94yg0zbz10l673v118d0dq5phim0khzrsgmkfrmr"))))
    (supported-systems '("armhf-linux"))))

(define-public tailscale-bin-arm64
  (package
    (inherit tailscale-bin-amd64)
    (name "tailscale-bin-arm64")
    (version "1.80.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://pkgs.tailscale.com/stable/tailscale_"
                                  version "_arm64.tgz"))
              (sha256
               (base32 "07icbn45lwlyf7d5kmzjy5iyz3iajls67cgqbqygnp62p35zrjjw"))))
    (supported-systems '("aarch64-linux"))))

(define-public tailscale-bin-riscv64
  (package
    (inherit tailscale-bin-amd64)
    (name "tailscale-bin-riscv64")
    (version "1.80.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://pkgs.tailscale.com/stable/tailscale_"
                                  version "_riscv64.tgz"))
              (sha256
               (base32 "01c9q47paz6fhbmv2bsr0ik615ysbb0fn5pnakd6k4hxb9yhh8wy"))))
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
