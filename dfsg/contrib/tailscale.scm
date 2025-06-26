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
                            #+go-1.24
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
    (version "1.84.3")
    (source (origin
              (method go-fetch-vendored)
              (uri (go-git-reference
                    (url "https://github.com/tailscale/tailscale")
                    (commit (string-append "v" version))
                    (hash
                     (base32
                      "1cdmr6z351i2ammr82gvbs2x3cp9w75q0nzvvk91rsx2khvd8yyh"))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0p46r7r6wa45c6fx4m7bk9lagb8ndr0kp6yp7hf0p53w4qz9d3pv"))))
    (build-system go-build-system)
    (arguments
     (list
       #:go go-1.24
       #:install-source? #f
       #:tests? #f
       #:import-path "tailscale.com"
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'adjust-source
             (lambda* (#:key import-path #:allow-other-keys)
               (with-directory-excursion (string-append "src/" import-path)
                 ;; Adjust the --version output:
                 (substitute* "version/version.go"
                   (("ERR-BuildInfo") "GNU-Guix")))))
           (add-after 'unpack 'adjust-calls-to-binaries
             (lambda* (#:key import-path #:allow-other-keys)
               (substitute* (string-append "src/" import-path "/net/tstun/tun_linux.go")
                 (("/sbin/modprobe") "modprobe"))
               (substitute* (string-append "src/" import-path "/ipn/ipnlocal/c2n.go")
                 (("/usr/local/bin") (string-append #$output "/bin")))))
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
                     (invoke tailscale "completion" "zsh"))))))
           ;; Make sure this comes after 'install-binaries.
           (add-before 'strip 'wrap-binaries
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (for-each (lambda (binary)
                           (wrap-program binary
                             `("PATH" ":" prefix
                               (,(dirname (search-input-file
                                            inputs "/sbin/iptables"))
                                ,(dirname (search-input-file
                                            inputs "/bin/modprobe"))
                                ,(dirname (search-input-file
                                            inputs "/sbin/resolvconf"))
                                ,(dirname (search-input-file
                                            inputs "/sbin/sysctl"))
                                ,(dirname (search-input-file
                                            inputs "/bin/find"))
                                ,(dirname (search-input-file
                                            inputs "/sbin/ip"))))))
                         (find-files (string-append #$output "/bin")))))
           #$@(if (%current-target-system)
                  #~((add-after 'setup-go-environment 'fix-go-environment
                       (lambda _
                         (unsetenv "GOBIN")))
                     (add-before 'install-shell-completions 'install-binaries
                       (lambda _
                         (for-each (lambda (binary)
                                     (install-file binary (string-append #$output "/bin")))
                                   (find-files "bin")))))
                  #~()))))
    (inputs
     (list findutils iproute iptables kmod openresolv procps))
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
       #~'(("tailscaled" "bin/")
           ("tailscale" "bin/"))
       #:phases
       #~(modify-phases %standard-phases
           ;; This uses the host binaries when cross compiling because of the copy-build-system.
           (add-after 'install 'wrap-binary
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (wrap-program
                 (string-append (assoc-ref outputs "out") "/bin/tailscaled")
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
