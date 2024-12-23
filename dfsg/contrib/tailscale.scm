;;; Copyright © 2022-2024 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (gnu packages curl)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages version-control)
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
    (version "1.78.1")
    (source (origin
              (method go-fetch-vendored)
              (uri (go-git-reference
                    (url "https://github.com/tailscale/tailscale")
                    (commit (string-append "v" version))
                    (hash
                     (base32
                      "0fahkx6his4c8mc55n78y8xw7qm2mxlayjs376kcjc3p22zwcwhw"))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "11ixvhap14mf8vq02dx6avfk8zv68v6r8wsfhxs7pq727p4aa6ji"))))
    (build-system go-build-system)
    (arguments
     (list
       #:go go-1.23
       #:install-source? #f
       #:tests? #f
       #:import-path "tailscale.com"
       ;#:build-flags
       ;#~(list (string-append
       ;          "-tags="
       ;          "-X" (string-append "tailscale.com/version.longStamp=" #$version)
       ;          "-X" (string-append "tailscale.com/version.shortStamp=" #$version)))
       #:phases
       #~(modify-phases %standard-phases
           #;
           (add-after 'unpack 'fix-source
             (lambda* (#:key import-path #:allow-other-keys)
               (with-directory-excursion (string-append "src/" import-path)
                 (substitute* (cons* "ipn/ipnlocal/ssh.go"
                                     "ssh/tailssh/tailssh.go"
                                     (find-files "tempfork/gliderlabs/ssh" "\\.go$"))
                   (("github.com/tailscale/golang-x-crypto/ssh")
                    "golang.org/x/crypto/ssh"
                    ;(string-append
                    ;  "github.com/tailscale/golang-x-crypto/ssh\"\n"
                    ;  "        \"golang.org/x/crypto/ssh")
                    )))))
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
    (inputs (list iproute iptables))
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
    (version "1.78.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://pkgs.tailscale.com/stable/tailscale_"
                                  version "_amd64.tgz"))
              (sha256
               (base32 "12aa8zr8b8x374vg48p6lcfzfwjjf2k9ywvbrdrj2080d2yr3d1x"))))
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
    (version "1.78.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://pkgs.tailscale.com/stable/tailscale_"
                                  version "_386.tgz"))
              (sha256
               (base32 "1gpajhhfbm96f72smmivxl6k31v02yqmx635p2640xv1qalljkhw"))))
    (supported-systems '("i686-linux"))))

(define-public tailscale-bin-arm
  (package
    (inherit tailscale-bin-amd64)
    (name "tailscale-bin-arm")
    (version "1.78.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://pkgs.tailscale.com/stable/tailscale_"
                                  version "_arm.tgz"))
              (sha256
               (base32 "01fhp249s3fnl6p052l2n0dj3f3xf5mbvi5iyhp0ry3kj07nb5kf"))))
    (supported-systems '("armhf-linux"))))

(define-public tailscale-bin-arm64
  (package
    (inherit tailscale-bin-amd64)
    (name "tailscale-bin-arm64")
    (version "1.78.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://pkgs.tailscale.com/stable/tailscale_"
                                  version "_arm64.tgz"))
              (sha256
               (base32 "1qw7y34c31i7r8jh9v4g166k5vz6a5vb68lp6ynbx01gmh8sxc4f"))))
    (supported-systems '("aarch64-linux"))))

(define-public tailscale-bin-riscv64
  (package
    (inherit tailscale-bin-amd64)
    (name "tailscale-bin-riscv64")
    (version "1.78.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://pkgs.tailscale.com/stable/tailscale_"
                                  version "_riscv64.tgz"))
              (sha256
               (base32 "09dgy18sl87jmkdrjck9k2s9a5x842vv0b8pkncpqpqhzgvgfbkk"))))
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
