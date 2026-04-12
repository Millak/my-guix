;;; Copyright © 2026 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main arduino)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build utils)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages flashing-tools)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages nss)
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

(define-public arduino-cli
  (package
    (properties
     '((release-commit . "e39419312d7fb5fb7bf07d8e70c7d18a031ff464")
       (release-date . "2026-01-19T16:11:40Z")))
    (name "arduino-cli")
    (version "1.4.1")
    (source (origin
              (method go-fetch-vendored)
              (uri (go-git-reference
                    (url "https://github.com/arduino/arduino-cli")
                    (commit (string-append "v" version))
                    (hash
                     (base32
                      "07bl0ykcmx81pfcab4yy195qmcd5g376hlggjvpv42zs53piavfs"))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0p2dsbc64432pgc8rszny34wgak663zcr9ica5v23qriz5aaryx6"))))
    (build-system go-build-system)
    (arguments
     (list
       #:go go-1.24
       #:install-source? #f
       #:modules
       '((guix build go-build-system)
         (guix build utils)
         (ice-9 match))
       #:test-flags
       #~(list "-skip" (string-join
                         (list "TestRunScript"
                               "TestDummyMonitor"
                               ;; Needs network
                               "TestDownloadAndChecksums"
                               "TestParseReferenceCores"
                               "TestParseArgs"
                               "TestBuildInjectedInfo")
                         "|"))
       #:import-path "github.com/arduino/arduino-cli"
       #:build-flags
       #~(let ((base "github.com/arduino/arduino-cli/internal/"))
           (list (format #f "-ldflags=-X ~s -X ~s -X ~s"
                         (string-append base "version.versionString="
                                        "v" #$version)
                         (string-append base "version.commit="
                                        (string-take
                                          #$(assoc-ref properties 'release-commit) 7))
                         (string-append base "version.date="
                                        #$(assoc-ref properties 'release-date)))))
       #:phases
       #~(modify-phases %standard-phases
           (add-before 'check 'pre-check
             (lambda* (#:key import-path #:allow-other-keys)
               (with-directory-excursion (string-append "src/" import-path)
                 ;; These tests depend on being run from a git repository.
                 (delete-file-recursively "internal/integrationtest"))
               (setenv "HOME" (getcwd))))
           (add-after 'install 'install-shell-completions
             (lambda* (#:key native-inputs #:allow-other-keys)
               (for-each
                 (match-lambda
                   ((shell . path)
                    (mkdir-p (in-vicinity #$output (dirname path)))
                    (let ((binary
                            (if #$(%current-target-system)
                                (search-input-file native-inputs "bin/arduino-cli")
                                (in-vicinity #$output "bin/arduino-cli"))))
                      (with-output-to-file (in-vicinity #$output path)
                                           (lambda _
                                             (invoke binary "completion" shell))))))
                 '(("bash" . "share/bash-completion/completions/arduino-cli")
                   ("fish" . "share/fish/vendor_completions.d/arduino-cli.fish")
                   ("zsh"  . "share/zsh/site-functions/_arduino-cli"))))))))
    (native-inputs
      (if (%current-target-system)
          (list this-package)
          '()))
    (home-page "https://arduino.github.io/arduino-cli/latest/")
    (synopsis "Arduino command line tool")
    (description "Arduino CLI is an all-in-one solution that provides
Boards/Library Managers, sketch builder, board detection, uploader, and many
other tools needed to use any Arduino compatible board and platform from
command line or machine interfaces.")
    (license license:gpl3)))

;; ~/.arduino15/packages/builtin/tools/dfu-discovery/0.1.2/
(define-public arduino-dfu-discovery
  (package
    (name "dfu-discovery")
    (version "0.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/arduino/dfu-discovery")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hw7d3bpbgp848cn58c1sdb659p4bhqm1wiqrfx9niz8aynjjpns"))))
    (build-system go-build-system)
    (arguments
     (list
      ;#:go go-1.19
      #:install-source? #f
      #:import-path "github.com/arduino/dfu-discovery"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-source
            (lambda* (#:key import-path inputs #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                #;
                ((assoc-ref %standard-phases 'unpack)
                 #:source #$(package-source dfu-util)
                 #:unpack-path "dfu-util-0.11")
                (invoke "tar" "xf" #$(package-source dfu-util))
                ;; As seen in the Taskfile.yml
                (copy-file "dfu-util-0.11/src/dfuse_mem.c" "dfu-util_dfuse_mem.c")
                (copy-file "dfu-util-0.11/src/dfu_util.c" "dfu-util_dfu_util.c")
                (copy-file "dfu-util-0.11/src/quirks.c" "dfu-util_quirks.c")
                (substitute* "main.go"
                  (("/usr/local/include/libusb-1.0")
                   (search-input-directory inputs "include/libusb-1.0")))))))))
    (inputs
     (list go-github-com-arduino-go-properties-orderedmap
           ;go-github-com-arduino-go-win32-utils
           go-github-com-arduino-pluggable-discovery-protocol-handler-v2
           (package-source dfu-util)
           libusb))
    (home-page "https://github.com/arduino/dfu-discovery")
    (synopsis "Arduino pluggable discovery for dfu devices")
    (description
     "The @code{dfu-discovery} tool is a command line program that interacts via
stdio.  It accepts commands as plain ASCII strings terminated with LF @code{\\n}
and sends response as JSON.")
    (license license:gpl3)))

;; ~/.arduino15/packages/builtin/tools/mdns-discovery/1.0.12/
(define-public arduino-mdns-discovery
  (package
    (properties
     '((release-commit . "df767150a287ba6876fd720497337016ad1c66e0")
       (release-date . "2025-11-06T10:53:33Z")))
    (name "mdns-discovery")
    (version "1.0.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/arduino/mdns-discovery")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jl4z9j3k5z0w41jll2gnwakh000v9vs3vg7mwph3hizp9r8gr0a"))))
    (build-system go-build-system)
    (arguments
     (list
       ;#:go go-1.25
       #:install-source? #f
       #:import-path "github.com/arduino/mdns-discovery"
       #:build-flags
       #~(let ((base "github.com/arduino/mdns-discovery/"))
           (list (format #f "-ldflags=-X ~s -X ~s -X ~s"
                         (string-append base "version.Version="
                                        "v" #$version)
                         (string-append base "version.Commit="
                                        (string-take
                                          #$(assoc-ref properties 'release-commit) 7))
                         (string-append base "version.Timestamp="
                                        #$(assoc-ref properties 'release-date)))))))
    (inputs (list go-github-com-arduino-go-properties-orderedmap
                  go-github-com-arduino-pluggable-discovery-protocol-handler-v2
                  go-github-com-hashicorp-mdns))
    (home-page "https://github.com/arduino/mdns-discovery")
    (synopsis "mdns (bonjour) pluggable discovery")
    (description "MDNS (Bonjour) pluggable discovery tool.")
    (license license:gpl3)))

;; ~/.arduino15/packages/builtin/tools/serial-discovery/1.4.3/
(define-public arduino-serial-discovery
  (package
    (properties
     '((release-commit . "0e2e30d92624371b776db9ac5c1f69db979cc308")
       (release-date . "2025-11-06T10:54:34Z")))
    (name "serial-discovery")
    (version "1.4.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/arduino/serial-discovery")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12wzf96xdb6xwdhgpqp5s1nd25b8ss3i3pifl1y0vc0h2hmh4gdy"))))
    (build-system go-build-system)
    (arguments
     (list
       ;#:go go-1.25
       #:install-source? #f
       #:import-path "github.com/arduino/serial-discovery"
       #:build-flags
       #~(let ((base "github.com/arduino/serial-discovery/"))
           (list (format #f "-ldflags=-X ~s -X ~s -X ~s"
                         (string-append base "version.Version="
                                        "v" #$version)
                         (string-append base "version.Commit="
                                        (string-take
                                          #$(assoc-ref properties 'release-commit) 7))
                         (string-append base "version.Timestamp="
                                        #$(assoc-ref properties 'release-date)))))))
    (inputs
     (list go-github-com-arduino-go-properties-orderedmap
           go-github-com-arduino-pluggable-discovery-protocol-handler-v2
           go-github-com-s-urbaniak-uevent go-go-bug-st-serial
           go-golang-org-x-sys))
    (home-page "https://github.com/arduino/serial-discovery")
    (synopsis "Arduino IDE pluggable-discovery for Serial ports")
    (description "The serial-discovery tool is a command line program that
interacts via stdio.  It accepts commands as plain ASCII strings terminated
with LF @code{\\n} and sends response as JSON.")
    (license license:gpl3)))

;; ~/.arduino15/packages/builtin/tools/serial-monitor/0.15.0/
(define-public arduino-serial-monitor
  (package
    (properties
     '((release-commit . "13b8e8058fd5c3eb24e97cba091f3a897aa20afb")
       (release-date . "2025-03-18T11:28:46Z")))
    (name "serial-monitor")
    (version "0.15.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/arduino/serial-monitor")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wjcxia1mch9fjq0ikm0k1hrsllh09z0a5azlngcipyk27090wkm"))))
    (build-system go-build-system)
    (arguments
     (list
       ;#:go go-1.24
       #:install-source? #f
       #:import-path "github.com/arduino/serial-monitor"
       #:build-flags
       #~(let ((base "github.com/arduino/serial-monitor/"))
           (list (format #f "-ldflags=-X ~s -X ~s -X ~s"
                         (string-append base "version.Version="
                                        "v" #$version)
                         (string-append base "version.Commit="
                                        (string-take
                                          #$(assoc-ref properties 'release-commit) 7))
                         (string-append base "version.Timestamp="
                                        #$(assoc-ref properties 'release-date)))))))
    (inputs
     (list go-github-com-arduino-pluggable-monitor-protocol-handler
           go-go-bug-st-serial go-golang-org-x-exp))
    (home-page "https://github.com/arduino/serial-monitor")
    (synopsis "Arduino pluggable monitor for serial ports")
    (description "The serial-monitor tool is a command line program that
interacts via stdio.  It accepts commands as plain ASCII strings terminated
with LF @code{\\n} and sends response as JSON.")
    (license license:gpl3)))

;;;

(define go-github-com-arduino-go-paths-helper
  (package
    (name "go-github-com-arduino-go-paths-helper")
    (version "1.14.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/arduino/go-paths-helper")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mj4w272msnx2rckhryr288bd49cfmch6wrjmly2b6jb3fy1p321"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/arduino/go-paths-helper"
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install 'remove-tests
             (lambda* (#:key import-path #:allow-other-keys)
               (delete-file-recursively
                 (string-append "src/" import-path "/testdata")))))))
    (propagated-inputs (list go-github-com-stretchr-testify
                             go-golang-org-x-sys))
    (home-page "https://github.com/arduino/go-paths-helper")
    (synopsis "paths: a golang library to simplify handling of paths")
    (description
     "Paths is a library that provides a set of utilities to work with file paths in a
platform-independent way.  It includes functions for creating temporary
directories and files, handling null paths, and more.  It is designed to be used
in Go applications that require file system operations without worrying about
platform-specific details.")
    (license license:gpl3)))

(define go-github-com-arduino-go-properties-orderedmap
  (package
    (name "go-github-com-arduino-go-properties-orderedmap")
    (version "1.8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/arduino/go-properties-orderedmap")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11vrrj9ymkjjcjfaz078np1pa42rl041axxlsrd9iw3jzxx8jkrj"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/arduino/go-properties-orderedmap"))
    (propagated-inputs (list go-github-com-arduino-go-paths-helper
                             go-github-com-stretchr-testify))
    (home-page "https://github.com/arduino/go-properties-orderedmap")
    (synopsis "Security")
    (description
     "Package properties is a library for handling maps of hierarchical properties.
This library is mainly used in the Arduino platform software to handle
configurations made of key/value pairs stored in files with an INI like syntax,
for example:.")
    (license license:gpl2)))

(define go-github-com-arduino-mdns-discovery
  (package
    (name "go-github-com-arduino-mdns-discovery")
    (version "1.0.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/arduino/mdns-discovery")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jl4z9j3k5z0w41jll2gnwakh000v9vs3vg7mwph3hizp9r8gr0a"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.25
      #:import-path "github.com/arduino/mdns-discovery"))
    (propagated-inputs (list go-github-com-arduino-go-properties-orderedmap
                             go-github-com-arduino-pluggable-discovery-protocol-handler-v2
                             go-github-com-hashicorp-mdns))
    (home-page "https://github.com/arduino/mdns-discovery")
    (synopsis "mdns-discovery")
    (description "MDNS (Bonjour) pluggable discovery tool.")
    (license license:gpl3)))

(define go-github-com-arduino-pluggable-discovery-protocol-handler-v2
  (package
    (name "go-github-com-arduino-pluggable-discovery-protocol-handler")
    (version "2.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/arduino/pluggable-discovery-protocol-handler")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1v3wcydi1ccg2mkqv9mpdbcl0926c960q2hpx15yxbs8w7pp6f2s"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/arduino/pluggable-discovery-protocol-handler"
      #:unpack-path "github.com/arduino/pluggable-discovery-protocol-handler"))
    (propagated-inputs (list go-github-com-arduino-go-paths-helper
                             go-github-com-arduino-go-properties-orderedmap
                             go-github-com-stretchr-testify))
    (home-page
     "https://github.com/arduino/pluggable-discovery-protocol-handler")
    (synopsis "Pluggable Discovery Protocol Handler")
    (description
     "Package discovery is a library for handling the Arduino Pluggable-Discovery
protocol
(@@url{https://github.com/arduino/tooling-rfcs/blob/main/RFCs/0002-pluggable-discovery.md#pluggable-discovery-api-via-stdinstdout,https://github.com/arduino/tooling-rfcs/blob/main/RFCs/0002-pluggable-discovery.md#pluggable-discovery-api-via-stdinstdout}).")
    (license license:gpl3)))

(define go-github-com-arduino-pluggable-monitor-protocol-handler
  (package
    (name "go-github-com-arduino-pluggable-monitor-protocol-handler")
    (version "0.9.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/arduino/pluggable-monitor-protocol-handler")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "025pvqx0wmijdfilq8frif01gky5mn83slg8x4gzcdas1rnw5r74"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/arduino/pluggable-monitor-protocol-handler"))
    (propagated-inputs (list go-github-com-hashicorp-go-multierror))
    (home-page "https://github.com/arduino/pluggable-monitor-protocol-handler")
    (synopsis "Pluggable Monitor Protocol Handler")
    (description
     "Package monitor is a library for handling the Arduino Pluggable-Monitor protocol
(@@url{https://github.com/arduino/tooling-rfcs/blob/main/RFCs/0004-pluggable-monitor.md#pluggable-monitor-api-via-stdinstdout,https://github.com/arduino/tooling-rfcs/blob/main/RFCs/0004-pluggable-monitor.md#pluggable-monitor-api-via-stdinstdout}).")
    (license license:gpl3)))

(define go-github-com-arduino-serial-discovery
  (package
    (name "go-github-com-arduino-serial-discovery")
    (version "1.4.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/arduino/serial-discovery")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12wzf96xdb6xwdhgpqp5s1nd25b8ss3i3pifl1y0vc0h2hmh4gdy"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.25
      #:import-path "github.com/arduino/serial-discovery"))
    (propagated-inputs (list go-github-com-arduino-go-properties-orderedmap
                        go-github-com-arduino-pluggable-discovery-protocol-handler-v2
                        go-github-com-s-urbaniak-uevent go-go-bug-st-serial
                        go-golang-org-x-sys))
    (home-page "https://github.com/arduino/serial-discovery")
    (synopsis "Arduino pluggable discovery for serial ports")
    (description "Package main implements the serial discovery.")
    (license license:gpl3)))

(define go-github-com-arduino-serial-monitor
  (package
    (name "go-github-com-arduino-serial-monitor")
    (version "0.15.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/arduino/serial-monitor")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wjcxia1mch9fjq0ikm0k1hrsllh09z0a5azlngcipyk27090wkm"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/arduino/serial-monitor"))
    (propagated-inputs
     (list go-github-com-arduino-pluggable-monitor-protocol-handler
           go-go-bug-st-serial go-golang-org-x-exp))
    (home-page "https://github.com/arduino/serial-monitor")
    (synopsis "Arduino pluggable monitor for serial ports")
    (description "Package main implements the serial monitor.")
    (license license:gpl3)))

(define go-github-com-creack-goselect
  (package
    (name "go-github-com-creack-goselect")
    (version "0.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/creack/goselect")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0h2a8nip8gzw9c6wkgdgxw1v86hcafmb7iihhxcxb4571mnzl7zc"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/creack/goselect"))
    (home-page "https://github.com/creack/goselect")
    (synopsis "go-select")
    (description "select(2) implementation in Go.")
    (license license:expat)))

(define go-github-com-s-urbaniak-uevent
  (package
    (name "go-github-com-s-urbaniak-uevent")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/s-urbaniak/uevent")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1r5zbpma4nsq7pvgjil9wyq07r2ddpn5569iv2qkkx3dlr4c0pvd"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/s-urbaniak/uevent"))
    (home-page "https://github.com/s-urbaniak/uevent")
    (synopsis "Golang uevent bindings")
    (description
     "Package uevent implements a Linux kernel uevent reader and decoder.  The reader
uses a Netlink (AF_NETLINK) socket to listen to kernel udev events (see
netlink(7)).  The decoder takes an arbitrary io.Reader and decodes Uevent
objects.")
    (license license:asl2.0)))

(define go-go-bug-st-serial
  (package
    (name "go-go-bug-st-serial")
    (version "1.6.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bugst/go-serial")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "062iqzvfn1nvji6fd8053xlsfh9favxwkpgyhyxsk9r4gnv06xw9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "go.bug.st/serial"))
    (propagated-inputs (list go-github-com-creack-goselect
                             go-github-com-stretchr-testify
                             go-golang-org-x-sys))
    (native-inputs (list (@ (gnu packages networking) socat)))
    (home-page "https://go.bug.st/serial")
    (synopsis "go.bug.st/serial")
    (description
     "Package serial is a cross-platform serial library for the go language.")
    (license license:bsd-3)))

