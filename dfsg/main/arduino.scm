;;; Copyright © 2026 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (guix gexp)
  #:use-module (guix build-system go)
  #:use-module (gnu packages flashing-tools)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages libusb))


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
           go-github-com-arduino-pluggable-discovery-protocol-handler
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
                  go-github-com-arduino-pluggable-discovery-protocol-handler
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
           go-github-com-arduino-pluggable-discovery-protocol-handler
           go-github-com-s-urbaniak-uevent
           go-go-bug-st-serial
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
           go-go-bug-st-serial
           go-golang-org-x-exp))
    (home-page "https://github.com/arduino/serial-monitor")
    (synopsis "Arduino pluggable monitor for serial ports")
    (description "The serial-monitor tool is a command line program that
interacts via stdio.  It accepts commands as plain ASCII strings terminated
with LF @code{\\n} and sends response as JSON.")
    (license license:gpl3)))

;;;

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
                             go-github-com-arduino-pluggable-discovery-protocol-handler
                             go-github-com-hashicorp-mdns))
    (home-page "https://github.com/arduino/mdns-discovery")
    (synopsis "mdns-discovery")
    (description "MDNS (Bonjour) pluggable discovery tool.")
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
    (propagated-inputs
     (list go-github-com-arduino-go-properties-orderedmap
           go-github-com-arduino-pluggable-discovery-protocol-handler
           go-github-com-s-urbaniak-uevent
           go-go-bug-st-serial
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
           go-go-bug-st-serial
           go-golang-org-x-exp))
    (home-page "https://github.com/arduino/serial-monitor")
    (synopsis "Arduino pluggable monitor for serial ports")
    (description "Package main implements the serial monitor.")
    (license license:gpl3)))

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
