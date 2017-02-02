;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;;
;;; This file is part of GNU Guix.
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
;;;
;;; https://github.com/lfam/guix/raw/contrib-syncthing/gnu/packages/syncthing.scm

(define-module (dfsg main syncthing)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (gnu packages golang))

(define-public syncthing
  (package
    (name "syncthing")
    (version "0.14.21")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/syncthing/syncthing"
                                 "/releases/download/v" version
                                 "/syncthing-source-v" version ".tar.gz"))
             (sha256
              (base32
               "11b0qg00dwr1xs3wbkhcyx7lf3plzka7q4h87pn9yrr5ajw4kzp5"))))

    ;; TODO Make go-build-system.
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure) ; No ./configure script.

         ;; This is the directory structure recommended by Syncthings "guide to
         ;; building": https://docs.syncthing.net/dev/building.html
         ;; Can we simplify this step?
         (replace 'unpack
           (lambda* (#:key source #:allow-other-keys)
             (let ((dir "src/github.com/syncthing/"))
               (and (mkdir-p dir)
                    (with-directory-excursion dir
                      (zero? (system* "tar" "xvf" source)))))))

         (add-after 'unpack 'set-env
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "GOPATH" (string-append (getcwd)))
             ;; This should control where `go run build.go install` installs
             ;; things, but it seems to have no effect in this case.
             (setenv "GOBIN" (assoc-ref outputs "out"))
             #t))

         (add-before 'build 'increase-test-timeout
           (lambda _
             (substitute* "src/github.com/syncthing/syncthing/build.go"
               (("60s") "999s"))
             #t))

         (replace 'build
           (lambda _
             (with-directory-excursion "src/github.com/syncthing/syncthing"
               (zero? (system* "go" "run" "build.go"
                               ;; Disable Syncthing's built-in updater.
                               "-no-upgrade")))))
         (replace 'check
           (lambda _
             (with-directory-excursion "src/github.com/syncthing/syncthing"
               (zero? (system* "go" "run" "build.go" "test")))))

         ;; TODO Make this use `go run build.go install`.
         (replace 'install
           (lambda _
             (with-directory-excursion "src/github.com/syncthing/syncthing"
               (copy-recursively "bin" (string-append (assoc-ref %outputs "out")
                                                      "/bin")))))

         ;; TODO These man pages are generated from a different Git
         ;; repo, https://github.com/syncthing/docs.
         (add-after 'install 'install-doc
           (lambda* (#:key outputs source #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (man1 (string-append out "/share/man/man1"))
                    (man5 (string-append out "/share/man/man5"))
                    (man7 (string-append out "/share/man/man7"))
                    (src "src/github.com/syncthing/syncthing/man/"))
               (install-file (string-append src "syncthing.1") man1)
               (install-file (string-append src "syncthing-config.5") man5)
               (install-file (string-append src "syncthing-stignore.5") man5)
               (install-file (string-append src "syncthing-bep.7") man7)
               (install-file (string-append src "syncthing-device-ids.7") man7)
               (install-file (string-append src "syncthing-event-api.7") man7)
               (install-file (string-append src "syncthing-faq.7") man7)
               (install-file (string-append src "syncthing-globaldisco.7") man7)
               (install-file (string-append src "syncthing-localdisco.7") man7)
               (install-file (string-append src "syncthing-networking.7") man7)
               (install-file (string-append src "syncthing-relay.7") man7)
               (install-file (string-append src "syncthing-rest-api.7") man7)
               (install-file (string-append src "syncthing-security.7") man7)
               (install-file (string-append src "syncthing-versioning.7") man7)
             #t))))))
    (native-inputs
     `(("go" ,go)))
    (synopsis "Decentralized filesystem synchronization")
    (description "Syncthing is a peer-to-peer file synchronization tool that
supports a wide variety of computing platforms.  It uses the Block Exchange
Protocol.")
    (home-page "https://syncthing.net")
    ;; TODO Either delete the bundled dependencies or list their licenses here.
    (license mpl2.0)))
