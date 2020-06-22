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

(define-module (dfsg main newsboat)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages syndication)
  #:use-module (gnu packages tls)
  #:use-module (srfi srfi-1))

(define-public newsboat-2.19
  (package
    (inherit newsboat)
    (name "newsboat")
    (version "2.19")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://newsboat.org/releases/" version
                            "/newsboat-" version ".tar.xz"))
        (sha256
         (base32
          "06rx80jii3r0ca1kcpa53c8l37mm0xhi4m9xsgvdl0xrbf14qj5s"))))
    (build-system cargo-build-system)
    (arguments
     `(#:modules ((guix build cargo-build-system)
                  (guix build utils)
                  ((guix build gnu-build-system) #:prefix gnu:))
       #:cargo-inputs
       (("rust-chrono" ,rust-chrono-0.4)
        ("rust-rand" ,rust-rand-0.5)
        ("rust-once-cell" ,rust-once-cell-0.1)
        ("rust-regex" ,rust-regex-1)
        ("rust-url" ,rust-url-1.7)
        ("rust-dirs" ,rust-dirs-1.0)
        ("rust-xdg" ,rust-xdg-2.2)
        ("rust-backtrace" ,rust-backtrace-0.3)
        ("rust-unicode-width" ,rust-unicode-width-0.1)
        ("rust-nom" ,rust-nom-4.2)
        ("rust-curl-sys" ,rust-curl-sys-0.4)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-gettext-rs" ,rust-gettext-rs-0.4)
        ("rust-natord" ,rust-natord-1.0)
        ("rust-libz-sys" ,rust-libz-sys-1.0)
        ("rust-smallvec" ,rust-smallvec-0.6)
        ("rust-clap" ,rust-clap-2)
        ("rust-gettext-sys" ,rust-gettext-sys-0.19))
       #:cargo-development-inputs
       (("rust-tempfile" ,rust-tempfile-3)
        ("rust-proptest" ,rust-proptest-0.7)
        ("rust-section-testing" ,rust-section-testing-0.0))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             ;; Don't demand an exact value of a dependency
             (substitute* "rust/libnewsboat/Cargo.toml"
               (("\"= ") "\""))
             #t))
         (add-after 'unpack 'find-openssl
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((openssl (assoc-ref inputs "openssl")))
               (setenv "OPENSSL_DIR" openssl)
               #t)))
         (add-after 'configure 'dont-vendor-self
           (lambda* (#:key vendor-dir #:allow-other-keys)
             ;; Don't keep the whole tarball in the vendor directory
             (delete-file-recursively
               (string-append vendor-dir "/" ,name "-" ,version ".tar.xz"))
             #t))
         (replace 'build
           (lambda* args
             ((assoc-ref gnu:%standard-phases 'build)
              #:make-flags
              (list (string-append "prefix=" (assoc-ref %outputs "out"))))))
         (replace 'check
           (lambda* args
             ((assoc-ref gnu:%standard-phases 'check)
              #:test-target "test"
              #:make-flags
              (list (string-append "prefix=" (assoc-ref %outputs "out"))))))
         (replace 'install
           (lambda* args
             ((assoc-ref gnu:%standard-phases 'install)
              #:make-flags
              (list (string-append "prefix=" (assoc-ref %outputs "out")))))))))
    (native-inputs
     `(("asciidoctor" ,ruby-asciidoctor)
       ("openssl" ,openssl)
       ,@(alist-delete "asciidoc" (package-native-inputs newsboat))))))

(define-public newsboat-2.20
  (package
    (inherit newsboat-2.19)
    (name "newsboat")
    (version "2.20")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://newsboat.org/releases/" version
                            "/newsboat-" version ".tar.xz"))
        (sha256
         (base32
          "00p4lwawk8gazwz1bsq23lvf65lq4fawnlc61fbz129slgaxb9yk"))))
    (arguments
     (substitute-keyword-arguments (package-arguments newsboat-2.19)
       ((#:cargo-inputs _)
        `(("rust-chrono" ,rust-chrono-0.4)
          ("rust-rand" ,rust-rand-0.6)
          ("rust-once-cell" ,rust-once-cell-1.2)
          ("rust-url" ,rust-url-2.1)
          ("rust-dirs" ,rust-dirs-2.0)
          ("rust-xdg" ,rust-xdg-2.2)
          ("rust-backtrace" ,rust-backtrace-0.3)
          ("rust-unicode-width" ,rust-unicode-width-0.1)
          ("rust-nom" ,rust-nom-5)
          ("rust-curl-sys" ,rust-curl-sys-0.4)
          ("rust-libc" ,rust-libc-0.2)
          ("rust-gettext-rs" ,rust-gettext-rs-0.4)
          ("rust-natord" ,rust-natord-1.0)
          ("rust-libz-sys" ,rust-libz-sys-1.0)
          ("rust-smallvec" ,rust-smallvec-0.6)
          ("rust-clap" ,rust-clap-2)
          ("rust-gettext-sys" ,rust-gettext-sys-0.19)))
       ((#:cargo-development-inputs _)
        `(("rust-tempfile" ,rust-tempfile-3)
          ("rust-proptest" ,rust-proptest-0.9)
          ("rust-section-testing" ,rust-section-testing-0.0)))
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'dont-vendor-self
             (lambda* (#:key vendor-dir #:allow-other-keys)
               ;; Don't keep the whole tarball in the vendor directory
               (delete-file-recursively
                 (string-append vendor-dir "/" ,name "-" ,version ".tar.xz"))
               #t))))))))
