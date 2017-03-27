;;; Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (wip quassel-irssi)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages irc)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls))

(define-public quassel-irssi
  (let ((commit "7b034e3a8084d08e87869a96795ab59aa4901c74")
        (revision "1"))
    (package
      (name "quassel-irssi")
      (version (string-append "0.0.0-" revision "." (string-take commit 7)))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/phhusson/quassel-irssi.git")
                 (commit commit)
                 (recursive? #t)))
          (file-name (string-append name "-" version "-checkout"))
          (sha256
           (base32
            "1lh3x91wp2qa4yv9psljfqbjsn045zi35776frk5k861mshryn6l"))))
      (build-system gnu-build-system)
      (arguments
       `(#:make-flags (list
                        (string-append "DESTDIR=" (assoc-ref %outputs "out"))
                        "CC=gcc")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'change-dir
             (lambda* (#:key inputs #:allow-other-keys)
               (let* ((irssi (assoc-ref inputs "irssi"))
                      (irssi-include (string-append irssi "/include/irssi")))
                 (substitute* "core/Makefile"
                   (("/usr/include/irssi/") irssi-include)))
               (chdir "core")
               #t))
           (delete 'configure)))) ; no configure
      (native-inputs `(("pkg-config" ,pkg-config)))
      (inputs
       `(("irssi" ,irssi)
         ("glib" ,glib)
         ("openssl" ,openssl)))
      (home-page "https://github.com/phhusson/quassel-irssi")
      (synopsis "Irssi plugin to connect to quassel core")
      (description "An irssi plugin to connect to quassel core.")
      (license license:gpl3+)))) ; with openssl linking exception
