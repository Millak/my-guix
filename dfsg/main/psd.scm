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

(define-module (dfsg main psd)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages rsync))

(define-public psd
  (package
    (name "psd")
    (version "6.40")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/graysky2/profile-sync-daemon")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1naiqcag89dprcidc8zjvik2gib9089skq93vf4hxvpgp85c4pzc"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f              ; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)    ; no configure script.
         (add-after 'unpack 'patch-source
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (glib:bin (assoc-ref inputs "glib:bin"))
                   (rsync (assoc-ref inputs "rsync"))
                   (grep (assoc-ref inputs "grep"))
                   (sed (assoc-ref inputs "sed")))
               (substitute* "common/profile-sync-daemon.in"
                 (("SHAREDIR=.*")
                  (string-append "SHAREDIR=\"" out "/share/psd\"\n"))
                 (("getent passwd \"\\$user\"")
                  (string-append grep "/bin/grep \"$user\" /etc/passwd"))
                 (("gdbus") (string-append glib:bin "/bin/gdbus"))
                 (("rsync") (string-append rsync "/bin/rsync")))
               (substitute* "common/psd-overlay-helper"
                 (("PATH=.*")
                  "PATH=/run/setuid-programs:/run/current-system/profile/bin\n"))
               (substitute* "common/psd-suspend-sync"
                 (("gdbus") (string-append glib:bin "/bin/gdbus"))
                 (("/usr") out))
               (substitute* (find-files "common/browsers" ".")
                 (("grep") (string-append grep "/bin/grep"))
                 (("sed") (string-append sed "/bin/sed"))))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "PREFIX" (assoc-ref outputs "out"))
             (setenv "COMPRESS_MAN" "0")
             (invoke "make" "install-bin" "install-man"))))))
    (inputs
     `(("glib:bin" ,glib "bin")
       ("rsync" ,rsync)))
    (home-page "https://github.com/graysky2/profile-sync-daemon")
    (synopsis "Symlinks and syncs browser profile dirs to RAM")
    (description "Profile-sync-daemon (psd) is a tiny pseudo-daemon designed to
manage your browser's profile in tmpfs and to periodically sync it back to your
physical disc (HDD/SSD).  This is accomplished via a symlinking step and an
innovative use of rsync to maintain back-up and synchronization between the two.
One of the major design goals of psd is a completely transparent user experience.")
    (license license:expat)))
