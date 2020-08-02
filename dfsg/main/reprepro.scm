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

(define-module (dfsg main reprepro)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages gnupg))

(define-public reprepro
  (package
    (name "reprepro")
    (version "5.3.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://salsa.debian.org/brlink/reprepro.git/")
               (commit (string-append name "-" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1kn7m5rxay6q2c4vgjgm4407xx2r46skkkb6rn33m6dqk1xfkqnh"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; testtool not found
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (if tests?
               (with-directory-excursion "tests"
                 (invoke (which "sh") "test.sh"))
               #t)))
         (add-after 'install 'install-completions
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (bash (string-append out "/etc/bash_completion.d/"))
                    (zsh  (string-append out "/share/zsh/site-fucnctions/")))
               (mkdir-p bash)
               (mkdir-p zsh)
               (copy-file "docs/reprepro.bash_completion" (string-append bash "reprepro"))
               (copy-file "docs/reprepro.zsh_completion" (string-append zsh "_reprepro"))
               #t))))))
    (inputs
     `(("bdb" ,bdb)
       ("bzip2" ,bzip2)
       ("gpgme" ,gpgme)
       ("libarchive" ,libarchive)
       ("xz" ,xz)
       ("zlib" ,zlib)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)))
    (home-page "https://salsa.debian.org/brlink/reprepro")
    (synopsis "Debian package repository producer")
    (description "Reprepro is a tool to manage a repository of Debian packages
(@code{.deb}, @code{.udeb}, @code{.dsc}, ...).  It stores files either being
injected manually or downloaded from some other repository (partially) mirrored
into one pool/ hierarchy.  Managed packages and files are stored in a Berkeley
DB, so no database server is needed.  Checking signatures of mirrored
repositories and creating signatures of the generated Package indices is
supported.")
    (license license:gpl2)))
