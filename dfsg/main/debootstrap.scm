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

(define-module (dfsg main debootstrap)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages wget))

(define-public debootstrap
  (package
    (name "debootstrap")
    (version "1.0.93")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://anonscm.debian.org/cgit/d-i/debootstrap.git")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1jxq91602a152c56l2f8kzkiszp26cziqddcs4v695bcif72kfz6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (add-after 'unpack 'patch-source
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out    (assoc-ref outputs "out"))
                   (debian (assoc-ref %build-inputs "debian"))
                   (ubuntu (assoc-ref %build-inputs "ubuntu")))
               (substitute* "scripts/sid"
                 (("/usr") debian))
               (substitute* "scripts/gutsy"
                 (("/usr") ubuntu))
               (substitute* "debootstrap"
                 (("=/usr") (string-append "=" out))
                 (("@VERSION@") ,version))
               (substitute* "functions"
                 (("wget ") (string-append (which "wget") " ")))
               #t)))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (copy-recursively "scripts"
                                 (string-append out "/share/debootstrap/scripts"))
               (install-file "functions" (string-append out "/share/debootstrap"))
               (install-file "debootstrap" (string-append out "/sbin"))
               (install-file "debootstrap.8" (string-append out "/share/man/man8"))
               #t))))
       #:tests? #f)) ; no tests
    (inputs
     `(("debian" ,debian-archive-keyring)
       ("ubuntu" ,ubuntu-keyring)
       ("wget" ,wget)))
    ;; The following are required for debootstrap to work correctly
    (propagated-inputs
     `(("binutils" ,binutils)
       ("gnupg" ,gnupg)
       ("perl" ,perl)))
    (home-page "https://anonscm.debian.org/cgit/d-i/debootstrap.git")
    (synopsis "Bootstrap a basic Debian system")
    (description "Debootstrap is used to create a Debian base system from
scratch, without requiring the availability of @code{dpkg} or @code{apt}.
It does this by downloading .deb files from a mirror site, and carefully
unpacking them into a directory which can eventually be chrooted into.")
    (license license:gpl2)))

(define-public jetring
  (package
    (name "jetring")
    (version "0.25")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://debian/pool/main/j/" name "/"
                            name "_" version ".tar.xz"))
        (sha256
         (base32
          "0shcnnw0h31b08vmnvf18ni33dg40w18wv9smb69vkklz3h4jhpw"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure) ; no configure script
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (man (string-append out "/share/man")))
               (for-each (lambda (file)
                           (install-file file (string-append out "/bin/")))
                         '("jetring-accept" "jetring-apply" "jetring-build"
                           "jetring-checksum" "jetring-diff" "jetring-explode"
                           "jetring-gen" "jetring-review" "jetring-signindex"))
               (for-each (lambda (file)
                           (install-file file (string-append man "/man1/")))
                         (find-files "." ".*\\.1$"))
               (install-file "jetring.7" (string-append man "/man7/"))
               #t))))
       #:tests? #f)) ; no test phase
    (native-inputs `(("gnupg" ,gnupg)))
    (inputs `(("perl" ,perl)))
    (home-page "https://joeyh.name/code/jetring/")
    (synopsis "Gpg keyring maintenance using changesets")
    (description
     "Jetring is a collection of tools that allow for gpg keyrings to be
maintained using changesets.  It was developed with the Debian keyring in mind,
and aims to solve the problem that a gpg keyring is a binary blob that's hard
for multiple people to collaboratively edit.
With jetring, changesets can be submitted, reviewed to see exactly what they
will do, applied, and used to build a keyring.  The origin of every change made
to the keyring is available for auditing, and gpg signatures can be used to
further secure things.")
    (license license:gpl2+)))

(define-public debian-archive-keyring
  (package
    (name "debian-archive-keyring")
    (version "2017.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://debian/pool/main/d/" name "/"
                            name "_" version ".tar.xz"))
        (sha256
         (base32
          "1pdwgipfi0y4svhxlw8arhq792f1g3vlmw4raphizy7sa65vd4ca"))))
    (build-system gnu-build-system)
    (arguments
     '(#:test-target "verify-results"
       #:parallel-build? #f ; has race conditions
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ; no configure script
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (apt (string-append out "/etc/apt/trusted.gpg.d/"))
                    (key (string-append out "/share/keyrings/")))
               (install-file "keyrings/debian-archive-keyring.gpg" key)
               (install-file "keyrings/debian-archive-removed-keys.gpg" key)
               (for-each (lambda (file)
                           (install-file file apt))
                         (find-files "trusted.gpg" "\\.gpg$")))
             #t)))))
    (native-inputs
     `(("gnupg" ,gnupg)
       ("jetring" ,jetring)))
    (home-page "https://packages.qa.debian.org/d/debian-archive-keyring.html")
    (synopsis "GnuPG archive keys of the Debian archive")
    (description
     "The Debian project digitally signs its Release files.  This package
contains the archive keys used for that.")
    (license (list license:public-domain ; the keys
                   license:gpl2+)))) ; see debian/copyright

(define-public ubuntu-keyring
  (package
    (name "ubuntu-keyring")
    (version "2018.02.28")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://launchpad.net/ubuntu/+archive/primary/"
                            "+files/" name "_" version ".tar.gz"))
        (sha256
         (base32
          "1zj3012cz7rlx9pm39wnwa0lmi1h38n6bkgbz81vnmcsvqsc9a3a"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let* ((out (assoc-ref %outputs "out"))
                          (apt (string-append out "/etc/apt/trusted.gpg.d/"))
                          (key (string-append out "/share/keyrings/")))
                     (setenv "PATH" (string-append
                                      (assoc-ref %build-inputs "gzip") "/bin:"
                                      (assoc-ref %build-inputs "tar") "/bin"))
                     (invoke "tar" "xvf" (assoc-ref %build-inputs "source"))
                     (for-each (lambda (file)
                                 (install-file file key)
                                 (install-file file apt))
                               (find-files "." "\\.gpg$")))
                   #t)))
    (native-inputs
     `(("tar" ,tar)
       ("gzip" ,gzip)))
    (home-page "https://launchpad.net/ubuntu/+source/ubuntu-keyring")
    (synopsis "GnuPG keys of the Ubuntu archive")
    (description
     "The Ubuntu project digitally signs its Release files.  This package
contains the archive keys used for that.")
    (license (list license:public-domain ; the keys
                   license:gpl2+)))) ; see debian/copyright
