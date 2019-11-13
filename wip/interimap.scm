;;; Copyright © 2019 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (wip interimap)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system perl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages tls))

(define-public interimap
  (package
    (name "interimap")
    (version "0.4")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://git.guilhem.org/interimap")
               (commit (string-append "upstream/" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "159n09g4a11pgr6qb23i5kz1f3p5jx9fs2w8j8kqddf6cj4yk6pj"))))
    (build-system perl-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (bin  (string-append out "/bin"))
                    (doc  (string-append out "/share/doc/" ,name))
                    (perl (string-append out "/lib/perl5/site_perl/" ,(package-version perl) "/Net/IMAP")))
               (install-file "interimap" bin)
               (install-file "pullimap" bin)
               (install-file "lib/Net/IMAP/InterIMAP.pm" perl)
               (install-file "interimap.sample" doc)
               (install-file "pullimap.sample" doc))
             #t))
         (add-after 'install 'wrap-binaries
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/interimap")
                 `("PERL5LIB" ":" prefix
                   ,(map (lambda (i) (string-append (assoc-ref inputs i)
                                                    "/lib/perl5/site_perl/"
                                                    ,(package-version perl)))
                         '("perl-conf-libconfig" "perl-dbd-sqlite" "perl-dbi" "perl-net-ssleay"))))
               #t)
             ;(let* ((out  (assoc-ref outputs "out"))
             ;       (bin  (string-append out "/bin/"))
             ;       (path (getenv "PERL5LIB")))
             ;  (for-each (lambda (file)
             ;              (wrap-program file
             ;                            `("PERL5LIB" ":" ,(string-append out "/lib/perl5/site_perl/" ,(package-version perl)) ":" prefix (,path))))
             ;            (find-files bin "\\.*$"))
             ;  #t)
             )))
       #:tests? #f)) ; no tests
    (inputs
     `(("perl-conf-libconfig" ,perl-conf-libconfig)
       ("perl-dbd-sqlite" ,perl-dbd-sqlite)
       ("perl-dbi" ,perl-dbi)
       ("perl-net-ssleay" ,perl-net-ssleay)))
    (home-page "https://guilhem.org/interimap/")
    (synopsis "Fast bidirectional synchronization for QRESYNC-capable IMAP servers")
    (description
     "InterIMAP is a fast bidirectional synchronization program for
QRESYNC-capable IMAP4rev1 servers.  PullIMAP retrieves messages a remote IMAP
mailbox and deliver them to an SMTP session.")
    (license license:gpl3+)))

(define-public interimap-git
  (let ((commit "67e0d741f21bd589a2cbb4d23f07f5fb5eae889b")
        (revision "2"))
    (package
      (inherit interimap)
      (name "interimap-git")
      (version (git-version (package-version interimap) revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://git.guilhem.org/interimap")
                 (commit commit)))
          (file-name (git-file-name "interimap" version))
          (sha256
           (base32
            "0h38jq1hjjwhh0crm9nf01f5dk5xl4lxaiz6bb0324q58llqviki"))))
      (arguments
       (substitute-keyword-arguments (package-arguments interimap)
         ((#:tests? _ #t) #f) ; tests require running dovecot service
         ((#:phases phases)
          `(modify-phases ,phases
             (add-after 'unpack 'patch-source
               (lambda* (#:key inputs #:allow-other-keys)
                 (let ((coreutils (assoc-ref inputs "coreutils")))
                   (substitute* "tests/run"
                     (("PATH=.*") "")
                     )
                   )
                 #t))))
         ))
      )))
