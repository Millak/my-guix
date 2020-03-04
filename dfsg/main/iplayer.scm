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

(define-module (dfsg main iplayer)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system perl)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-web)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml))

(define-public get-iplayer
  (package
    (name "get-iplayer")
    (version "3.25")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/get-iplayer/get_iplayer")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1qjcxdpjr7ad82fvv17885ylw6as25dm0ghz7937g287h8y3qwr1"))))
    (build-system perl-build-system)
    (arguments
     `(#:tests? #f  ; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (man (string-append out "/share/man/man1")))
               (install-file "get_iplayer" bin)
               (install-file "get_iplayer.cgi" bin)
               (install-file "get_iplayer.1" man))
             #t))
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (perllib (string-append out "/lib/perl5/site_perl/"
                                            ,(package-version perl))))
               (wrap-program (string-append out "/bin/get_iplayer")
                 `("PERL5LIB" ":"
                   prefix (,(string-append perllib ":" (getenv "PERL5LIB")))))
               (wrap-program (string-append out "/bin/get_iplayer.cgi")
                 `("PERL5LIB" ":"
                   prefix (,(string-append perllib ":" (getenv "PERL5LIB")))))
               #t))))))
    (inputs
     `(("perl-mojolicious" ,perl-mojolicious)
       ("perl-lwp-protocol-https" ,perl-lwp-protocol-https)
       ("perl-xml-libxml" ,perl-xml-libxml)))
    (home-page "https://github.com/get-iplayer/get_iplayer")
    (synopsis "Download or stream available BBC iPlayer TV and radio programmes")
    (description "@code{get_iplayer} lists, searches and records BBC iPlayer
TV/Radio, BBC Podcast programmes.  Other third-party plugins may be available.
@code{get_iplayer} has three modes: recording a complete programme for later
playback, streaming a programme directly to a playback application, such as
mplayer; and as a @dfn{Personal Video Recorder} (PVR), subscribing to search
terms and recording programmes automatically.  It can also stream or record live
BBC iPlayer output.")
    (license license:gpl3+)))
