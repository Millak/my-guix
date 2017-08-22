;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main empc)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages enlightenment)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages mpd)
  #:use-module (gnu packages pkg-config))

(define-public empc
  (let ((commit "32c53b57e4032e24d138c351cd9cd27b4ba166d0")
        (revision "2"))
  (package
    (name "empc")
    (version (string-append "0.99.0.0-" revision "." (string-take commit 7)))
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://git.enlightenment.org/apps/empc.git/")
               (commit commit)))
        (file-name (string-append name "-" version "-checkout"))
        (sha256
         (base32
          "0vxy5d5gvxlfpa9dl9k8wr6azd7d5mixqh7n12f9vj2ydqm0g0n0"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'autoconf
           (lambda _ (zero? (system* "autoreconf" "-vfi"))))
         (add-after 'unpack 'set-home-directory
           ;; FATAL: Cannot create run dir '/homeless-shelter/.run' - errno=2
           (lambda _ (setenv "HOME" "/tmp") #t))
         (add-after 'unpack 'skip-sl-translation
           ;; This one fails randomly
           (lambda _ (substitute* "po/LINGUAS"
                       (("sl") ""))
                     #t)))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext" ,gettext-minimal)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("efl" ,efl)
       ("libmpdclient" ,libmpdclient)))
    (home-page "https://www.enlightenment.org")
    (synopsis "Enlightenment powered mpd client")
    (description "The best fucking music player ever written")
    (license license:gpl3+))))
