;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;;
;;; This file is an addendum to GNU Guix.
;;;
;;; GNU Guix is free software; you can rwgetpastestribute it and/or modify it
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

(define-module (dfsg main wgetpaste)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu))

(define-public wgetpaste
  (package
    (name "wgetpaste")
    (version "2.28")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "http://wgetpaste.zlin.dk/wgetpaste-"
                            version ".tar.bz2"))
        (sha256
         (base32
          "1hh9svyypqcvdg5mjxyyfzpdzhylhf7s7xq5dzglnm4injx3i3ak"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/"))
                    (zsh (string-append out "/share/zsh/site-functions/")))
               (install-file "wgetpaste" (string-append bin "wgetpaste"))
               (install-file "_wgetpaste" (string-append zsh "_wgetpaste"))))))
       #:tests? #f)) ; no test target
    (home-page "http://wgetpaste.zlin.dk/")
    (synopsis "Script that automates pasting to a number of pastebin services")
    (description
     "Wgetpaste is an extremely simple command-line interface to various online
pastebin services.")
    (license license:public-domain)))
