;;; Copyright © 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main viper)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu))

(define-public viper
  (let ((commit "edd02980f70019b1d01508467c671282561ed778")
        (revision "2"))
    (package
      (name "viper")
      (version (string-append "0.0.0-" revision "." (string-take commit 7)))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/girst/viiper")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0lf2ba3grl3i5m6k1hsamwz288sslhy7ydnrwc6gqjc7yl0jm5ab"))))
      (build-system gnu-build-system)
      (arguments
       '(#:tests? #f ; no test target
         #:make-flags (list "CC=gcc")
         #:phases
          (modify-phases %standard-phases
            (delete 'configure) ; no configure script
            (replace 'install
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (bin (string-append out "/bin")))
                  (install-file "viper" bin))
                #t)))))
      (home-page "https://gir.st/viper.htm")
      (synopsis "terminal + emoji = snek")
      (description
       "VIper - a snake clone for unicode-compatible terminals.")
      (license license:gpl3))))

(define-public viiper
  (deprecated-package "viiper" viper))
