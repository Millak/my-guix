;;; Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main viiper)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu))

(define-public viiper
  (let ((commit "6b7680f67b8362da98fbc11fcfc017ac1898ad22")
        (revision "1"))
    (package
      (name "viiper")
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
            "047gyssdhkflry9s71xay89qf74gs9gcdcsvd2j74qyp2jidwmgg"))))
      (build-system gnu-build-system)
      (arguments
       '(#:tests? #f ; no test target
         #:phases
          (modify-phases %standard-phases
            (delete 'configure) ; no configure script
            (replace 'install
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (bin (string-append out "/bin")))
                  (install-file "viiper" bin))
                #t)))))
      (home-page "https://www.viiper.org/")
      (synopsis "terminal + emoji = snek")
      (description
       "VIIper - a snake clone for unicode-compatible terminals.")
      (license license:gpl3))))
