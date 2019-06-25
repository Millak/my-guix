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

(define-module (dfsg main evisum)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages enlightenment)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config))

(define-public evisum
  (package
    (name "evisum")
    (version "0.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://download.enlightenment.org/rel/apps/"
                            "evisum/evisum-" version ".tar.xz"))
        (sha256
         (base32
          "1lj62n896kablsl687c66yxrwajrh6ralb3y6nmcqv34pglnigca"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f   ; no tests
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ; no configure phase
         (add-after 'unpack 'set-environmental-variables
           (lambda _ (setenv "CC" (which "gcc")) #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("efl" ,efl)
       ("perl" ,perl)))
    (home-page "https://www.enlightenment.org")
    (synopsis "EFL process viewer")
    (description
     "This is a process monitor and system monitor using the
@dfn{Enlightenment Foundation Libraries} (EFL).")
    (license license:bsd-2)))

;(define-public evisum-git
;  (let ((commit "7262b879165f5c156269265618427aae45bb5c91")
;        (revision "1"))
;    (package
;      (inherit evisum)
;      (name "evisum-git")
;      (version (string-append (package-version evisum) "-"
;                              revision "." (string-take commit 7)))
;      (source
;        (origin
;          (method git-fetch)
;          (uri (git-reference
;                 (url "https://git.enlightenment.org/apps/evisum.git")
;                 (commit commit)))
;          (file-name (git-file-name name version))
;          (sha256
;           (base32
;            "1vdb875wz8b825mi6pngj7jdqa6bvfdw6hbdbjv2lls4qrhlwbmd")))))))
