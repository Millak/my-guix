;;; Copyright © 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main culmus)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages fontutils))

(define-public culmus
  (package
    (name "culmus")
    (version "0.132")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://sourceforge.net/projects/"
                            "culmus/files/culmus/" version "/culmus-src-"
                            version ".tar.gz"))
        (sha256
         (base32
          "1djxalm26r7bcq33ckmfa15xfs6pmqzvcl64d5lqa1dl01bl4j4z"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:modules ((guix build utils)
                  (guix build gnu-build-system)
                  (srfi srfi-1)
                  (srfi srfi-26))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda _
             (let ((compile
                    (lambda (name ext)
                      (zero? (system*
                              "fontforge" "-lang=ff"
                              "-c" (string-append "Open('" name "');"
                                                  "Generate('"
                                                  (basename name "sfd") ext
                                                  "')"))))))
               ;; This part based on the fonts shipped in the non-source package.
               (every (lambda (name)
                        (compile name "ttf"))
                      (find-files "." "^[^Nachlieli].*\\.sfd$"))
               (every (lambda (name)
                        (compile name "otf"))
                      (find-files "." "^Nachlieli.*\\.sfd$"))
               #t)))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out     (assoc-ref %outputs "out"))
                    (ttf     (string-append out "/share/fonts/truetype"))
                    (otf     (string-append out "/share/fonts/opentype"))
                    (license (string-append out "/share/doc/" ,name)))
               (for-each (lambda (file)
                           (install-file file ttf))
                         (find-files "." "\\.ttf$"))
               (for-each (lambda (file)
                           (install-file file otf))
                         (find-files "." "\\.otf$"))
               (for-each (lambda (file)
                           (install-file file license))
                         '("GNU-GPL" "LICENSE" "LICENSE-BITSTREAM"))
               #t))))))
    (native-inputs
     `(("fontforge" ,fontforge)))
    (home-page "http://culmus.sourceforge.net/")
    (synopsis "TrueType and Type1 Hebrew Fonts for X11")
    (description "14 Hebrew trivial families. Contain ASCII glyphs from various
sources.  Those families provide a basic set of a serif (Frank Ruehl), sans
serif (Nachlieli) and monospaced (Miriam Mono) trivials.  Also included Miriam,
Drugulin, Aharoni, David, Hadasim etc.  Cantillation marks support is
available in Keter YG.")
    (license license:gpl2))) ; consult the LICENSE file included
