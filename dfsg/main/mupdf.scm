;;; Copyright © 2018, 2020 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main mupdf)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (gnu packages pdf)
  #:use-module (srfi srfi-1))

(define-public my-mupdf
  (package
    (inherit mupdf)
    (name "my-mupdf")
    (source
      (origin
        (inherit (package-source mupdf))
        (snippet
         ;; We keep lcms2 since it is different than our lcms.
         ;; In contrast to upstream mupdf, we also keep mujs.
         '(begin
            (for-each
              (lambda (dir)
                (delete-file-recursively (string-append "thirdparty/" dir)))
              '("curl" "freeglut" "freetype" "harfbuzz" "jbig2dec"
                "libjpeg" "openjpeg" "zlib"))
            #t))))
    (arguments
      (substitute-keyword-arguments (package-arguments mupdf)
        ((#:phases phases)
         `(modify-phases ,phases
            (add-after 'install 'install-symlink
              (lambda* (#:key outputs #:allow-other-keys)
                (let ((out (assoc-ref outputs "out")))
                  (with-directory-excursion (string-append out "/bin")
                    (delete-file "mutool")
                    (delete-file "muraster")
                    (symlink "mupdf-gl" "mupdf"))
                  #t)))))
        ((#:make-flags flags)
         `(delete "USE_SYSTEM_MUJS=yes" ,flags))))
    (inputs
     (fold alist-delete (package-inputs mupdf)
           '("curl" "mujs" "openssl")))))

(define-public my-mupdf-1.17
  (package
    (inherit my-mupdf)
    (version "1.17.0")
    (source
      (origin
        (inherit (package-source my-mupdf))
        (uri (string-append "https://mupdf.com/downloads/archive/mupdf-"
                            version "-source.tar.xz"))
        (sha256
         (base32
          "11k0phq49jvxz9l7l9ca1xwsc5h77dpav7dmasdqv8nrjcjzndf9"))))))
