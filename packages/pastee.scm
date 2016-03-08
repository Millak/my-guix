;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (packages pastee)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages python))

(define-public pastee
  (package
    (name "pastee")
    (version "0.1.0")
    (source
      (origin
        (method url-fetch)
        (uri "https://pastee.org/pastee.py")
        (sha256
         (base32
          "173m8j46ikmb530ryx84cy2dapxsvkvixal6zakz3vjmq8vz46h6"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out  (assoc-ref %outputs "out"))
                (dest (string-append out "/bin"))
                (source (assoc-ref %build-inputs "source")))
           (mkdir-p dest)
           (copy-file source (string-append dest "/" "pastee"))
           (patch-shebang (string-append dest "/" "pastee") (list (string-append (assoc-ref %build-inputs "python") "/bin")))
           (chmod (string-append dest "/" "pastee") #o755)))))
    (native-inputs `(("source" ,source)))
    (inputs `(("python" ,python-2)))
    (home-page "https://pastee.org/")
    (synopsis "Pastee python uploader")
    (description "Pastee python uploader")
    (license (license:non-copyleft "no license listed"))))
