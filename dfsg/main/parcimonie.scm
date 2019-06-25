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

(define-module (dfsg main parcimonie)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages tor))

(define-public parcimonie.sh
  (let ((commit "551999ca7480079b912b5a07f631b4fc1323ce49")
        (revision "1"))
    (package
      (name "parcimonie.sh")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/EtiennePerot/parcimonie.sh")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0j3x61al99zfb4p2fbdkdydnrl6z7rjf5bc89bfmy2q4mmshkl4c"))))
      (build-system trivial-build-system)
      (arguments
       `(#:modules ((guix build utils))
         #:builder
         (begin
           (use-modules (guix build utils))
           (let* ((out    (assoc-ref %outputs "out"))
                  (bin    (string-append out "/bin"))
                  (dest   (string-append bin "/parcimonie.sh"))
                  (bash   (assoc-ref %build-inputs "bash"))
                  (source (assoc-ref %build-inputs "source")))
             (setenv "PATH" (string-append bash "/bin/" ":" "$PATH"))
             (install-file (string-append source "/parcimonie.sh") bin)
             (install-file (string-append source "/LICENSE")
                           (string-append out "/share/doc/" ,name "-" ,version))
             (patch-shebang dest
                            (list (string-append bash "/bin")))
             (wrap-program dest
               `("PATH" ":" prefix
                 ,(map (lambda (input)
                         (string-append
                           (assoc-ref %build-inputs input) "/bin"))
                       (list "gnupg" "torsocks"))))
             #t))))
      (native-inputs `(("source" ,source)))
      (inputs
       `(("bash" ,bash-minimal)
         ("gnupg" ,gnupg)
         ("torsocks" ,torsocks)))
      (home-page "https://github.com/EtiennePerot/parcimonie.sh")
      (synopsis "Incrementally refreshes a GnuPG keyring")
      (description "@code{parcimonie.sh} refreshes individual keys in your GnuPG
keyring at randomized intervals.  Each key is refreshed over a unique,
single-use Tor circuit.")
      (license license:wtfpl2))))
