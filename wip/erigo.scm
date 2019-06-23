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

(define-module (wip erigo)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages enlightenment)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  )

(define-public erigo
  (package
    (name "erigo")
    (version "20160428")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://git.enlightenment.org/tools/erigo.git/")
               (commit "d2cc463ccc31f0061d4d144279d9968de6dfb030")))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "09s9s2ymbba4sx2i6v1divkvap9zlrdiaw4z6qy26rpb04llhg2h"))))
    (build-system cmake-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-home-directory
           ;; FATAL: Cannot create run dir '/homeless-shelter/.run' - errno=2
           (lambda _ (setenv "HOME" "/tmp") #t)))))
    (native-inputs
     `(("libffi" ,libffi)
       ("pkg-config" ,pkg-config)
       ("python-2" ,python-2)))
    (inputs
     `(("efl" ,efl)
       ("graphviz" ,graphviz)))
    (home-page "https://www.enlightenment.org")
    (synopsis "GUI Builder for the EFL")
    (description "Erigo, the EFL gui builder, enables you easily create user
interface for your application.  You can easily build multi-windowed
application, simulate its behavior without compiling, generate sources in
different languages (currently C).")
    (license license:bsd-2)))
