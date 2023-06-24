;;; Copyright © 2023 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main syncthing)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages syncthing))

(define-public syncthing-goamdv3
  (package
    (inherit syncthing)
    (name "syncthing-goamdv3")  ; To make it easy to search for.
    (arguments
     (substitute-keyword-arguments (package-arguments syncthing)
       ((#:tests? _ #t) #f)
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (add-after 'setup-go-environment 'set-microarchitecture
             (lambda _
               (setenv "GOAMD" "v3")))))))
    ;; This is the only architecture which can build this package.
    ;; go: cannot install cross-compiled binaries when GOBIN is set
    (supported-systems '("x86_64-linux"))))

(define-public syncthing-goarm5
  (package
    (inherit syncthing)
    (name "syncthing-goarm5")   ; To make it easy to search for.
    (arguments
     (substitute-keyword-arguments (package-arguments syncthing)
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (add-after 'setup-go-environment 'set-microarchitecture
             (lambda _
               (setenv "GOARM" "5")))))))
    ;; This is the only architecture which can build this package.
    ;; go: cannot install cross-compiled binaries when GOBIN is set
    (supported-systems '("armhf-linux"))))
