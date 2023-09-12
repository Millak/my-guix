;;; vim: set ft=scheme :
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

(define-module (dfsg main guix)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system copy)
  #:use-module (gnu artwork))

(define-public guix-backgrounds
  (package
    (name "guix-backgrounds")
    (version "0")
    (source %artwork-repository)
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       '(("backgrounds" "share/backgrounds/guix" #:exclude ("README")))))
    (home-page "https://www.gnu.org/software/guix/")
    (synopsis "Background images for GNU Guix")
    (description "The SVG files in this directory are intended to be used as
backgrounds for different components of the GNU system like login managers and
desktop environments. The backgrounds are available in different aspect ratios
which are indicated in the file name.")
    (license (list license:public-domain license:cc-by-sa4.0))))
