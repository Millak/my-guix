;;; Copyright © 2022, 2023, 2025 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main quassel)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (gnu packages irc)
  #:use-module (gnu packages qt))

(define-public quasselclient
  (package
    (inherit quassel)
    (name "quasselclient")
    (arguments
     (substitute-keyword-arguments (package-arguments quassel)
       ((#:configure-flags _)
        #~`("-DBUILD_TESTING=ON"
            "-DWANT_CORE=OFF"
            "-DWANT_MONO=OFF"))))
    (inputs
     (modify-inputs (package-inputs quassel)
                    (delete "snorenotify")))
    (properties '((upstream-name . "quassel")))))
