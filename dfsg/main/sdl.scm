;;; Copyright © 2021 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main sdl)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages sdl))

(define-public sdl2-2.0.14
  (package/inherit sdl2
    (name "sdl2")
    (version "2.0.14")
    (source (origin
             (method url-fetch)
             (uri
              (string-append "https://libsdl.org/release/SDL2-"
                             version ".tar.gz"))
             (sha256
              (base32
               "1g1jahknv5r4yhh1xq5sf0md20ybdw1zh1i15lry26sq39bmn8fq"))))))
