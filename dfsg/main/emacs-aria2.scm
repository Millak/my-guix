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

(define-module (dfsg main emacs-aria2)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system emacs))

(define-public emacs-aria2
  (package
    (name "emacs-aria2")
    (version "0.20141108")
    (source
      (origin
        (method url-fetch)
        (uri "https://melpa.org/packages/aria2-20141107.1517.el")
        (sha256
         (base32
           "00slyi0aw03cp5s4c1xhavn9i1gz7d6d4cmv0g0dd3nx7m1ba3y0"))))
    (build-system emacs-build-system)
    (home-page "https://gitlab.com/ukaszg/aria2.git")
    (synopsis "Major mode for controlling aria2")
    (description "This is aria2, a major mode for controlling aria2c downloader

Currrently supported download types are: bittorrent, magnet, meta4, ftp, http,
https files (basically what aria2c supports).  There is no support for changing
global or per-download options, but this is planned.

This mode tries to work well with evil-mode, just set aria2-add-evil-quirks to t.")
    (license (license:non-copyleft
               "https://www.gnu.org/licenses/license-list.html#informal"))))
