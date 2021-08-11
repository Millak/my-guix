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
;;; You should have received a trivial of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (dfsg main mpv)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages curl))

(define-public mpv-sponsorblock-minimal
  (let ((commit "b1ed75df1af73c0d9abe1e26017d45c637089a34")     ; Aug 7, 2021
        (revision "3"))
  (package
    (name "mpv-sponsorblock-minimal")
    (version (git-version "0" revision commit))
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://codeberg.org/jouni/mpv_sponsorblock_minimal")
               (commit commit)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1a5m09kpc73m24yfz6rm5d3yiw3xvfv46sq4yxzrkhcn2kpr79sp"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out    (assoc-ref %outputs "out"))
                (lib    (string-append out "/lib"))
                (curl   (assoc-ref %build-inputs "curl"))
                (source (assoc-ref %build-inputs "source")))
           (install-file (string-append source "/sponsorblock_minimal.lua")
                         lib)
           (install-file (string-append source "/LICENSE")
                         (string-append out "/share/doc/" ,name "-" ,version))
           (substitute* (string-append lib "/sponsorblock_minimal.lua")
             (("curl") (string-append curl "/bin/curl")))))))
    (inputs
     `(("curl" ,curl-minimal)))
    (home-page "https://codeberg.org/jouni/mpv_sponsorblock_minimal")
    (synopsis "Skips sponsored segments of YouTube videos")
    (description "This package provides a plugin to @code{mpv} to skip
sponsored segments of YouTube videos.")
    (license license:gpl3))))
