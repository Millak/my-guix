;;; Copyright © 2022 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main nilfs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages linux))

(define-public nilfs-utils
  (package
    (name "nilfs-utils")
    (version "2.2.9")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://nilfs.sourceforge.io/download"
                            "/nilfs-utils-" version ".tar.bz2"))
        (sha256
         (base32 "15vsayvzr8nc29n939sz9ddq46vpn53rp8h8qv484h88qac3kxjx"))))
    (build-system gnu-build-system)
    (arguments
     (list
       #:configure-flags
       #~(list "--enable-static=no")
       #:phases
       #~(modify-phases %standard-phases
           (add-before 'bootstrap 'force-bootstrap
             (lambda _
               (delete-file "configure")
               (substitute* "configure.ac"
                 (("\\[/etc\\]") "[${prefix}/etc]")
                 (("\\[/sbin\\]") "[${prefix}/sbin]")))))))
    (inputs
     (list (list util-linux "lib")))
    (native-inputs (list autoconf automake libtool))
    (home-page "https://nilfs.sourceforge.io/")
    (synopsis "Continuous Snapshotting Filesystem")
    (description
     "NILFS is a log-structured file system supporting versioning of the entire
file system and continuous snapshotting, which allows users to even restore
files mistakenly overwritten or destroyed just a few seconds ago.")
    (license license:gpl3+)))
