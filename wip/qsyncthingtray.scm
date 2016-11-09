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

(define-module (wip qsyncthingtray)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages qt))

(define-public qsyncthingtray
  (package
    (name "qsyncthingtray")
    (version "0.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://github.com/sieren/QSyncthingTray/archive/"
               version ".tar.gz"))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1q1drrarv249yzcn40095iqik179094z4w48bzl10sx76ms71ms2"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags '("-DQST_BUILD_WEBKIT=1")
       #:tests? #f)) ; no test target
    (inputs
     `(("qtbase" ,qtbase)
       ("qtwebkit" ,qtwebkit)
       ))
    (home-page "https://github.com/sieren/QSyncthingTray")
    (synopsis "Traybar Application for Syncthing")
    (description
     "A traybar application for syncthing.
@enumerate
@item Shows number of connections at a glance.
@item Traffic statistics about incoming, outgoing and total throughput.
@item Launches Syncthing and Syncthing-iNotifier if specified.
@item Quickly pause Syncthing with one click.
@item Last Synced Files - Quickly see the recently synchronised files and open
their folder.
@item Quick Access to all shared folders.
@item Presents Syncthing UI in a separate view instead of using the browser.
@item Supports authenticated HTTPS connections.
@item Uses System Notifications about current connection status.
@item Toggle for monochrome icon.
@end enumerate\n")
    (license license:lgpl3+)))
