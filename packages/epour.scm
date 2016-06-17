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

(define-module (packages epour)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system python)
  #:use-module (gnu packages bittorrent)
  #:use-module (gnu packages enlightenment)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib) ; intltool
  #:use-module (gnu packages python))

(define-public epour
  (package
    (name "epour")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://download.enlightenment.org/rel/apps/epour"
                            "/epour-" version ".tar.xz"))
        (sha256
         (base32
          "0fm5gyh6x03ypsqy3gsr36wfr0ys8h7hqwafk49fzjbpv9d3cmcw"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-binary
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Wrap 'epour' so that it finds libtorrent
             (let ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/epour")
                `("PATH" ":" prefix
                  (,(string-append (assoc-ref inputs "libtorrent") "/bin"))))))))
       #:tests? #f)) ; no test target
    (native-inputs `(("intltool" ,intltool)))
    (inputs
     `(("libtorrent" ,libtorrent)
       ("python2-dbus" ,python2-dbus)
       ("python2-distutils-extra" ,python2-distutils-extra)
       ("python2-efl" ,python2-efl)
       ("python2-pyxdg" ,python2-pyxdg)))
    (home-page "https://www.enlightenment.org")
    (synopsis "EFL Bittorrent client")
    (description "EFL Bittorrent client")
    (license license:gpl3+)))
