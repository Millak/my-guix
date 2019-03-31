;;; Copyright © 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg non-free ftl)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages bootstrap) ; glibc-dynamic-linker
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages xorg)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match))

(define-public ftl
  (package
    (name "ftl")
    (version "1.6.9")
    (source
      (origin
        (method url-fetch/tarbomb)
        (uri "file:///gnu/store/jzwz3gni1rdfkvkcghp9n7yh8b8yiwqd-FTL.1.6.9.tar.gz")
        (file-name (string-append "FTL-Linux-" version ".tar.gz"))
        (sha256
         (base32
          "0vlgi0kpbybyvrgkdy1mpzhk8h9p2qp5qg1k2irs08izdj66dya4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-26))
       #:strip-binaries? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'chdir
           (lambda _
             (chdir "FTL-linux")
             #t))
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((ftl       ,(match (or (%current-target-system)
                                           (%current-system))
                                       ("x86_64-linux" "data/FTL.amd64")
                                       ("i686-linux" "data/FTL.x86")
                                       (_ "")))
                    (libc      (assoc-ref inputs "libc"))
                    (ld-so     (string-append libc ,(glibc-dynamic-linker)))
                    (libGL     (assoc-ref inputs "mesa"))
                    (libX11    (assoc-ref inputs "libx11"))
                    (libasound (assoc-ref inputs "alsa-lib"))
                    (rpath     (string-append libGL "/lib:"
                                              libX11 "/lib:"
                                              libasound "/lib:"
                                              libc "/lib")))
                 (system* "patchelf" "--set-rpath" rpath ftl)
                 (system* "patchelf" "--set-interpreter" ld-so ftl))
             #t))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out     (assoc-ref outputs "out"))
                    (bin     (string-append out "/bin"))
                    (license (string-append out "/share/doc/ftl"))
                    (desktop (string-append out "/share/applications")))
               (for-each
                 (cut install-file <> bin)
                 (cons*
                   ,(match (or (%current-target-system)
                               (%current-system))
                           ("x86_64-linux" "data/FTL.amd64")
                           ("i686-linux" "data/FTL.x86")
                           (_ ""))
                   (list "data/ftl.dat" "data/exe_icon.bmp" "data/FTL")))
               (for-each
                 (cut install-file <> license)
                 (find-files "data/licenses" ".*"))
               (mkdir-p desktop)
               (call-with-output-file
                 (string-append desktop "/ftl.desktop")
                 (lambda (file)
                   (format file
                           "[Desktop Entry]~@
                           Name=ftl~@
                           Comment=Faster than light~@
                           Exec=~a/bin/FTL~@
                           TryExec=~@*~a/bin/FTL~@
                           Icon=~@*~a/bin/exe_icon.bmp~@
                           Type=Application~%"
                           out))))
             #t)))))
    (native-inputs
     `(("patchelf" ,patchelf)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("libx11" ,libx11)
       ("mesa" ,mesa)))
    (home-page "https://subsetgames.com/ftl.html")
    (synopsis "Spaceship Simulation rogue-like")
    (description "In FTL you experience the atmosphere of running a spaceship
trying to save the galaxy.  It's a dangerous mission, with every encounter
presenting a unique challenge with multiple solutions.")
    (supported-systems '("x86_64-linux" "i686-linux"))
    (license #f)))
