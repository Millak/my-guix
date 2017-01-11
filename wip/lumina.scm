;;; Copyright © 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (wip lumina)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build utils)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils) ; substitute-keyword-arguments
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages wm) ; fluxbox
  #:use-module (gnu packages xdisorg) ; xscreensaver
  #:use-module (gnu packages xorg))

(define-public lumina
  (package
    (name "lumina")
    (version "1.2.0-p1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
              "https://github.com/trueos/lumina/archive/v" version ".tar.gz"))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0jdxcpygivc6q3srfb9y91rfp4ksx56mpvzbbyf06a9zzq964dpq"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            ;; This creates the GuixSD specific Lumina config.
            (copy-file "src-qt5/core/libLumina/LuminaOS-Gentoo.cpp"
                       "src-qt5/core/libLumina/LuminaOS-GuixSD.cpp")
            (substitute* "src-qt5/core/libLumina/LuminaOS-GuixSD.cpp"
              (("Gentoo Linux") "GuixSD")
              (("AppStoreShortcut\\(\\)")
               (string-append "AppStoreShortcut(){ return \"\"; }//"))
              (("\"/usr/") "PREFIX+\"usr")
              (("\"/\"") "PREFIX")
              )
            ))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-install-locations
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((fluxbox (assoc-ref inputs "fluxbox"))
                   (out     (assoc-ref outputs "out")))
               (substitute* "src-qt5/core-utils/lumina-config/pages/page_fluxbox_settings.cpp"
                 (("LOS::AppPrefix\\(\\)+\\\"") (string-append "\"" fluxbox "/"))
                 )
               ;(substitute* "src-qt5/core/libLumina/LuminaOS-GuixSD.cpp"
               ;  (("/usr/") (string-append out "/usr"))
               ;  (("\"/\"") (string-append "\"" out "\""))
               ;  )
               (substitute* "src-qt5/OS-detect.pri"
                 (("L_SESSDIR=/usr/share/xsessions") "")
                 )
               )
               #t))
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((qttools (assoc-ref inputs "qttools"))
                    (lrelease (string-append qttools "/bin/lrelease"))
                    (out (assoc-ref outputs "out")))
               (zero? (system* "qmake" "LINUX_DISTRO=GuixSD"
                               (string-append "LRELEASE=" lrelease)
                               (string-append "PREFIX=" out)))))))))
    (propagated-inputs
     `(("fluxbox" ,fluxbox))) ; Also needed at runtime
    (native-inputs
     `(("qttools" ,qttools)))
    (inputs
     `(("libxcb" ,(package
                    (inherit libxcb)
                    (arguments
                     (substitute-keyword-arguments (package-arguments libxcb)
                       ((#:configure-flags flags)
                        `(cons* "--enable-xinput" ,flags))))))
       ("qtbase" ,qtbase)
       ("qtmultimedia" ,qtmultimedia)
       ("qtsvg" ,qtsvg)
       ("qtx11extras" ,qtx11extras)
       ("xcb-util" ,xcb-util)
       ("xcb-util-image" ,xcb-util-image)
       ("xcb-util-wm" ,xcb-util-wm)))
    (home-page "https://lumina-desktop.org/")
    (synopsis "Lumina Desktop Environment")
    (description
     "The Lumina Desktop Environment is a lightweight system interface that is
designed for use on any Unix-like operating system.  It takes a plugin-based
approach, allowing the entire interface to be assembled/arranged by each
individual user as desired, with a system-wide default layout which can be
setup by the system administrator.  This allows every system (or user session)
to be designed to maximize the individual user's productivity.")
    (license (list license:bsd-3
                   license:cc-by-sa4.0)))) ; icon-theme

(define-public luminaOS
  (package
    (name "luminaOS")
    (version (package-version lumina))
    (source #f)
    (build-system trivial-build-system)
    (arguments '(#:builder (mkdir %output)))
    (propagated-inputs
     `(
       ("acpi" ,acpi) ; linux
       ("dbus" ,dbus) ; glib
       ("lumina" ,lumina)
       ;("oxygen-icons" ,oxygen-icons) ; kde-frameworks
       ;("pavucontrol" ,pavucontrol) ; pulseaudio
       ("sysstat" ,sysstat) ; linux?
       ("xscreensaver" ,xscreensaver) ; xdisorg
       ))
    (home-page "https://lumina-desktop.org/")
    (synopsis "The Lumina Desktop Environment")
    (description
     "The Lumina Desktop Environment is a lightweight system interface that is
designed for use on any Unix-like operating system.  It takes a plugin-based
approach, allowing the entire interface to be assembled/arranged by each
individual user as desired, with a system-wide default layout which can be
setup by the system administrator.  This allows every system (or user session)
to be designed to maximize the individual user's productivity.")
    (license (list license:bsd-3
                   license:cc-by-sa4.0)))) ; icon-theme
