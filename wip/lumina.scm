;;; Copyright © 2016, 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils) ; substitute-keyword-arguments
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages compton)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages wm) ; fluxbox
  #:use-module (gnu packages xdisorg) ; xscreensaver
  #:use-module (gnu packages xorg))

(define-public lumina
  (package
    (name "lumina")
    (version "1.4.0-p1")
    (source
      (origin
        (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/lumina-desktop/lumina.git")
                 (commit (string-append "v" version))))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0jin0a2s6pjbpw7w1bz67dgqp0xlpw1a7nh8zv0qwdf954zczanp"))
          (patches (search-patches "add-LuminaOS-GuixSD.patch"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-install-locations
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((fluxbox (assoc-ref inputs "fluxbox"))
                   (poppler (assoc-ref inputs "poppler"))
                   (out     (assoc-ref outputs "out")))
               (substitute*
                 "src-qt5/core-utils/lumina-config/pages/page_fluxbox_settings.cpp"
                 (("LOS::AppPrefix\\(\\)+\\\"")
                  (string-append "\"" fluxbox "/")))
               (substitute* "src-qt5/core/lumina-desktop/fluxboxconf/fluxbox-init-rc"
                 (("/usr/local") fluxbox))
               (substitute* "src-qt5/OS-detect.pri"
                 (("L_SESSDIR=/usr/share/xsessions")
                  (string-append out "/share/xsessions")))
               ;; This is probably not needed
               (substitute* "src-qt5/core/lumina-desktop/Lumina-DE.desktop"
                 (("start-lumina-desktop")
                  (string-append out "/bin/start-lumina-desktop")))
               (substitute*
                 "src-qt5/desktop-utils/lumina-xdg-entry/lumina-xdg-entry.desktop"
                 (("/usr/local") out))
               (substitute* "src-qt5/desktop-utils/lumina-pdf/lumina-pdf.pro"
                 (("\\$\\$\\{L_INCLUDEDIR\\}") (string-append poppler "/include")))
               ;; This one is probably wrong
               (substitute*
                 '("src-qt5/core/lumina-theme-engine/src/lthemeengine-qtplugin/lthemeengine-qtplugin.pro"
                   "src-qt5/core/lumina-theme-engine/src/lthemeengine-style/lthemeengine-style.pro")
                 (("PLUGINDIR") "L_LIBDIR/qt5/plugins")))
             #t))
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((qttools  (assoc-ref inputs "qttools"))
                    (lrelease (string-append qttools "/bin/lrelease"))
                    (out      (assoc-ref outputs "out")))
               (invoke "qmake" "LINUX_DISTRO=GuixSD"
                       (string-append "LRELEASE=" lrelease)
                       (string-append "PREFIX=" out)
                       "DEFAULT_SETTINGS=GuixSD"
                       "CONFIG+=WITH_I18N")))))))
    (propagated-inputs
     `(("fluxbox" ,fluxbox))) ; Also needed at runtime
    (native-inputs
     `(("qttools" ,qttools)))
    (inputs
     `(("libxcb" ,libxcb)
       ("libxdamage" ,libxdamage)
       ("poppler" ,poppler-qt5)
       ("pulseaudio" ,pulseaudio)
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
    (arguments
     '(#:modules ((guix build union))
       #:builder
       (begin
         (use-modules (ice-9 match)
                      (guix build union))
         (match %build-inputs
           (((names . directories) ...)
            (union-build (assoc-ref %outputs "out")
                         directories))))))
    (inputs
     `(("acpi" ,acpi)
       ("alsa-utils" ,alsa-utils)
       ("compton" ,compton)
       ("dbus" ,dbus)
       ("lumina" ,lumina)
       ("oxygen-icons" ,oxygen-icons)
       ("pavucontrol" ,pavucontrol)
       ("sysstat" ,sysstat)
       ("xbacklight" ,xbacklight)
       ("xinit" ,xinit)
       ("xrandr" ,xrandr)
       ("xscreensaver" ,xscreensaver)
       ("xterm" ,xterm)))
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

;;; Notes:
;;; For the theme engine:
;;; Environment Variable QT_QPA_PLATFORMTHEME=lthemeengine needs to be set
