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

(define-module (dfsg main whatsappqt)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system qt)
  #:use-module (gnu packages qt))

(define-public whatsappqt
  (package
    (name "whatsappqt")
    (version "1.0.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://gitlab.com/bit3/whatsappqt")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1ijr7snmpfmg708pnisnmnz4z4ss3hr1b4l1ryw58h7valcng4gz"))))
    (build-system qt-build-system)
    (arguments
     `(#:tests? #f          ; No tests.
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "WhatsAppQT.pro"
               (("/usr/local") (assoc-ref outputs "out")))
             (invoke "qmake"))))))
    (inputs
     `(("qtbase" ,qtbase-5)
       ("qtdeclarative" ,qtdeclarative)
       ("qtwebchannel" ,qtwebchannel)
       ("qtwebengine" ,qtwebengine)))
    (home-page "https://gitlab.com/bit3/whatsappqt")
    (synopsis "Unofficial WhatsApp Web Desktop Client")
    (description
     "This package provides an unofficial WhatsApp web desktop client.")
    (license license:expat)))
