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

(define-module (wip dianara)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages file)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages qt))

(define-public dianara
  (package
    (name "dianara")
    (version "1.3.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://savannah/dianara/dianara-v" version ".tar.gz"))
        (sha256
         (base32
          "083bzbrrq2n3jk48643bcbh8dh21fgx62dzqj6ffsw13gygxjn62"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (zero? (system* "qmake"
                               (string-append "PREFIX=" out)))))))))
    (inputs
     `(("file" ,file) ; libmagic
       ("qca" ,qca)
       ("qoauth" ,qoauth)
       ("qtbase" ,qtbase)))
    (home-page "https://jancoding.wordpress.com/dianara/")
    (synopsis "Client for the pump.io federated social network")
    (description
     "Dianara is a pump.io client, a desktop application for GNU/linux that
allows users to manage their Pump.io social networking accounts without the
need to use a web browser.")
    (license license:gpl2+)))

(define-public qoauth
  (package
    (name "qoauth")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://github.com/ayoy/qoauth/archive/v" version ".tar.gz"))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "130y1gnc9q10k5j7r612j9p7w8dqjbp8wjv4xzx9hnvkbc6jw88h"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-qtcrypto
           (lambda _
             ;; Qca puts QtCrypto in include/Qca-qt5/QtCrypto
             (substitute* '("tests/ut_interface/ut_interface.h"
                            "src/interface.h" "src/interface.cpp")
                          (("QtCrypto") "Qca-qt5/QtCrypto"))))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (zero? (system* "qmake"
                               (string-append "PREFIX=" out)))))))
       ))
    (inputs
     `(("qca" ,qca)
       ("qtbase" ,qtbase)))
    (home-page "https://github.com/ayoy/qoauth")
    (synopsis "Qt-based client implementation of the OAuth authorization scheme")
    (description
     "QOAuth is a Qt-based C++ implementation of an interface to services
using OAuth authorization scheme.")
    (license license:lgpl2.1+)))
