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
  #:use-module (guix build utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
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
  ;; The last release is from 2010, and support for qt5 has been added since
  (let ((commit "02fbc13a42d945b703a28a49d71c02b20c76a0b8")
        (revision "1"))
    (package
      (name "qoauth")
      (version (string-append "1.0.1-" revision "." (string-take commit 7)))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                (url "https://github.com/ayoy/qoauth.git")
                (commit commit)))
          (file-name (string-append name "-" version "-checkout"))
          (sha256
           (base32
            "0lnzgpdfjcx7szw1ibm4sqrsd7ahnnrh7i7bxsj63hyicnaihxh8"))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (replace 'configure
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (zero? (system* "qmake"
                                 (string-append "PREFIX=" out)))))))
         #:tests? #f ; F*** tests
         ))
      (inputs
       `(("qca" ,qca)
         ("qtbase" ,qtbase)))
      (home-page "https://github.com/ayoy/qoauth")
      (synopsis "Qt-based client implementation of the OAuth authorization scheme")
      (description
       "QOAuth is a Qt-based C++ implementation of an interface to services
  using OAuth authorization scheme.")
      (license license:lgpl2.1+))))

(define-public qoauth-qt4
  (package (inherit qoauth)
    (name "qoauth-qt4")
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-qtcrypto
           (lambda _
             ;; Qca puts QtCrypto in include/Qca-qt5/QtCrypto
             (substitute* '("tests/ut_interface/ut_interface.h"
                            "src/interface.h" "src/interface.cpp")
                          (("QtCrypto") "QtCrypto/QtCrypto"))
             #t))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (zero? (system* "qmake"
                               (string-append "PREFIX=" out)))))))
       ))
    (inputs
     `(("qca-qt4" ,qca-qt4)
       ("qt-4" ,qt-4)))))


(define-public qca-qt4
  (package (inherit qca)
    (name "qca-qt4")
    (inputs
     `(("qt-4" ,qt-4)
       ,@(alist-delete "qtbase" (package-inputs qca))))))
