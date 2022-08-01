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

(define-module (dfsg main minitube)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system qt)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages video))

(define-public minitube
  (package
    (name "minitube")
    (version "3.9.3")
    (source
      (origin
        (method url-fetch)
          (uri (string-append "https://github.com/flaviotordini/minitube"
                              "/releases/download/" version
                              "/minitube-" version ".tar.bz2"))
          (sha256
           (base32 "13349a8ap3cgj7f8a9088w559vsxqqfgnj2s2hzka6326vzp0bhf"))))
    (build-system qt-build-system)
    (arguments
     `(#:tests? #f      ; No tests?
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "qmake"
                     (string-append "PREFIX=" (assoc-ref outputs "out"))
                     "QMAKE_LRELEASE=lrelease"
                     "QMAKE_LUPDATE=lupdate"))))))
    (native-inputs
     (list qttools-5))
    (inputs
     (list mpv
           qtbase-5
           qtdeclarative-5
           qtx11extras))
    (home-page "https://flavio.tordini.org/minitube")
    (synopsis "Native YouTube client")
    (description
     "Minitube is a native YouTube client.  With it you can watch YouTube videos
in a new way: you type a keyword, Minitube gives you an endless video stream.")
    (license license:gpl3+)))
