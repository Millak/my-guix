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

(define-module (packages iceweasel)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages ccache)
  #:use-module (gnu packages gnuzilla))

(define-public iceweasel
  (package (inherit icecat)
    (name "iceweasel")
    (version "38.7.1esr")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://ftp.mozilla.org/pub/firefox/releases/"
                            version "/source/firefox-" version ".source.tar.bz2"))
        (sha256
         (base32
          "0rcgi1gra01ggzbl28zzcgrpmm9bajjmvbga3hiwh446jimf9z6h"))))
    (arguments
      (substitute-keyword-arguments (package-arguments icecat)
        ((#:configure-flags cf)
         `(append ,cf
                  (list
                    "--disable-eme"
                    (string-append "--with-branding="
                                   (assoc-ref %build-inputs "branding") "/branding")
                    (string-append "--with-ccache="
                                   (assoc-ref %build-inputs "ccache")
                                   "/bin/ccache"))))))
    (inputs
     `(("ccache" ,ccache)
       ("branding" ,(origin
                      (method git-fetch)
                      (uri (git-reference
                           (url "https://projects.parabola.nu/packages/iceweasel.git")
                           (commit "45.0")))
                      (sha256
                       (base32
                        "0r1nq4s4mwl79p28jd7jmjaxqfh0ls8vm92xksm1cm2m40bi7q9b"))
                      (file-name "iceweasel-branding")))
       ,@(package-inputs icecat)))
    (home-page "https://www.mozilla.org/en-US/")))
