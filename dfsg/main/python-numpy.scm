;;; Copyright © 2023 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main python-numpy)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (dfsg main openblas)
  #:use-module (gnu packages python-xyz))

(define-public python-numpy-ilp64
  (package
    (inherit python-numpy)
    (name "python-numpy-ilp64")
    (arguments
     (substitute-keyword-arguments (package-arguments python-numpy)
       ((#:phases phases)
        #~(modify-phases #$phases
            (replace 'configure-blas
              (lambda* (#:key inputs #:allow-other-keys)
                (setenv "NPY_USE_BLAS_ILP64" "1")
                (setenv "NPY_BLAS_ILP64_ORDER" "openblas64_")
                (setenv "NPY_LAPACK_ILP64_ORDER" "openblas64_")
                (call-with-output-file "site.cfg"
                  (lambda (port)
                    (format port
                            "\
[openblas64_]
libraries = openblas64_
library_dirs = ~a/lib
include_dirs = ~:*~a/include~%"
                            (dirname (dirname
                                       (search-input-file
                                         inputs "include/openblas_config.h")))))))))
          )))
    (inputs
     (modify-inputs (package-inputs python-numpy)
                    (replace "openblas" openblas-ilp64-custom)))
    (supported-systems '("x86_64-linux" "aarch64-linux"))))
