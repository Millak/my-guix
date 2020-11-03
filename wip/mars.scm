;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (wip mars)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system ant)
  #:use-module (gnu packages java))

;; This package should just be "mars" but there's a name collision with a game.
(define-public mars-mips
  (package
    (name "mars-mips")
    (version "4.5")
    (source
      (origin
        (method url-fetch)
          (uri (string-append "https://courses.missouristate.edu/KenVollmar/mars/MARS_"
                              (string-map (lambda (c) (if (char=? c #\.) #\_ c))
                                          version)
                              "_Aug2014/Mars"
                              (string-map (lambda (c) (if (char=? c #\.) #\_ c))
                                          version)
                              ".jar"))
          (sha256
           (base32
            "15kh1fahkkbbf4wvb6ijzny4fi5dh4pycxyzp5325dm2ddkhnd5c"))
          ;(modules '((guix build utils)))
          ;(snippet
          ; '(begin
          ;    (for-each delete-file (find-files "." "\\.class$"))
          ;    #t))
          ))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f  ; No test target
       #:jdk ,openjdk9  ; As recommended by upstream
       #:jar-name "Mars.jar"
       #:source-dir "."
       #:main-class "Mars"
       #:phases
       (modify-phases %standard-phases
         ;; This should be in a snippet, but I can't figure out how to unpack it.
         (add-after 'unpack 'remove-vendored-class-files
           (lambda _
             (for-each delete-file (find-files "." "\\.class$"))
             #t))
         (add-after 'unpack 'make-code-utf8-safe
           (lambda _
             (with-fluids ((%default-port-encoding "ISO-8859-1"))
               (substitute* "src/mars/tools/DigitalLabSim.java"
                 (("Didier Teifreto LIFC Universit.* de franche-Comt.* www.lifc.univ-fcomte.fr/~teifreto")
                  "Didier Teifreto LIFC Universite de franche-Comte www.lifc.univ-fcomte.fr/~teifreto"))
               (substitute* "src/mars/tools/MipsXray.java"
                 (("M.*rcio Roberto") "Marcio Roberto")
                 (("Fabr.*cio Vivas") "Fabricio Vivas")
                 (("Fl.*vio Cardeal") "Flavio Cardeal")
                 (("F.*bio L.*cio") "Fabio Lucio"))
               (substitute* "src/mars/venus/MessagesPane.java"
                 (("Ricardo Fern.*ndez Pascual") "Ricardo Fernandez Pascual")))
             #t))
         (replace 'install-license-files
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "src/MARSlicense.txt"
                           (string-append (assoc-ref outputs "out")
                                          "/share/doc/mars-" ,version))
             #t)))))
    (home-page "https://courses.missouristate.edu/KenVollmar/MARS/")
    (synopsis "IDE for MIPS Assembly Language Programming")
    (description
     "MARS is a lightweight @acronym{IDE, Interactive Development Environment}
for programming in MIPS assembly language, intended for educational-level use
with Patterson and Hennessy's Computer Organization and Design.  It has:
@enumerate
@item GUI with point-and-click control and integrated editor.
@item Easily editable register and memory values, similar to a spreadsheet.
@item Display values in hexadecimal or decimal.
@item Command line mode for instructors to test and evaluate many programs easily.
@item Floating point registers, coprocessor1 and coprocessor2.  Standard tool:
bit-level view and edit of 32-bit floating point registers.
@item Variable-speed single-step execution.
@item \"Tool\" utility for MIPS control of simulated devices.  Standard tool:
Cache performance analysis tool.
@item Single-step backwards.
@end enumerate")
    (license license:expat)))
