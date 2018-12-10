;;; Copyright © 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main brendan_gregg)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages perl))

(define-public baud
  (package
    (name "baud")
    (version "1.20")
    (source
      (origin
        (method url-fetch)
        (uri "http://www.brendangregg.com/Specials/baud")
        (sha256
         (base32
          "04zah2yyby7g0jwqhrvxvjhfxhkq2r8k4ixxx79d059dkzdvyhvr"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out    (assoc-ref %outputs "out"))
                (dest   (string-append out "/bin/baud"))
                (perl   (assoc-ref %build-inputs "perl"))
                (source (assoc-ref %build-inputs "source")))
           (mkdir-p (string-append out "/bin"))
           (copy-file source dest) ; not install-file
           (patch-shebang dest
             (list (string-append perl "/bin")))
           (chmod dest #o555)))))
    (native-inputs `(("source" ,source)))
    (inputs `(("perl" ,perl)))
    (home-page "http://www.brendangregg.com/specials.html")
    (synopsis "Run commands safley at their native baud")
    (description "This allows older commands to be executed safely at their
original native baud.  Commands are now often run over high speed telnet or ssh
sessions, at speeds they were not designed for - sometimes called
\"overbauding\".  Overbauding can cause command overheating, resulting in
@dfn{Command Fault Heat State Exception} (CFHSE) errors.

Many command line programs, especially those for Unix, were orginially written
to run at bauds such as 300, 1200, 2400 or even 9600. This was the days of
serial connections to teletypes or dumb terminals (aka glass teletypes, green
screens, etc).  While links to servers have increased in speed, the code for
most commands has remained the same. Some operating systems have a man page,
fastcommands(5), that lists commands that are high speed link safe.")
    (license license:gpl2+)))

(define-public maybe
  (package
    (name "maybe")
    (version "1.00")
    (source
      (origin
        (method url-fetch)
        (uri "http://www.brendangregg.com/Specials/maybe")
        (sha256
         (base32
          "195hylz46lh8z0skl0hwsrsk64qn0bx0fwb9z3dcnrfjymi266ss"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out    (assoc-ref %outputs "out"))
                (dest   (string-append out "/bin/maybe"))
                (perl   (assoc-ref %build-inputs "perl"))
                (source (assoc-ref %build-inputs "source")))
           (mkdir-p (string-append out "/bin"))
           (copy-file source dest) ; not install-file
           (patch-shebang dest
             (list (string-append perl "/bin")))
           (chmod dest #o555)))))
    (native-inputs `(("source" ,source)))
    (inputs `(("perl" ,perl)))
    (home-page "http://www.brendangregg.com/specials.html")
    (synopsis "Sometimes true, sometimes false")
    (description "Maybe is a companion to @code{/usr/bin/true} and
@code{/usr/bin/false}.")
    (license license:gpl2+)))

(define-public onstat
  (package
    (name "onstat")
    (version "1.00")
    (source
      (origin
        (method url-fetch)
        (uri "http://www.brendangregg.com/Specials/onstat")
        (sha256
         (base32
          "1gd5issqwacl4ypgibj6b1hmclslv54mz0xc361f7zmsss5ywrdr"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out    (assoc-ref %outputs "out"))
                (dest   (string-append out "/bin/onstat"))
                (perl   (assoc-ref %build-inputs "perl"))
                (source (assoc-ref %build-inputs "source")))
           (mkdir-p (string-append out "/bin"))
           (copy-file source dest) ; not install-file
           (patch-shebang dest
             (list (string-append perl "/bin")))
           (chmod dest #o555)))))
    (native-inputs `(("source" ,source)))
    (inputs `(("perl" ,perl)))
    (home-page "http://www.brendangregg.com/specials.html")
    (synopsis "Server on status")
    (description "Onstat tells you if your server is switched on.  A common
problem is when staff attempt to use a server or desktop when the power is not
switched on.  This may help diagnose such a situation.")
    (license license:gpl2+)))

(define-public cdrewind
  (package
    (name "cdrewind")
    (version "1.00")
    (source
      (origin
        (method url-fetch)
        (uri "http://www.brendangregg.com/Specials/cdrewind")
        (sha256
         (base32
          "18sai89dv46v746s8xpjis77lk885ri2y8h9xmrsrwd36ggbn6v7"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out    (assoc-ref %outputs "out"))
                (dest   (string-append out "/bin/cdrewind"))
                (perl   (assoc-ref %build-inputs "perl"))
                (source (assoc-ref %build-inputs "source")))
           (mkdir-p (string-append out "/bin"))
           (copy-file source dest) ; not install-file
           (patch-shebang dest
             (list (string-append perl "/bin")))
           (chmod dest #o555)))))
    (native-inputs `(("source" ,source)))
    (inputs `(("perl" ,perl)))
    (home-page "http://www.brendangregg.com/specials.html")
    (synopsis "Rewind CDROMs before ejection")
    (description "Many Unix based operating systems neglect to rewind CDROMs
fully before ejection.  This may leave some CDROMs positioned incorrectly when
they are next used.  @code{cdrewind} should be used to ensure that the CDROM
drive has completed the rewind cycle before the disk is removed.

Some cheaper CDROM drives can eject the disk while there are still tracks in
the drive.  Placing these tracks back on the CDROM is a tedious process, only
slightly improved when using a quality pair of CDROM tweezers.  Some admins and
found that spraying their CDROMs with shaving cream can help keep the tracks on
the CDROM.  @code{cdrewind} is far more reliable and should be used for desktops
through to servers.")
    (license license:gpl2+)))

(define-public lsss
  (package
    (name "lsss")
    (version "0.90")
    (source
      (origin
        (method url-fetch)
        (uri "http://www.brendangregg.com/Specials/lsss")
        (sha256
         (base32
          "00czx5wih03s8djpsj021aabin04m6b9gwpwxazzmkyvrxhv19i6"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out    (assoc-ref %outputs "out"))
                (dest   (string-append out "/bin/lsss"))
                (perl   (assoc-ref %build-inputs "perl"))
                (source (assoc-ref %build-inputs "source")))
           (mkdir-p (string-append out "/bin"))
           (copy-file source dest) ; not install-file
           (patch-shebang dest
             (list (string-append perl "/bin")))
           (chmod dest #o555)))))
    (native-inputs `(("source" ,source)))
    (inputs `(("perl" ,perl)))
    (home-page "http://www.brendangregg.com/specials.html")
    (synopsis "ls with StarSign")
    (description "@code{lsss} is ls with StarSign, an improved @code{ls}
command.  This runs the @code{ls} command and inserts a field to display the
starsign of each file.")
    (license license:gpl2+)))

(define-public gwhiz
  (package
    (name "gwhiz")
    (version "1.12")
    (source
      (origin
        (method url-fetch)
        (uri "http://www.brendangregg.com/Specials/gwhiz")
        (sha256
         (base32
          "1rkggpn6ydxil1r9d9li8kdh8g54l7rzc3smb3wk6rk473flmd11"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out    (assoc-ref %outputs "out"))
                (dest   (string-append out "/bin/gwhiz"))
                (perl   (assoc-ref %build-inputs "perl"))
                (source (assoc-ref %build-inputs "source")))
           (mkdir-p (string-append out "/bin"))
           (copy-file source dest) ; not install-file
           (patch-shebang dest
             (list (string-append perl "/bin")))
           (chmod dest #o555)))))
    (native-inputs `(("source" ,source)))
    (inputs `(("perl" ,perl)))
    (home-page "http://www.brendangregg.com/specials.html")
    (synopsis "whiz files or output streams")
    (description "@code{gwhiz} will whiz files or commands, making them easier
to read so that you do not miss important details.  If your terminal supports
color, @code{gwhiz} can highlight in colour using the \"-c\" option.")
    (license license:gpl2+)))

(define-public l33t
  (package
    (name "l33t")
    (version "0.80")
    (source
      (origin
        (method url-fetch)
        (uri "http://www.brendangregg.com/Specials/l33t")
        (sha256
         (base32
          "08rl0qk4vfijncaykfjsc6g94ih7c0qv642h3vpwfyvanqdydg0j"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out    (assoc-ref %outputs "out"))
                (dest   (string-append out "/bin/l33t"))
                (perl   (assoc-ref %build-inputs "perl"))
                (source (assoc-ref %build-inputs "source")))
           (mkdir-p (string-append out "/bin"))
           (copy-file source dest) ; not install-file
           (patch-shebang dest
             (list (string-append perl "/bin")))
           (chmod dest #o555)))))
    (native-inputs `(("source" ,source)))
    (inputs `(("perl" ,perl)))
    (home-page "http://www.brendangregg.com/specials.html")
    (synopsis "auto l33t converter")
    (description "This program converts text to l33t-speak.  This is helpful to
convert text that has been written in a formal and confusing manner, into text
that is easy to follow.")
    (license license:gpl2+)))

(define-public 3rot13
  (package
    (name "3rot13")
    (version "1.00")
    (source
      (origin
        (method url-fetch)
        (uri "http://www.brendangregg.com/Specials/3rot13")
        (sha256
         (base32
          "1znn983c05ma4ck31b0irwdpf3m6dy83nlgmc2l8x35k47q2rm35"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out    (assoc-ref %outputs "out"))
                (dest   (string-append out "/bin/3rot13"))
                (perl   (assoc-ref %build-inputs "perl"))
                (source (assoc-ref %build-inputs "source")))
           (mkdir-p (string-append out "/bin"))
           (copy-file source dest) ; not install-file
           (patch-shebang dest
             (list (string-append perl "/bin")))
           (chmod dest #o555)))))
    (native-inputs `(("source" ,source)))
    (inputs `(("perl" ,perl)))
    (home-page "http://www.brendangregg.com/specials.html")
    (synopsis "heavyweight encryption algorithm: Triple ROT13")
    (description "This implements a new encryption algorithm, 3ROT13.  This is a
symmetric cypher that has a number of desirable features over existing
algorithms; firstly it is stateless - this allows recovery of any random
substring from the cyphertext, which is especially useful if portions of the
cyphertext are lost or damaged.  @code{3rot13} is also keyless, removing the
need to worry about key storage or key lengths.  Finally ROT13 is fast.  Should
a higher performance be desired this code can be reworked in C.")
    (license license:gpl2+)))

(define-public nrot13
  (package
    (name "nrot13")
    (version "1.00")
    (source
      (origin
        (method url-fetch)
        (uri "http://www.brendangregg.com/Specials/nrot13")
        (sha256
         (base32
          "0xq76d39hglf53mp24g0sdl2688by0yyh9v69rc4xi5sbzmdxaya"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out    (assoc-ref %outputs "out"))
                (dest   (string-append out "/bin/nrot13"))
                (perl   (assoc-ref %build-inputs "perl"))
                (source (assoc-ref %build-inputs "source")))
           (mkdir-p (string-append out "/bin"))
           (copy-file source dest) ; not install-file
           (patch-shebang dest
             (list (string-append perl "/bin")))
           (chmod dest #o555)))))
    (native-inputs `(("source" ,source)))
    (inputs `(("perl" ,perl)))
    (home-page "http://www.brendangregg.com/specials.html")
    (synopsis "configurable encryption algorithm: NROT13")
    (description "This implements a new configurable encryption algorithm,
NROT13.  This is similar to the 3ROT13 algorithm, a symmetric stateless
keyless cypher, however this allows the user to customise the number of cycles.
For example, 3ROT13 can be used, as well as 9ROT13 and even the
supersymmetric 31ROT13.")
    (license license:gpl2+)))

(define-public mkzombie
  (package
    (name "mkzombie")
    (version "0.80")
    (source
      (origin
        (method url-fetch)
        (uri "http://www.brendangregg.com/Specials/mkzombie.c")
        (sha256
         (base32
          "04fciciykphswj12vwlbvii3sjcqqf2j6kiqynsg5n14qyghr6cl"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ; no test suite
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'unpack)
         (replace 'build
           (lambda* (#:key source #:allow-other-keys)
             (invoke "gcc" "-o" "mkzombie" source)))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (dest (string-append out "/bin/mkzombie")))
               (mkdir-p (string-append out "/bin"))
               (copy-file "mkzombie" dest))
             #t)))))
    (home-page "http://www.brendangregg.com/specials.html")
    (synopsis "make zombie processes")
    (description "This program creates one or more zombies and a daemon their
leader.  It can be used to replenish system zombies, or to feed the init
monster.")
    (license license:gpl2+)))
