;;; Copyright © 2017, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
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
  #:use-module (gnu packages linux)
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
to run at bauds such as 300, 1200, 2400 or even 9600.  This was the days of
serial connections to teletypes or dumb terminals (aka glass teletypes, green
screens, etc).  While links to servers have increased in speed, the code for
most commands has remained the same.  Some operating systems have a man page,
fastcommands(5), that lists commands that are high speed link safe.")
    (license license:gpl2+)))

(define-public ishadm
  (package
    (name "ishadm")
    (version "1.00")
    (source
      (origin
        (method url-fetch)
        (uri "http://www.brendangregg.com/Specials/ishadm")
        (sha256
         (base32
          "1mr3a46qxzbn00gaqgfbwcjd3lxgbgnirvv8xj60vdja63s10g8z"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out     (assoc-ref %outputs "out"))
                (dest    (string-append out "/bin/ishadm"))
                (perl    (assoc-ref %build-inputs "perl"))
                (netstat (string-append (assoc-ref %build-inputs "net-tools")
                                        "/bin/netstat"))
                (source  (assoc-ref %build-inputs "source")))
           (mkdir-p (string-append out "/bin"))
           (copy-file source dest) ; not install-file
           (patch-shebang dest
             (list (string-append perl "/bin")))
           (substitute* dest
             (("netstat") netstat))
           (chmod dest #o555)))))
    (native-inputs `(("source" ,source)))
    (inputs
     `(("net-tools" ,net-tools)
       ("perl" ,perl)))
    (home-page "http://www.brendangregg.com/specials.html")
    (synopsis "Information Super Highway Administration")
    (description "This checks and enables network routes to the Information
Super Highway to ensure maximum Internet performance.  Must be run as root to
update the routes.")
    (license license:gpl2+)))

;;;TODO: Write a service.
(define-public turbo
  (package
    (name "turbo")
    (version "1.00")
    (source
      (origin
        (method url-fetch)
        (uri "http://www.brendangregg.com/Specials/turbo")
        (sha256
         (base32
          "0liq55l3nnd77pd2zs0rcgk2afszgzkxjcambl7v405ylgxl04b6"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out    (assoc-ref %outputs "out"))
                (dest   (string-append out "/bin/turbo"))
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
    (synopsis "Toggle the turbo button")
    (description "Once upon a time computers were made with a physical turbo
button that doubled the CPU speed.  These days we need to make do with a
script.")
    (license license:gpl2+)))

;;;TODO: Write a service.
(define-public icmpcharger
  (package
    (name "icmpcharger")
    (version "1.00")
    (source
      (origin
        (method url-fetch)
        (uri "http://www.brendangregg.com/Specials/icmpcharger")
        (sha256
         (base32
          "1i9jlly3758mz4pagxcnrjs50a6anr9npkjraixanhd1ga76wl0b"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out    (assoc-ref %outputs "out"))
                (dest   (string-append out "/bin/icmpcharger"))
                (perl   (assoc-ref %build-inputs "perl"))
                (icmp   (string-append (assoc-ref %build-inputs "headers")
                                       "/include/linux"))
                (source (assoc-ref %build-inputs "source")))
           (mkdir-p (string-append out "/bin"))
           (copy-file source dest) ; not install-file
           (substitute* dest
             (("/usr/include") icmp))
           (patch-shebang dest
             (list (string-append perl "/bin")))
           (chmod dest #o555)))))
    (native-inputs `(("source" ,source)))
    (inputs
     `(("headers" ,linux-libre-headers)
       ("perl" ,perl)))
    (home-page "http://www.brendangregg.com/specials.html")
    (synopsis "ICMP driver trickle charger")
    (description "This trickle charges the ICMP driver by sending a series of
ICMP packets to localhost at regular intervals.  This program autodetects the
hardware type and uses appropriate values for the interval based on the wattage
of each packet.  This script can be incorporated into the system startup
process as a daemon.

The batteries in the kernel's ICMP driver can become depleted, a common problem
that causes high latency or dropped packets for any ICMP type.  RFC792 fails to
specify an ICMP message for battery status, leaving such problems difficult to
identify or diagnose.  This script serves as a precaution against ICMP driver
failure.

This program also improves the performance of the ICMP driver by \"priming\" the
L1, L2 and TLB caches with ICMP driver entries.")
    (license license:gpl2+)))

(define-public bottom
  (package
    (name "bottom")
    (version "0.91")
    (source
      (origin
        (method url-fetch)
        (uri "http://www.brendangregg.com/Specials/bottom")
        (sha256
         (base32
          "1ziarv1whhqv43w0k6zbvqs7w25py5izb16w1ipm278raxqqwgq7"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out    (assoc-ref %outputs "out"))
                (dest   (string-append out "/bin/bottom"))
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
    (synopsis "Display bottom processes")
    (description "This is the opposite of @code{top}, it displays processes
that are using the least CPU.  It is the companion to the \"prstat\" command.")
    (license license:gpl2+)))

(define-public ltzip
  (package
    (name "ltzip")
    (version "0.70")
    (source
      (origin
        (method url-fetch)
        (uri "http://www.brendangregg.com/Specials/ltzip")
        (sha256
         (base32
          "0gr25mh0daklc590lk324mn3i5mdj1zaipjnr8jxlvv92nk9l8n6"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out    (assoc-ref %outputs "out"))
                (dest   (string-append out "/bin/ltzip"))
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
    (synopsis "Lossy Text Compression")
    (description "This program compresses text files using a unique lossy text
compression algorithm.  Decompression is not possible.  The result file has a
\".ltz\" extension, and the original file remains.

Not only is the byte count reduced, but the bytes themselves are smaller
bytes - and weigh less when stored on disk.")
    (license license:gpl2+)))

(define-public ltunzip
  (package
    (name "ltunzip")
    (version "0.70")
    (source
      (origin
        (method url-fetch)
        (uri "http://www.brendangregg.com/Specials/ltunzip")
        (sha256
         (base32
          "154fk9lw2450ij3b78rbqy3wl88wjbg0afmnvlamk1n2qgz7k3bv"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out    (assoc-ref %outputs "out"))
                (dest   (string-append out "/bin/ltunzip"))
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
    (synopsis "Lossy Text Uncompression")
    (description "This program uncompresses text files that were compressed
using @dfn{the lossy text compression tool} (ltzip).  Like @code{ltzip},
@code{ltunzip} is also a lossy algorithm, and so performs lossy uncompression.
There is a high probability that the original text file will NOT be returned.
The result file has a \".un\" extension, and  the original ltz file remains.")
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

(define-public psss
  (package
    (name "psss")
    (version "1.00")
    (source
      (origin
        (method url-fetch)
        (uri "http://www.brendangregg.com/Specials/psss")
        (sha256
         (base32
          "1xrxyw13aab1d8d2ibs3ajd6l5hl9my342682ppj4scxqg60gkcn"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out    (assoc-ref %outputs "out"))
                (dest   (string-append out "/bin/psss"))
                (perl   (assoc-ref %build-inputs "perl"))
                (source (assoc-ref %build-inputs "source")))
           (mkdir-p (string-append out "/bin"))
           (copy-file source dest) ; not install-file
           (patch-shebang dest
             (list (string-append perl "/bin")))
           (chmod dest #o555)))))
    (native-inputs `(("source" ,source)))
    (inputs `(("perl" ,perl)))
    (home-page "http://www.brendangregg.com/Specials/psss_chart.html")
    (synopsis "Process Status with StarSign")
    (description "@code{psss} is ps with StarSign, and inserts a field to
display the starsign of the process.
The Starsign is calculated from the proc->p_mstart value.  Some OSes do
maintain this as a \"struct starsign *p_starsign\" in the proc structure, which
this program currently does not use.")
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
    (synopsis "@code{ls} with StarSign")
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
    (synopsis "Whiz files or output streams")
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
    (synopsis "Auto l33t converter")
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
    (synopsis "Heavyweight encryption algorithm: Triple ROT13")
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
    (synopsis "Configurable encryption algorithm: NROT13")
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
    (synopsis "Make zombie processes")
    (description "This program creates one or more zombies and a daemon their
leader.  It can be used to replenish system zombies, or to feed the init
monster.")
    (license license:gpl2+)))
