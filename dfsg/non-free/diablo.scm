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

(define-module (dfsg non-free diablo)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system go)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages sdl))

;; TODO: Unbundle libsmacker, stormlib, radon, pkware, CharisSILB.ttf
(define-public devilutionx
  (package
    (name "devilutionx")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/diasurgical/devilutionX")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0lx903gchda4bgr71469yn63rx5ya6xv9j1azx18nrv3sskrphn4"))
        (modules '((guix build utils)))
        (snippet
         '(begin (delete-file-recursively "3rdParty/asio") #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ; No tests.
       #:configure-flags (list "-DNIGHTLY_BUILD=ON" ; RelWithDebInfo
                               "-DSPAWN=ON")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((asio (assoc-ref inputs "asio")))
               (substitute* "CMakeLists.txt"
                 (("3rdParty/asio") asio)
                 (("sodium_USE_STATIC_LIBS ON") "sodium_USE_STATIC_LIBS OFF"))
               #t)))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (font (string-append out "/share/fonts/truetype"))
                    (icon (string-append out "/share/icons/hicolor")))
               (mkdir-p (string-append icon "/16x16/apps"))
               (mkdir-p (string-append icon "/32x32/apps"))
               (mkdir-p (string-append icon "/48x48/apps"))
               (install-file "devilutionx" bin)
               (install-file (string-append "../" ,name "-" ,version "-checkout/"
                                            "Packaging/resources/CharisSILB.ttf")
                             font)
               (copy-file (string-append "../" ,name "-" ,version "-checkout/"
                                         "Packaging/resources/16.png")
                          (string-append icon "/16x16/apps/devilutionx.png"))
               (copy-file (string-append "../" ,name "-" ,version "-checkout/"
                                         "Packaging/resources/Diablo_32.png")
                          (string-append icon "/32x32/apps/devilutionx.png"))
               (copy-file (string-append "../" ,name "-" ,version "-checkout/"
                                         "Packaging/resources/Diablo_48.png")
                          (string-append icon "/48x48/apps/devilutionx.png"))
               #t))))))
    (native-inputs
     `(("asio" ,asio)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libsodium" ,libsodium)
       ("sdl2-mixer" ,sdl2-mixer)
       ("sdl2-ttf" ,sdl2-ttf)))
    (home-page "https://github.com/diasurgical/devilutionX")
    (synopsis "Diablo build for modern operating systems")
    (description "Diablo build for modern operating systems.")
    (license (list license:unlicense
                   license:silofl1.1)))) ; CharisSILB.ttf

(define diablosw.exe
  (origin
    (method url-fetch)
    (uri "http://ftp.blizzard.com/pub/demos/diablosw.exe")
    (sha256
     (base32
      "1dnkcbwajbijlhh0fq6wgw81pxixgbs8qk0qn0vaj0q3hl3drfyj"))))

(define-public diablo-spawn
  (package
    (name "diablo-spawn")
    (version "0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (let* ((out          (assoc-ref %outputs "out"))
                (bin          (string-append out "/bin"))
                (diablosw.exe (assoc-ref %build-inputs "diablosw.exe"))
                (spawn-mpq    (assoc-ref %build-inputs "spawn-mpq"))
                (devilutionx  (assoc-ref %build-inputs "devilutionx"))
                (desktop      (string-append out "/share/applications"))
                (share        (string-append out "/share/diasurgical/devilution/")))
           (use-modules (guix build utils))
           (mkdir-p bin)
           (mkdir-p desktop)
           (mkdir-p share)
           (install-file (string-append devilutionx "/share/fonts/truetype/CharisSILB.ttf")
                         (string-append out "/share/fonts/truetype"))
           (invoke (string-append spawn-mpq "/bin/spawn_mpq") "-dir" share diablosw.exe)
           (call-with-output-file
             (string-append bin "/spawn")
             (lambda (file)
               (format file
                       "#!/bin/sh
~a/bin/devilutionx --data-dir ~a $@\n"
                       devilutionx share)))
           (chmod (string-append bin "/spawn") #o555)

           (call-with-output-file
             (string-append desktop "/spawn.desktop")
             (lambda (file)
               (format file
                       "[Desktop Entry]~@
                       Name=spawn~@
                       Comment=Diablo I demo~@
                       Exec=~a/bin/spawn~@
                       Type=Application~%" out)))))))
    (inputs
     `(("devilutionx" ,devilutionx)))
    (native-inputs
     `(("diablosw.exe" ,diablosw.exe)
       ("spawn-mpq" ,spawn-mpq)))
    (home-page "")
    (synopsis "")
    (description "")
    (license #f)))

(define-public spawn-mpq
  (let ((commit "58e8f5c50218efc41101803314a7e861537d6c61") ; January 1, 2020
        (revision "1"))
    (package
      (name "spawn-mpq")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/mewspring/spawn_mpq")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "1qgbiim00d4h8i940l608ks0s05pnpjlralcvvxwq7pm8qqdzhcv"))))
      (build-system go-build-system)
      (arguments
       '(#:tests? #f ; No test files.
         #:install-source? #f
         #:import-path "github.com/mewspring/spawn_mpq"))
      (propagated-inputs
       `(("go-github-com-mewrev-pe" ,go-github-com-mewrev-pe)
         ("go-github-com-mewkiz-pkg-httputil" ,go-github-com-mewkiz-pkg-httputil)
         ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)
         ("go-github-com-sanctuary-exp-mpq" ,go-github-com-sanctuary-exp-mpq)))
      (home-page "https://github.com/mewspring/spawn_mpq")
      (synopsis "Tool to extract spawn.mpq from diablosw.exe demo")
      (description
       "Spawn-mpq is a tool to extract spawn.mpq from diablosw.exe demo.")
      (license #f))))

(define-public go-github-com-mewkiz-pkg-httputil
  (let ((commit "518ade7978e2ce16b08e90878fb43cdeed230bde") ; Sept 19, 2019
        (revision "1"))
    (package
      (name "go-github-com-mewkiz-pkg-httputil")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/mewkiz/pkg")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0qw7baq6189g48wgzbdp2ijvkjhmfs1fanzzr0fspn72nkbj548i"))))
      (build-system go-build-system)
      (arguments
       '(#:tests? #f ; No test files.
         #:import-path "github.com/mewkiz/pkg/httputil"
         #:unpack-path "github.com/mewkiz/pkg"))
      (propagated-inputs
       `(("go-golang-org-x-net" ,go-golang-org-x-net)))
      (home-page "github.com/mewkiz/pkg")
      (synopsis "Small utility packages")
      (description "The pkg project provides packages for various utility
functions and commonly used features.")
      (license license:public-domain))))

(define-public go-github-com-mewrev-pe
  (let ((commit "8f6d1d7d219c750096c34991293583e4a6cb5a33") ; Oct. 24, 2018
        (revision "1"))
    (package
      (name "go-github-com-mewrev-pe")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/mewrev/pe")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0zv7piadv3794bwg64r998i5zq5yx5ikxgzgmbr0qsxac6c8zsar"))))
      (build-system go-build-system)
      (arguments
       '(#:tests? #f ; No test files.
         #:import-path "github.com/mewrev/pe"))
      (propagated-inputs
       `(("go-github-com-pkg-errors" ,go-github-com-pkg-errors)))
      (home-page "https://github.com/mewkiz/pkg")
      (synopsis "Access to the Portable Executable (PE) file format")
      (description "Package pe implements access to the @dfn{Portable
Executable} (PE) file format.")
      (license license:public-domain))))

(define-public go-github-com-sanctuary-exp-mpq
  (let ((commit "f737bea33cb613b69667f47fb38c03ba88aad007") ; Jan. 1, 2020
        (revision "1"))
    (package
      (name "go-github-com-sanctuary-exp-mpq")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/sanctuary/exp")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "1njr05gp3d0r2192b6bmlzls49r0iw71ncin0rhn88s0lcx6l56b"))))
      (build-system go-build-system)
      (arguments
       '(#:tests? #f ; Tests fail: src/github.com/sanctuary/exp/mpq/decrypt_test.go:17:10: undefined: hash
         #:import-path "github.com/sanctuary/exp/mpq"
         #:unpack-path "github.com/sanctuary/exp"))
      (propagated-inputs
       `(("go-github-com-egonelbre-exp-bit" ,go-github-com-egonelbre-exp-bit)
         ("go-github-com-pkg-errors" ,go-github-com-pkg-errors)))
      (home-page "https://github.com/sanctuary/exp")
      (synopsis "Tools and libraries related to the Diablo 1 game engine")
      (description "Throw-away prototypes for experimental tools and libraries
related to the Diablo 1 game engine.")
      (license license:public-domain))))

(define-public go-github-com-egonelbre-exp-bit
  (let ((commit "e195833d0f10acae9805025ae800e80db097ed3e") ; Dec. 9, 2019
        (revision "1"))
    (package
      (name "go-github-com-egonelbre-exp-bit")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/egonelbre/exp")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0fzk4xaiw4wqx9dwp3691c0rrid408mdq34ndh5z7ikfffj1vi47"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/egonelbre/exp/bit"
         #:unpack-path "github.com/egonelbre/exp"))
      (home-page "https://github.com/egonelbre/exp")
      (synopsis "Experiments that do not fit into a separate repository")
      (description "Anything you find here should be considered broken, these
are quick hacks to try out some ideas and should not be regarded as completed.
There can be serious bugs lurking around.  Other than that, these might give
some nice ideas how to structure things in Go.")
      (license license:unlicense))))
