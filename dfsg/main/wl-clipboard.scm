 (define-module (dfsg main wl-clipboard)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages xdisorg)
  )

(define-public wl-clipboard-x11
  (package
    (name "wl-clipboard-x11")
    (version "5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/brunelli/wl-clipboard-x11")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1y7jv7rps0sdzmm859wn2l8q4pg2x35smcrm7mbfxn5vrga0bslb"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       '(("src/" "bin/")
         ("man/" "man/man1"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-binary
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out          (assoc-ref outputs "out"))
                   (wl-clipboard (assoc-ref inputs "wl-clipboard")))
               (wrap-program (string-append out "/bin/wl-clipboard-x11")
                `("PATH" prefix (,(string-append wl-clipboard "/bin")))))
             #t))
         (add-after 'wrap-binary 'symlink-utilities
           ;; As seen in the Makefile.
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin/")))
               (symlink "wl-clipboard-x11" (string-append bin "xclip"))
               (symlink "wl-clipboard-x11" (string-append bin "xsel")))
             #t)))))
    (inputs
     `(("wl-clipboard" ,wl-clipboard)))
    (home-page "https://github.com/brunelli/wl-clipboard-x11")
    (synopsis "Use wl-clipboard as a drop-in replacement to X11 clipboard tools")
    (description "This package provides a wrapper script around
@code{x11-clipboard} to use it as a clipboard on X11 also.  It also contains
helper scripts for @code{xclip} and @code{xsel} to assist with the transition.")
    (license license:gpl3+)))
