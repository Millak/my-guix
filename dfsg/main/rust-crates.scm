;;; Copyright © 2025 Hilton Chain <hako@ultrarare.space>
;;; Copyright © 2025 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (dfsg main rust-crates)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (gnu packages rust-sources)
  #:export (lookup-cargo-inputs))

(define rust-adblock-0.5.6
  (crate-source "adblock" "0.5.6"
                "05c3iw2i6nyimxknd4avg61ibv7mzpzfrqgkvv98zj4vp3smwy17"))

(define rust-addr-0.14.0
  (crate-source "addr" "0.14.0"
                "0w6v0wwv203v0lyvwsq27rwfhvmw7dsmrqc9r3gszbx2974wlk65"))

(define rust-adler-1.0.2
  (crate-source "adler" "1.0.2"
                "1zim79cvzd5yrkzl3nyfx0avijwgk9fqv3yrscdy1cc79ih02qpj"))

(define rust-aho-corasick-0.7.18
  (crate-source "aho-corasick" "0.7.18"
                "0vv50b3nvkhyy7x7ip19qnsq11bqlnffkmj2yx2xlyk5wzawydqy"))

(define rust-autocfg-1.1.0
  (crate-source "autocfg" "1.1.0"
                "1ylp3cb47ylzabimazvbz9ms6ap784zhb6syaz6c1jqpmcmq0s6l"))

(define rust-base64-0.13.0
  (crate-source "base64" "0.13.0"
                "1z82g23mbzjgijkpcrilc7nljpxpvpf7zxf6iyiapkgka2ngwkch"))

(define rust-bitflags-1.3.2
  (crate-source "bitflags" "1.3.2"
                "12ki6w8gn1ldq7yz9y680llwk5gmrhrzszaa17g1sbrw2r2qvwxy"))

(define rust-byteorder-1.4.3
  (crate-source "byteorder" "1.4.3"
                "0456lv9xi1a5bcm32arknf33ikv76p3fr9yzki4lb2897p2qkh8l"))

(define rust-cfg-if-1.0.0
  (crate-source "cfg-if" "1.0.0"
                "1za0vb97n4brpzpv8lsbnzmq5r8f2b0cpqqr0sy8h5bn751xxwds"))

(define rust-crc32fast-1.3.2
  (crate-source "crc32fast" "1.3.2"
                "03c8f29yx293yf43xar946xbls1g60c207m9drf8ilqhr25vsh5m"))

(define rust-either-1.7.0
  (crate-source "either" "1.7.0"
                "1gl1cybx0rrcvny4rcfl2agqbj6n0vz5bb1ws57sdhmgns3pn41z"))

(define rust-flate2-1.0.24
  (crate-source "flate2" "1.0.24"
                "1xmzzg91c0hdl39qz0hwph0w629bva1dh21j3zyqp7xd4x60yazq"))

(define rust-form-urlencoded-1.0.1
  (crate-source "form_urlencoded" "1.0.1"
                "1491fmakavcmsjbm3q6iy0bhmn9l422jasdhzx5hkljgza3mmhjz"))

(define rust-idna-0.2.3
  (crate-source "idna" "0.2.3"
                "1y7ca2w5qp9msgl57n03zqp78gq1bk2crqzg6kv7a542mdphm2j1"))

(define rust-indoc-1.0.6
  (crate-source "indoc" "1.0.6"
                "0vkxja3mdjgq9hjbsvq4ql9dbfdphw01hmbqndldkr9rjc0vv805"))

(define rust-itertools-0.10.3
  (crate-source "itertools" "0.10.3"
                "1qy55fqbaisr9qgbn7cvdvqlfqbh1f4ddf99zwan56z7l6gx3ad9"))

(define rust-libc-0.2.126
  (crate-source "libc" "0.2.126"
                "0diqlpap4pq81284368vffjwvp9jg9hp2x03s7hlk2yj3icmm79l"))

(define rust-lock-api-0.4.7
  (crate-source "lock_api" "0.4.7"
                "0lwckl9l51y69bwf854kmdmmr1543spbxaa9xjclc3lllsvaazrj"))

(define rust-matches-0.1.9
  (crate-source "matches" "0.1.9"
                "0gw5ib38jfgyyah8nyyxr036grqv1arkf1srgfa4h386dav7iqx3"))

(define rust-memchr-2.5.0
  (crate-source "memchr" "2.5.0"
                "0vanfk5mzs1g1syqnj03q8n0syggnhn55dq535h2wxr7rwpfbzrd"))

(define rust-miniz-oxide-0.5.3
  (crate-source "miniz_oxide" "0.5.3"
                "1k1wfxb35v129mhqy14yqhrj3wvknafrwygiq7zvi0m5iml7ap3g"))

(define rust-num-traits-0.2.15
  (crate-source "num-traits" "0.2.15"
                "1kfdqqw2ndz0wx2j75v9nbjx7d3mh3150zs4p5595y02rwsdx3jp"))

(define rust-once-cell-1.13.0
  (crate-source "once_cell" "1.13.0"
                "1qfqvgnwfzzwxd13ybvplzshaqwnjnna9ghcn0zgijaq0zixp9hq"))

(define rust-parking-lot-0.12.1
  (crate-source "parking_lot" "0.12.1"
                "13r2xk7mnxfc5g0g6dkdxqdqad99j7s7z8zhzz4npw5r0g0v4hip"))

(define rust-parking-lot-core-0.9.3
  (crate-source "parking_lot_core" "0.9.3"
                "0ab95rljb99rm51wcic16jgbajcr6lgbqkrr21w7bc2wyb5pk8h9"))

(define rust-paste-1.0.7
  (crate-source "paste" "1.0.7"
                "1z15h1rnq1wcacpcvgm77djl3413gs1nlhmn90qpcvjx2c2hwlhc"))

(define rust-percent-encoding-2.1.0
  (crate-source "percent-encoding" "2.1.0"
                "0bp3zrsk3kr47fbpipyczidbbx4g54lzxdm77ni1i3qws10mdzfl"))

(define rust-proc-macro2-1.0.40
  (crate-source "proc-macro2" "1.0.40"
                "1xyazdlnqmnkapjah7mjbanzb0zc4i4z5rgaz0vw75i5xpla35nx"))

(define rust-psl-2.0.90
  (crate-source "psl" "2.0.90"
                "06zlxjhw5idyqlmzpr85fqs1ndsyjspa7qh9imsjbdar4qv7v73y"))

(define rust-psl-types-2.0.10
  (crate-source "psl-types" "2.0.10"
                "0w74li516dsalxmsk5mfcqbgdbg0dl04qdv2iggszjly5p3agvg8"))

(define rust-pyo3-0.16.5
  (crate-source "pyo3" "0.16.5"
                "1p5kjsj3jdw2gnahdjrzljmi93w3nxdp11qq8x3i80b0a3l04qqy"))

(define rust-pyo3-build-config-0.16.5
  (crate-source "pyo3-build-config" "0.16.5"
                "1j2jj5qnnpagi3gvkwjpydcxfsd5qv3vmpghnaqs7n1mdia5pdmm"))

(define rust-pyo3-ffi-0.16.5
  (crate-source "pyo3-ffi" "0.16.5"
                "0avls4q393nmzhb124zg6kp5lj6xzy2f6qx564qa7b614xqs0xf2"))

(define rust-pyo3-macros-0.16.5
  (crate-source "pyo3-macros" "0.16.5"
                "1xwh7sl4n73746q80n5m5afd261zg0kxcqfnlr89ik7vbd4c8kr8"))

(define rust-pyo3-macros-backend-0.16.5
  (crate-source "pyo3-macros-backend" "0.16.5"
                "1bvzvdx2a6hhliny12n2vy7v7gbsgzanxjckjr1cbxbkizss1gak"))

(define rust-quote-1.0.20
  (crate-source "quote" "1.0.20"
                "015qrb5jf9q0pajx38mfn431gfqn0hv2kc1ssarbqvvpx49g5k9v"))

(define rust-redox-syscall-0.2.13
  (crate-source "redox_syscall" "0.2.13"
                "0hpgwvgjlg1j9z7bjf5y18fkd8ag7y4znhqxg85hnpp5qz25pwk2"))

(define rust-regex-1.6.0
  (crate-source "regex" "1.6.0"
                "12wqvyh4i75j7pc8sgvmqh4yy3qaj4inc4alyv1cdf3lf4kb6kjc"))

(define rust-regex-syntax-0.6.27
  (crate-source "regex-syntax" "0.6.27"
                "0i32nnvyzzkvz1rqp2qyfxrp2170859z8ck37jd63c8irrrppy53"))

(define rust-rmp-0.8.11
  (crate-source "rmp" "0.8.11"
                "17rw803xv84csxgd654g7q64kqf9zgkvhsn8as3dbmlg6mr92la4"))

(define rust-rmp-serde-0.13.7
  (crate-source "rmp-serde" "0.13.7"
                "1lqclnffx1b3r1faicscmk9j21mijl9bj7ywgjps77vf8ic1s7h1"))

(define rust-rmp-serde-0.15.5
  (crate-source "rmp-serde" "0.15.5"
                "178f4qlicldm9iy74q4wdqldk5i11p1ad30wzs9avx04mpwwygkj"))

(define rust-scopeguard-1.1.0
  (crate-source "scopeguard" "1.1.0"
                "1kbqm85v43rq92vx7hfiay6pmcga03vrjbbfwqpyj3pwsg3b16nj"))

(define rust-seahash-3.0.7
  (crate-source "seahash" "3.0.7"
                "0iqg12lxkn0ivsfa1gkylcwj5wmi6zl87mbizlrkg918s6hprxaq"))

(define rust-serde-1.0.139
  (crate-source "serde" "1.0.139"
                "1mna8q52l2qc2ipqw8jwbxplrs7d7sdqbq5f8j5scnp4i6wfnw81"))

(define rust-serde-derive-1.0.139
  (crate-source "serde_derive" "1.0.139"
                "1yrxaj1jviv99z14mn59sppmbgc4szhzp3xdb2pk4yfyq4q347fw"))

(define rust-smallvec-1.9.0
  (crate-source "smallvec" "1.9.0"
                "1lfss4vs5z5njm3ac9c499s5m1gphzm5a7gxcbw1zncpjmsdpl1g"))

(define rust-syn-1.0.98
  (crate-source "syn" "1.0.98"
                "1pbklw6fnwwgrkj8qz3wcjfggmn7vmyln44gg0yc5r2dj25fy2n5"))

(define rust-target-lexicon-0.12.4
  (crate-source "target-lexicon" "0.12.4"
                "1hfk4v8gbhczr6jwsy1ja6yg4npkvznym6b7r4fbgjc0fw428960"))

(define rust-tinyvec-1.6.0
  (crate-source "tinyvec" "1.6.0"
                "0l6bl2h62a5m44jdnpn7lmj14rd44via8180i7121fvm73mmrk47"))

(define rust-tinyvec-macros-0.1.0
  (crate-source "tinyvec_macros" "0.1.0"
                "0p5zvgbas5nh403fbxica819mf3g83n8g2hzpfazfr56w6klv9yd"))

(define rust-twoway-0.2.2
  (crate-source "twoway" "0.2.2"
                "0iqb54firzb8jinl2674vz8s6c4h30842sa3v9pcs93w1m3gnzy5"))

(define rust-unchecked-index-0.2.2
  (crate-source "unchecked-index" "0.2.2"
                "0p6qcai1mjayx59cpgk27d0zgw9hz9r1ira5jiqil66f4ba8dfpf"))

(define rust-unicode-bidi-0.3.8
  (crate-source "unicode-bidi" "0.3.8"
                "14p95n9kw9p7psp0vsp0j9yfkfg6sn1rlnymvmwmya0x60l736q9"))

(define rust-unicode-ident-1.0.2
  (crate-source "unicode-ident" "1.0.2"
                "19zf5lzhzix2s35lp5lckdy90sw0kfi5a0ii49d24dcj7yk1pihm"))

(define rust-unicode-normalization-0.1.21
  (crate-source "unicode-normalization" "0.1.21"
                "1rk0zci96pc26sk21nwk5cdkxb3p6bfani0dhaff2smwyz2bsk45"))

(define rust-unindent-0.1.9
  (crate-source "unindent" "0.1.9"
                "0i1sjxlhw7rbvq47xgk0lixgpnswfyks21ks6zgzfw75lccybzjj"))

(define rust-url-2.2.2
  (crate-source "url" "2.2.2"
                "132pzpvfvpw33gjlzqd55n5iag9qddzffq8qbp1myfykna1w61x5"))

(define rust-windows-aarch64-msvc-0.36.1
  (crate-source "windows_aarch64_msvc" "0.36.1"
                "0ixaxs2c37ll2smprzh0xq5p238zn8ylzb3lk1zddqmd77yw7f4v"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-i686-gnu-0.36.1
  (crate-source "windows_i686_gnu" "0.36.1"
                "1dm3svxfzamrv6kklyda9c3qylgwn5nwdps6p0kc9x6s077nq3hq"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-i686-msvc-0.36.1
  (crate-source "windows_i686_msvc" "0.36.1"
                "097h2a7wig04wbmpi3rz1akdy4s8gslj5szsx8g2v0dj91qr3rz2"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-sys-0.36.1
  (crate-source "windows-sys" "0.36.1"
                "1lmqangv0zg1l46xiq7rfnqwsx8f8m52mqbgg2mrx7x52rd1a17a"))

(define rust-windows-x86-64-gnu-0.36.1
  (crate-source "windows_x86_64_gnu" "0.36.1"
                "1qfrck3jnihymfrd01s8260d4snql8ks2p8yaabipi3nhwdigkad"
                #:snippet '(delete-file-recursively "lib")))

(define rust-windows-x86-64-msvc-0.36.1
  (crate-source "windows_x86_64_msvc" "0.36.1"
                "103n3xijm5vr7qxr1dps202ckfnv7njjnnfqmchg8gl5ii5cl4f8"
                #:snippet '(delete-file-recursively "lib")))

(define-cargo-inputs lookup-cargo-inputs
                     (python-adblock =>
                                     (list rust-adblock-0.5.6
                                           rust-addr-0.14.0
                                           rust-adler-1.0.2
                                           rust-aho-corasick-0.7.18
                                           rust-autocfg-1.1.0
                                           rust-base64-0.13.0
                                           rust-bitflags-1.3.2
                                           rust-byteorder-1.4.3
                                           rust-cfg-if-1.0.0
                                           rust-crc32fast-1.3.2
                                           rust-either-1.7.0
                                           rust-flate2-1.0.24
                                           rust-form-urlencoded-1.0.1
                                           rust-idna-0.2.3
                                           rust-indoc-1.0.6
                                           rust-itertools-0.10.3
                                           rust-libc-0.2.126
                                           rust-lock-api-0.4.7
                                           rust-matches-0.1.9
                                           rust-memchr-2.5.0
                                           rust-miniz-oxide-0.5.3
                                           rust-num-traits-0.2.15
                                           rust-once-cell-1.13.0
                                           rust-parking-lot-0.12.1
                                           rust-parking-lot-core-0.9.3
                                           rust-paste-1.0.7
                                           rust-percent-encoding-2.1.0
                                           rust-proc-macro2-1.0.40
                                           rust-psl-2.0.90
                                           rust-psl-types-2.0.10
                                           rust-pyo3-0.16.5
                                           rust-pyo3-build-config-0.16.5
                                           rust-pyo3-ffi-0.16.5
                                           rust-pyo3-macros-0.16.5
                                           rust-pyo3-macros-backend-0.16.5
                                           rust-quote-1.0.20
                                           rust-redox-syscall-0.2.13
                                           rust-regex-1.6.0
                                           rust-regex-syntax-0.6.27
                                           rust-rmp-0.8.11
                                           rust-rmp-serde-0.13.7
                                           rust-rmp-serde-0.15.5
                                           rust-scopeguard-1.1.0
                                           rust-seahash-3.0.7
                                           rust-serde-1.0.139
                                           rust-serde-derive-1.0.139
                                           rust-smallvec-1.9.0
                                           rust-syn-1.0.98
                                           rust-target-lexicon-0.12.4
                                           rust-tinyvec-1.6.0
                                           rust-tinyvec-macros-0.1.0
                                           rust-twoway-0.2.2
                                           rust-unchecked-index-0.2.2
                                           rust-unicode-bidi-0.3.8
                                           rust-unicode-ident-1.0.2
                                           rust-unicode-normalization-0.1.21
                                           rust-unindent-0.1.9
                                           rust-url-2.2.2
                                           rust-windows-sys-0.36.1
                                           rust-windows-aarch64-msvc-0.36.1
                                           rust-windows-i686-gnu-0.36.1
                                           rust-windows-i686-msvc-0.36.1
                                           rust-windows-x86-64-gnu-0.36.1
                                           rust-windows-x86-64-msvc-0.36.1)))
