;; -*- mode: scheme; -*-
;;;
;;; Channels specification for `guix pull'.
;;;
;;; IMPORTANT: this file is evaluated by `guix pull' in a SANDBOX that only
;;; provides the channel data constructors (`channel', `make-channel-introduction',
;;; `openpgp-fingerprint').  It MUST stay pure data: do NOT add `(define-module ...)'
;;; or `(use-modules ...)' here — newer Guix rejects both with
;;; "define-module: unbound variable" / "use-modules: unbound variable".
;;;
;;; The importable module with the same channels lives at
;;; modules/r0man/guix/channels.scm (used by system/m1.scm and home/channels.scm).
;;; Keep the two in sync; this file is what ~/.config/guix/channels.scm points to.
;;;
;;; Symlink setup:
;;;   ln -sf "$(pwd)/channels.scm" ~/.config/guix/channels.scm

(list
 (channel
  (name 'asahi)
  (branch "main")
  (url "https://codeberg.org/asahi-guix/channel")
  (introduction
   (make-channel-introduction
    "3eeb493b037bea44f225c4314c5556aa25aff36c"
    (openpgp-fingerprint
     "D226 A339 D8DF 4481 5DDE  0CA0 3DDA 5252 7D2A C199"))))
 (channel
  (name 'guix)
  (url "https://git.guix.gnu.org/guix.git")
  (commit "243d892254170f4d23ff9622f0d833827c67c6c4")
  (introduction
   (make-channel-introduction
    "9edb3f66fd807b096b48283debdcddccfea34bad"
    (openpgp-fingerprint
     "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
 (channel
  (name 'nonguix)
  (url "https://gitlab.com/nonguix/nonguix")
  (branch "master")
  (commit "5f2630e69fbbe9e79c350a67545f0fef7e93e223")
  (introduction
   (make-channel-introduction
    "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
    (openpgp-fingerprint
     "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
 (channel
  (name 'r0man-guix)
  (url "https://github.com/r0man/guix-channel")
  (branch "main")
  (introduction
   (make-channel-introduction
    "8eb7a76af9b51b80f5c01f18639e6360833fc377"
    (openpgp-fingerprint
     "D226 A339 D8DF 4481 5DDE  0CA0 3DDA 5252 7D2A C199"))))
 (channel
  (name 'r0man-home)
  (url "https://github.com/r0man/guix-home")
  (branch "main")
  (introduction
   (make-channel-introduction
    "fdf26126b62dd922620ef3ce922b71180e57f455"
    (openpgp-fingerprint
     "D226 A339 D8DF 4481 5DDE  0CA0 3DDA 5252 7D2A C199")))))
