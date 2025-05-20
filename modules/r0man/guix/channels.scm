(define-module (r0man guix channels)
  #:use-module (guix channels))

(define-public asahi-channel
  (channel
   (name 'asahi)
   (branch "main")
   (url "https://codeberg.org/asahi-guix/channel")
   (introduction
    (make-channel-introduction
     "3eeb493b037bea44f225c4314c5556aa25aff36c"
     (openpgp-fingerprint
      "D226 A339 D8DF 4481 5DDE  0CA0 3DDA 5252 7D2A C199")))))

(define-public guix-channel
  (channel
   (name 'guix)
   (url "https://codeberg.org/guix/guix-mirror")
   (introduction
    (make-channel-introduction
     "9edb3f66fd807b096b48283debdcddccfea34bad"
     (openpgp-fingerprint
      "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))

(define-public nonguix-channel
  (channel
   (name 'nonguix)
   (url "https://gitlab.com/nonguix/nonguix")
   (branch "master")
   (introduction
    (make-channel-introduction
     "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
     (openpgp-fingerprint
      "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5")))))

(define-public r0man-guix-channel
  (channel
   (name 'r0man-guix)
   (url "https://github.com/r0man/guix-channel")
   (branch "main")
   (introduction
    (make-channel-introduction
     "8eb7a76af9b51b80f5c01f18639e6360833fc377"
     (openpgp-fingerprint
      "D226 A339 D8DF 4481 5DDE  0CA0 3DDA 5252 7D2A C199")))))

(define-public r0man-home-channel
  (channel
   (name 'r0man-home)
   (url "https://github.com/r0man/guix-home")
   (branch "main")
   (introduction
    (make-channel-introduction
     "c07650c8ad1d275b6a6bbae0cb31dbfe769c7045"
     (openpgp-fingerprint
      "D226 A339 D8DF 4481 5DDE  0CA0 3DDA 5252 7D2A C199")))))

(define-public channels
  (list asahi-channel
        guix-channel
        nonguix-channel
        r0man-guix-channel))

channels
