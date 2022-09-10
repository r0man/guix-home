(define-module (r0man home channels)
  #:use-module (guix channels)
  #:export (channels))

(define channels
  (list (channel
         (name 'flat)
         (url "https://github.com/flatwhatson/guix-channel.git")
         (branch "master")
         (commit
          "e57424b680e1724105e2598b68c30084b180cf58")
         (introduction
          (make-channel-introduction
           "33f86a4b48205c0dc19d7c036c85393f0766f806"
           (openpgp-fingerprint
            "736A C00E 1254 378B A982  7AF6 9DBE 8265 81B6 4490"))))
        (channel
         (name 'nonguix)
         (url "https://gitlab.com/nonguix/nonguix")
         (branch "master")
         (commit
          "9563de3037f3e41ac3c0e99176047ce73003b0e6")
         (introduction
          (make-channel-introduction
           "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
           (openpgp-fingerprint
            "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
        (channel
         (name 'r0man)
         (url "https://github.com/r0man/guix-channel.git")
         (branch "main")
         (commit
          "f6dc054ca4e23a8ff9dcfbaf7c570025a0eabd46")
         (introduction
          (make-channel-introduction
           "8eb7a76af9b51b80f5c01f18639e6360833fc377"
           (openpgp-fingerprint
            "D226 A339 D8DF 4481 5DDE  0CA0 3DDA 5252 7D2A C199"))))
        (channel
         (name 'guix)
         (url "https://git.savannah.gnu.org/git/guix.git")
         (branch "master")
         (commit
          "b45a44eaad890f31d9418dbb8cb14e3ee1d83c19")
         (introduction
          (make-channel-introduction
           "9edb3f66fd807b096b48283debdcddccfea34bad"
           (openpgp-fingerprint
            "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))))

channels
