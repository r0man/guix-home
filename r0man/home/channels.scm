(define-module (r0man home channels)
  #:use-module (guix channels)
  #:export (channels))

(define channels
  (list (channel
         (name 'flat)
         (url "https://github.com/flatwhatson/guix-channel.git")
         (branch "master")
         (commit
          "657da22f0229b978b7bf4e4d476f59f17f6a175f")
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
          "1a122e06fe046caebf39395edc797515861acd3b")
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
          "c9b4bb44a37e770a4aa4c9860352200294087ab7")
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
          "89db6e7ec77bf0f33474e47945755f5ab45e9f06")
         (introduction
          (make-channel-introduction
           "9edb3f66fd807b096b48283debdcddccfea34bad"
           (openpgp-fingerprint
            "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))))

channels
