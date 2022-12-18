(define-module (r0man home channels)
  #:use-module (gnu home services guix)
  #:use-module (gnu home services)
  #:use-module (guix channels)
  #:export (channels home-channels-services))

(define channels
  (list (channel
         (name 'asahi)
         (url "https://github.com/r0man/asahi-guix.git")
         (branch "main")
         (introduction
          (make-channel-introduction
           "c11f1c583d11b1ed55d34d7041b0c12d51d573e4"
           (openpgp-fingerprint
            "D226 A339 D8DF 4481 5DDE  0CA0 3DDA 5252 7D2A C199"))))
        (channel
         (name 'nonguix)
         (url "https://gitlab.com/nonguix/nonguix")
         (branch "master")
         (commit "d77b0d270ddb3952b80957e15f977b32326a5690")
         (introduction
          (make-channel-introduction
           "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
           (openpgp-fingerprint
            "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))
        (channel
         (name 'r0man)
         (url "https://github.com/r0man/guix-channel.git")
         (branch "main")
         (introduction
          (make-channel-introduction
           "8eb7a76af9b51b80f5c01f18639e6360833fc377"
           (openpgp-fingerprint
            "D226 A339 D8DF 4481 5DDE  0CA0 3DDA 5252 7D2A C199"))))
        (channel
         (name 'asahi)
         (url "https://github.com/r0man/asahi-guix.git")
         (branch "main")
         (introduction
          (make-channel-introduction
           "d8f93a8a57be3e38f13916b4e5f3e1ad5ccee9de"
           (openpgp-fingerprint
            "D226 A339 D8DF 4481 5DDE  0CA0 3DDA 5252 7D2A C199"))))
        (channel
         (name 'guix)
         (url "https://git.savannah.gnu.org/git/guix.git")
         (branch "master")
         (commit "8548ba5847417eabe14aa08fa9861f91743bd3a1")
         (introduction
          (make-channel-introduction
           "9edb3f66fd807b096b48283debdcddccfea34bad"
           (openpgp-fingerprint
            "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))))

(define home-channels-services
  (list (service home-channels-service-type channels)))

channels
