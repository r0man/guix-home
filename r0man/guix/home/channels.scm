(define-module (r0man guix home channels)
  #:use-module (gnu home services guix)
  #:use-module (gnu home services)
  #:use-module (guix channels)
  #:use-module (guix ci)
  #:export (channels home-channels-services))

(define channels
  (list (channel
         (name 'asahi)
         (branch "main")
         (url "https://github.com/asahi-guix/channel")
         (introduction
          (make-channel-introduction
           "3eeb493b037bea44f225c4314c5556aa25aff36c"
           (openpgp-fingerprint
            "D226 A339 D8DF 4481 5DDE  0CA0 3DDA 5252 7D2A C199"))))

        (channel
         (name 'guix)
         (url "https://github.com/asahi-guix/guix")
         (branch "main")
         (introduction
          (make-channel-introduction
           "eb0d1bbbe54347b19b2fe93dac9c43b540d6cdcf"
           (openpgp-fingerprint
            "D226 A339 D8DF 4481 5DDE  0CA0 3DDA 5252 7D2A C199"))))

        (channel
         (name 'nonguix)
         (url "https://gitlab.com/nonguix/nonguix")
         (branch "master")
         (introduction
          (make-channel-introduction
           "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
           (openpgp-fingerprint
            "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5"))))

        (channel
         (name 'r0man)
         (url "https://github.com/r0man/guix-channel")
         (branch "main")
         (introduction
          (make-channel-introduction
           "8eb7a76af9b51b80f5c01f18639e6360833fc377"
           (openpgp-fingerprint
            "D226 A339 D8DF 4481 5DDE  0CA0 3DDA 5252 7D2A C199"))))))

(define home-channels-services
  (list (service home-channels-service-type channels)))

channels
