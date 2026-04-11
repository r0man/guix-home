(define-module (r0man guix system services qemu)
  #:use-module (gnu services)
  #:use-module (gnu services virtualization)
  #:export (%qemu-service-aarch64
            %qemu-service-x86-64))

(define %qemu-service-aarch64
  (service qemu-binfmt-service-type
           (qemu-binfmt-configuration
            (platforms (lookup-qemu-platforms "x86_64")))))

(define %qemu-service-x86-64
  (service qemu-binfmt-service-type
           (qemu-binfmt-configuration
            (platforms (lookup-qemu-platforms "aarch64")))))
