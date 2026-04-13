(define-module (r0man guix system services libvirt)
  #:use-module (gnu services)
  #:use-module (gnu services virtualization)
  #:export (%libvirt-service))

(define %libvirt-service
  (service libvirt-service-type
           (libvirt-configuration
            (firmwares (list))
            (unix-sock-group "libvirt")
            (tls-port "16555"))))
