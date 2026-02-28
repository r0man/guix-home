(define-module (r0man guix home agent)
  #:use-module (gnu home services containers)
  #:use-module (gnu home services)
  #:use-module (gnu services containers)
  #:use-module (gnu services)
  #:use-module (guix records)
  #:export (home-agent-configuration
            home-agent-service-type))

;;; Commentary:
;;;
;;; Home service for running an agent environment in an OCI container
;;; via rootless Podman.  Mounts workspace and SSH from the host.
;;;
;;; The `image-value' field accepts any value suitable for the
;;; `oci-image' `value' field: a manifest, operating-system, gexp,
;;; or file-like object.
;;;
;;; Usage in a host system config (e.g., m1.scm):
;;;
;;;   (service home-oci-service-type
;;;            (for-home (oci-configuration (runtime 'podman))))
;;;   (service home-agent-service-type
;;;            (home-agent-configuration
;;;             (user "roman")
;;;             (image-value agent-operating-system)))
;;;
;;; Code:

(define-record-type* <home-agent-configuration>
  home-agent-configuration make-home-agent-configuration
  home-agent-configuration?
  (user home-agent-user
        (description "Username for host-side volume mount paths."))
  (image-value home-agent-image-value
               (description "Value for the OCI image: a manifest, \
operating-system, gexp, or file-like object."))
  (extra-volumes home-agent-extra-volumes
                 (default '())
                 (description "Additional volume mounts as strings or pairs.")))

(define (home-agent-default-volumes user)
  "Return default volume mount strings for USER."
  (let ((home (string-append "/home/" user)))
    (list (string-append home "/workspace:" home "/workspace")
          (string-append home "/.ssh:" home "/.ssh:ro"))))

(define (home-agent-oci-extension config)
  "Return an OCI extension with the agent container configuration."
  (let ((user (home-agent-user config)))
    (oci-extension
     (containers
      (list
       (oci-container-configuration
        (image (oci-image
                (repository "guix/agent")
                (tag "latest")
                (value (home-agent-image-value config))))
        (provision "agent")
        (network "host")
        (container-user user)
        (volumes (append (home-agent-default-volumes user)
                         (home-agent-extra-volumes config)))))))))

(define home-agent-service-type
  (service-type
   (name 'home-agent)
   (extensions
    (list (service-extension home-oci-service-type
                             home-agent-oci-extension)))
   (description
    "Run the agent environment in an OCI container.")))
