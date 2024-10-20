(define-module (r0man guix system services)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services admin)
  #:use-module (gnu services auditd)
  #:use-module (gnu services avahi)
  #:use-module (gnu services base)
  #:use-module (gnu services cups)
  #:use-module (gnu services databases)
  #:use-module (gnu services desktop)
  #:use-module (gnu services docker)
  #:use-module (gnu services networking)
  #:use-module (gnu services nix)
  #:use-module (gnu services security-token)
  #:use-module (gnu services ssh)
  #:use-module (gnu services virtualization)
  #:use-module (gnu services web)
  #:use-module (gnu services xorg)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (r0man guix system base)
  #:use-module (r0man guix system keyboard)
  #:use-module (r0man guix system xorg)
  #:export (%auditd-service-type
            %avahi-service
            %bluetooth-service
            %containerd-service
            %cups-service
            %docker-service
            %elogind-service
            %guix-publish-service
            %libvirt-service
            %nix-service
            %openssh-service
            %pcscd-service
            %postgresql-service
            %qemu-service-aarch64
            %qemu-service-x86-64
            %screen-locker-service
            %slim-service
            %udev-fido2-service
            certbot-ssl-certificate
            certbot-ssl-certificate-key
            console-font-service-config
            guix-service-type-config))

(define %auditd-service-type
  (service auditd-service-type))

(define %avahi-service
  (service avahi-service-type))

(define %bluetooth-service
  (service bluetooth-service-type))

(define %cups-service
  (service cups-service-type
           (cups-configuration
            (web-interface? #t)
            (extensions
             (list cups-filters)))))

(define %containerd-service
  (service containerd-service-type))

(define %docker-service
  (service docker-service-type))

(define %elogind-service
  (service elogind-service-type))

(define %guix-publish-service
  (service guix-publish-service-type
           (guix-publish-configuration
            (compression '(("zstd" 3)))
            (port 8082))))

(define (certbot-ssl-certificate domain)
  (format #f "/etc/letsencrypt/live/~a/fullchain.pem" domain))

(define (certbot-ssl-certificate-key domain)
  (format #f "/etc/letsencrypt/live/~a/privkey.pem" domain))

(define %libvirt-service
  (service libvirt-service-type
           (libvirt-configuration
            (unix-sock-group "libvirt")
            (tls-port "16555"))))

(define %nix-service
  (service nix-service-type))

(define %openssh-service
  (service openssh-service-type
           (openssh-configuration
            (openssh openssh)
            (permit-root-login 'prohibit-password)
            (port-number 22)
            (x11-forwarding? #t))))

(define %pcscd-service
  (service pcscd-service-type))

(define %postgresql-service
  (service postgresql-service-type
           (postgresql-configuration
            (postgresql postgresql-15))))

(define %qemu-service-aarch64
  (service qemu-binfmt-service-type
           (qemu-binfmt-configuration
            (platforms (lookup-qemu-platforms "x86_64")))))

(define %qemu-service-x86-64
  (service qemu-binfmt-service-type
           (qemu-binfmt-configuration
            (platforms (lookup-qemu-platforms "aarch64")))))

(define %slim-service
  (service slim-service-type
           (slim-configuration
            (xorg-configuration
             (xorg-configuration
              (keyboard-layout %keyboard-layout)
              (extra-config (list %xorg-libinput-config)))))))

(define %screen-locker-service
  (service screen-locker-service-type
           (screen-locker-configuration
            (name "slock")
            (program (file-append slock "/bin/slock")))))

(define %udev-fido2-service
  (udev-rules-service 'fido2 libfido2 #:groups '("plugdev")))

(define (console-font-service-config config)
  (map (lambda (tty)
         (cons tty (file-append font-terminus "/share/consolefonts/ter-132n")))
       '("tty1" "tty2" "tty3" "tty4" "tty5" "tty6")))

(define (guix-service-type-config config)
  (guix-configuration
   (inherit config)
   (substitute-urls
    (append %default-substitute-urls
            (list "https://substitutes.asahi-guix.org"
                  "https://substitutes.nonguix.org")))
   (authorized-keys
    (append (list (local-file "./keys/asahi-guix.pub")
                  (local-file "./keys/nonguix.pub")
                  (local-file "./keys/precision.pub"))
            %default-authorized-guix-keys))))
