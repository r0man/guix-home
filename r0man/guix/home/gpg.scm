(define-module (r0man guix home gpg)
  #:use-module (gnu home services)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (home-gpg-services))

(define-record-type* <gpg-agent-configuration>
  gpg-agent-configuration make-gpg-agent-configuration gpg-agent-configuration?
  (default-cache-ttl
    gpg-agent-configuration-default-cache-ttl
    (default 60480000))
  (default-cache-ttl-ssh
    gpg-agent-configuration-default-cache-ttl-ssh
    (default 60480000))
  (max-cache-ttl
   gpg-agent-configuration-max-cache-ttl
   (default 60480000))
  (max-cache-ttl-ssh
   gpg-agent-configuration-max-cache-ttl-ssh
   (default 60480000))
  (pinentry-program
   gpg-agent-configuration-pinentry-program
   (default (file-append pinentry-gtk2 "/bin/pinentry-gtk-2"))))

(define (gpg-agent-config config)
  (mixed-text-file
   "gpg-agent.conf"
   ;; Set the time a cache entry is valid to n seconds. The default is
   ;; 600 seconds. Each time a cache entry is accessed, the entry’s
   ;; timer is reset. To set an entry’s maximum lifetime, use
   ;; max-cache-ttl. Note that a cached passphrase may not be evicted
   ;; immediately from memory if no client requests a cache
   ;; operation. This is due to an internal housekeeping function
   ;; which is only run every few seconds.
   (format #f "default-cache-ttl ~a\n" (gpg-agent-configuration-default-cache-ttl config))

   ;; Set the maximum time a cache entry is valid to n seconds. After
   ;; this time a cache entry will be expired even if it has been
   ;; accessed recently or has been set using
   ;; gpg-preset-passphrase. The default is 2 hours (7200 seconds).
   (format #f "max-cache-ttl ~a\n" (gpg-agent-configuration-max-cache-ttl config))

   ;; Enable SSH support.
   "enable-ssh-support\n"

   ;; Set the time a cache entry used for SSH keys is valid to n
   ;; seconds. The default is 1800 seconds. Each time a cache entry is
   ;; accessed, the entry’s timer is reset. To set an entry’s maximum
   ;; lifetime, use max-cache-ttl-ssh.
   (format #f "default-cache-ttl-ssh ~a\n" (gpg-agent-configuration-default-cache-ttl-ssh config))

   ;; Set the maximum time a cache entry used for SSH keys is valid to
   ;; n seconds. After this time a cache entry will be expired even if
   ;; it has been accessed recently or has been set using
   ;; gpg-preset-passphrase. The default is 2 hours (7200 seconds).
   (format #f "max-cache-ttl-ssh ~a\n" (gpg-agent-configuration-max-cache-ttl-ssh config))

   ;; Use program filename as the PIN entry. The default is
   ;; installation dependent. With the default configuration the name
   ;; of the default pinentry is pinentry; if that file does not exist
   ;; but a pinentry-basic exist the latter is used.
   "pinentry-program " (gpg-agent-configuration-pinentry-program config) "\n"))

(define files
  `((".gnupg/gpg-agent.conf" ,(gpg-agent-config (gpg-agent-configuration)))))

(define packages
  (list gnupg pinentry-gtk2))

(define home-gpg-services
  (list (simple-service 'gpg-config home-files-service-type files)
        (simple-service 'gpg-packages home-profile-service-type packages)))
