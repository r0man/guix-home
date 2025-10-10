(define-module (r0man guix home mbsync)
  #:use-module (gnu home services mcron)
  #:use-module (gnu home services)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:export (home-mbsync-configuration
            home-mbsync-service-type))

;;; Commentary:
;;;
;;; Home service for mbsync (isync) mail synchronization.
;;; Manages ~/.mbsyncrc configuration and sets up periodic sync via mcron.
;;;
;;; Code:

(define default-mbsync-config
  (mixed-text-file
   "mbsyncrc"
   "IMAPAccount burningswell\n"
   "Host imap.gmail.com\n"
   "User roman.scherer@burningswell.com\n"
   "PassCmd \"" (file-append password-store "/bin/pass") " Gmail/Burningswell\"\n"
   "TLSType IMAPS\n"
   "TLSVersions +1.3\n"
   "CertificateFile /etc/ssl/certs/ca-certificates.crt\n"
   "\n"

   "IMAPStore burningswell-remote\n"
   "Account burningswell\n"
   "\n"

   "MaildirStore burningswell-local\n"
   "Path ~/Mail/burningswell/\n"
   "Inbox ~/Mail/burningswell/inbox\n"
   "Trash ~/Mail/burningswell/trash\n"
   "SubFolders Verbatim\n"
   "\n"

   "Channel burningswell\n"
   "Far :burningswell-remote:\n"
   "Near :burningswell-local:\n"
   "Patterns * ![Gmail]* \"[Gmail]/Sent Mail\" \"[Gmail]/Starred\" \"[Gmail]/All Mail\" \"[Gmail]/Trash\"\n"
   "Expunge None\n"
   "CopyArrivalDate yes\n"
   "Sync All\n"
   "Create Both\n"
   "SyncState *\n"
   "\n"

   "IMAPAccount nubank\n"
   "Host imap.gmail.com\n"
   "User roman.scherer@nubank.com.br\n"
   "PassCmd \"" (file-append password-store "/bin/pass") " Gmail/Nubank\"\n"
   "TLSType IMAPS\n"
   "TLSVersions +1.3\n"
   "CertificateFile /etc/ssl/certs/ca-certificates.crt\n"
   "\n"

   "IMAPStore nubank-remote\n"
   "Account nubank\n"
   "\n"

   "MaildirStore nubank-local\n"
   "Path ~/Mail/nubank/\n"
   "Inbox ~/Mail/nubank/inbox\n"
   "Trash ~/Mail/nubank/trash\n"
   "SubFolders Verbatim\n"
   "\n"

   "Channel nubank\n"
   "Far :nubank-remote:\n"
   "Near :nubank-local:\n"
   "Patterns * ![Gmail]* \"[Gmail]/Sent Mail\" \"[Gmail]/Starred\" \"[Gmail]/All Mail\" \"[Gmail]/Trash\"\n"
   "Expunge None\n"
   "CopyArrivalDate yes\n"
   "Sync All\n"
   "Create Both\n"
   "SyncState *\n"))

(define-record-type* <home-mbsync-configuration>
  home-mbsync-configuration make-home-mbsync-configuration
  home-mbsync-configuration?
  (config-file home-mbsync-config-file
               (default default-mbsync-config)
               (description "Path to .mbsyncrc configuration file."))
  (enable-cron? home-mbsync-enable-cron?
                (default #t)
                (description "Whether to enable periodic sync via mcron.")))

(define (home-mbsync-files config)
  "Return alist of mbsync configuration files to deploy."
  `((".mbsyncrc" ,(home-mbsync-config-file config))))

(define (home-mbsync-mcron-jobs config)
  "Return list of mcron jobs for mbsync."
  (if (home-mbsync-enable-cron? config)
      (list #~(job next-minute-from
                   (lambda ()
                     (system* (string-append #$isync "/bin/mbsync") "--all"))))
      '()))

(define home-mbsync-service-type
  (service-type
   (name 'home-mbsync)
   (extensions
    (list (service-extension home-files-service-type
                             home-mbsync-files)
          (service-extension home-mcron-service-type
                             home-mbsync-mcron-jobs)))
   (default-value (home-mbsync-configuration))
   (description
    "Configure mbsync mail synchronization with periodic sync via mcron.")))
