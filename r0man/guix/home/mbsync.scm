(define-module (r0man guix home mbsync)
  #:use-module (gnu home services mcron)
  #:use-module (gnu home services)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:export (home-mbsync-services))

(define mbsync-config
  (mixed-text-file
   "mbsyncrc"
   "IMAPAccount burningswell\n"
   "Host imap.gmail.com\n"
   "User roman.scherer@burningswell.com\n"
   "PassCmd \"" (file-append password-store "/bin/pass") " Gmail/Burningswell\"\n"
   "SSLType IMAPS\n"
   "SSLVersions TLSv1.2\n"
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
   "SSLType IMAPS\n"
   "SSLVersions TLSv1.2\n"
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

(define jobs
  (list #~(job next-minute-from
               (lambda ()
                 (system* (string-append #$isync "/bin/mbsync") "--all")))))

(define home-mbsync-services
  (list (simple-service
         'mbsync-config home-files-service-type
         `((".mbsyncrc" ,mbsync-config)))
        (service home-mcron-service-type
                 (home-mcron-configuration (jobs jobs)))))
