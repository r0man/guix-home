(define-module (r0man guix home git)
  #:use-module (gnu home services)
  #:use-module (gnu packages version-control)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 format)
  #:export (home-git-configuration
            home-git-service-type))

;;; Commentary:
;;;
;;; Home service for Git version control configuration.
;;; Manages ~/.gitconfig and installs Git packages.
;;;
;;; Code:

(define-record-type* <home-git-configuration>
  home-git-configuration make-home-git-configuration
  home-git-configuration?
  (user-name home-git-user-name
             (default "Roman Scherer")
             (description "Git user name."))
  (user-email home-git-user-email
              (default "roman@burningswell.com")
              (description "Git user email."))
  (signing-key home-git-signing-key
               (default "D226A339D8DF44815DDE0CA03DDA52527D2AC199")
               (description "GPG signing key ID."))
  (gpg-sign? home-git-gpg-sign?
             (default #t)
             (description "Enable GPG signing of commits."))
  (github-user home-git-github-user
               (default "r0man")
               (description "GitHub username."))
  (credential-helper home-git-credential-helper
                     (default "netrc -f ~/.authinfo.gpg -v")
                     (description "Git credential helper command."))
  (sendemail-from home-git-sendemail-from
                  (default "Roman Scherer <roman@burningswell.com>")
                  (description "Email address for git send-email."))
  (sendemail-smtp-server home-git-sendemail-smtp-server
                         (default "smtp.gmail.com")
                         (description "SMTP server for git send-email."))
  (sendemail-smtp-port home-git-sendemail-smtp-port
                       (default 587)
                       (description "SMTP port for git send-email."))
  (sendemail-smtp-user home-git-sendemail-smtp-user
                       (default "roman.scherer@burningswell.com")
                       (description "SMTP username for git send-email."))
  (sendemail-smtp-encryption home-git-sendemail-smtp-encryption
                             (default "tls")
                             (description "SMTP encryption for git send-email."))
  (packages home-git-packages
            (default (list git
                           git-crypt
                           (list git "credential-netrc")
                           (list git "send-email")))
            (description "List of Git-related packages to install.")))

(define (home-git-gitconfig config)
  "Generate .gitconfig file from CONFIG."
  (mixed-text-file
   "gitconfig"
   (string-join
    (list "[credential]"
          (format #f "  helper = \"~a\"" (home-git-credential-helper config))
          "[commit]"
          (format #f "  gpgsign = ~a"
                  (if (home-git-gpg-sign? config) "true" "false"))
          "[diff \"scheme\"]"
	  "  xfuncname = \"^(\\\\(define.*)$\""
          "[diff \"texinfo\"]"
	  "  xfuncname = \"^@node[[:space:]]+([^,]+).*$\""
          "[format]"
          "  thread = shallow"
          "[github]"
          (format #f "  user = ~a" (home-git-github-user config))
          "[rerere]"
          "  enabled = 1"
          "[sendemail]"
          (format #f "  from = ~a" (home-git-sendemail-from config))
          (format #f "  smtpencryption = ~a"
                  (home-git-sendemail-smtp-encryption config))
          (format #f "  smtpserver = ~a"
                  (home-git-sendemail-smtp-server config))
          (format #f "  smtpserverport = ~a"
                  (home-git-sendemail-smtp-port config))
          (format #f "  smtpuser = ~a"
                  (home-git-sendemail-smtp-user config))
          "  thread = no"
          "[user]"
          (format #f "  email = ~a" (home-git-user-email config))
          (format #f "  name = ~a" (home-git-user-name config))
          (format #f "  signingkey = ~a" (home-git-signing-key config)))
    "\n")))

(define (home-git-files config)
  "Return alist of Git configuration files to deploy."
  `((".gitconfig" ,(home-git-gitconfig config))))

(define (home-git-profile-packages config)
  "Return list of Git packages to install."
  (home-git-packages config))

(define home-git-service-type
  (service-type
   (name 'home-git)
   (extensions
    (list (service-extension home-files-service-type
                             home-git-files)
          (service-extension home-profile-service-type
                             home-git-profile-packages)))
   (default-value (home-git-configuration))
   (description
    "Install and configure Git version control system for the user.")))
