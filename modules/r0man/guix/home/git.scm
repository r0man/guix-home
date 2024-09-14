(define-module (r0man guix home git)
  #:use-module (gnu home services)
  #:use-module (gnu packages version-control)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:export (home-git-services))

(define git-config
  (mixed-text-file
   "gitconfig"
   (string-join
    (list "[credential]"
          "  helper = \"netrc -f ~/.authinfo.gpg -v\""
          "[commit]"
          "  gpgsign = true"
          "[diff \"scheme\"]"
	  "  xfuncname = \"^(\\\\(define.*)$\""
          "[diff \"texinfo\"]"
	  "  xfuncname = \"^@node[[:space:]]+([^,]+).*$\""
          "[format]"
          "  thread = shallow"
          "[github]"
          "  user = r0man"
          "[rerere]"
          "  enabled = 1"
          "[sendemail]"
          "  from = Roman Scherer <roman@burningswell.com>"
          "  smtpencryption = tls"
          "  smtpserver = smtp.gmail.com"
          "  smtpserverport = 587"
          "  smtpuser = roman.scherer@burningswell.com"
          "  thread = no"
          "[user]"
          "  email = roman@burningswell.com"
          "  name = Roman Scherer"
          "  signingkey = D226A339D8DF44815DDE0CA03DDA52527D2AC199")
    "\n")))

(define files
  `((".gitconfig" ,git-config)))

(define packages
  (list git
        git-crypt
        (list git "credential-netrc")
        (list git "send-email")))

(define home-git-services
  (list (simple-service 'git-config home-files-service-type files)
        (simple-service 'git-packages home-profile-service-type packages)))
