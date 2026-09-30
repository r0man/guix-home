;; Development shell entry point for the r0man Guix Home channel.
;; Usage:
;;   guix shell -D -f guix.scm   # native dev shell
;;   guix shell -D -f guix.scm --pure --container
;;
;; Mirrors guix-channel's guix.scm.  Cross-channel dependencies are
;; packaged as plain Guix packages so the matching `(use-modules ...)`
;; forms resolve inside `guix shell --pure --container`:
;;
;;   * `guile-nonguix`     -> (nonguix ...) and (nongnu ...)
;;   * `guile-asahi-guix`  -> (asahi guix ...) plus the asahi extensions
;;                            to (gnu ...) and (guix ...)
;;   * `guile-r0man-channel` -> (r0man guix packages ...) and friends
;;
;; A local checkout can still be substituted at build time via
;; R0MAN_EXTRA_LOAD_PATH, which `pre-inst-env` honours:
;;
;;   R0MAN_EXTRA_LOAD_PATH=/path/to/asahi-guix/channel/modules \
;;     ./pre-inst-env make -j$(nproc)

(use-modules
 ((guix licenses) #:prefix license:)
 (gnu packages autotools)
 (gnu packages gettext)
 (gnu packages guile)
 (gnu packages package-management)
 (gnu packages pkg-config)
 (gnu packages texinfo)
 (guix build-system copy)
 (guix build-system gnu)
 (guix gexp)
 (guix git-download)
 (guix packages)
 (guix utils)
 (srfi srfi-1))

(define-public guile-nonguix
  (let ((commit "f9171dd0d0a58d63c0811d61e51493a3fa4ae4f3")
        (revision "0"))
    (package
      (name "guile-nonguix")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.com/nonguix/nonguix")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1bhlabjifwjan4yimfd1m61izafqjkz4k7d7yma4cc6n6kyz3jiz"))))
      (build-system copy-build-system)
      (arguments
       `(#:install-plan
         '(("nonguix"     "share/guile/site/3.0/nonguix")
           ("nongnu"      "share/guile/site/3.0/nongnu")
           ("nonguix.scm" "share/guile/site/3.0/nonguix.scm"))))
      (synopsis "nonguix channel modules as a Guix package")
      (description "Provides the @code{(nonguix …)} and @code{(nongnu …)}
Guile modules from the nonguix channel, packaged for use as a dev-shell
input where the channel mechanism isn't available — for example inside
@code{guix shell --container --pure}.")
      (home-page "https://gitlab.com/nonguix/nonguix")
      (license license:gpl3+))))

(define-public guile-asahi-guix
  (let ((commit "ee2e7465c0137224bcc0ad3b3d0d02109cb3a6c7")
        (revision "0"))
    (package
      (name "guile-asahi-guix")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://codeberg.org/asahi-guix/channel")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "07m30nmy7s36w6phrf10zy8wn9dzynbk3ziqd4hxjx64ys63lgic"))))
      (build-system copy-build-system)
      (arguments
       `(#:install-plan
         '(("modules/asahi" "share/guile/site/3.0/asahi")
           ("modules/gnu"   "share/guile/site/3.0/gnu")
           ("modules/guix"  "share/guile/site/3.0/guix"))))
      (synopsis "asahi-guix channel modules as a Guix package")
      (description "Provides the @code{(asahi guix …)} Guile modules
plus the channel's extensions to @code{(gnu …)} and @code{(guix …)},
packaged for use as a dev-shell input where the channel mechanism
isn't available — for example inside @code{guix shell --container
--pure}.")
      (home-page "https://codeberg.org/asahi-guix/channel")
      (license license:gpl3+))))

(define-public guile-r0man-channel
  (let ((commit "86640e3a324b4b39619a5d833931e6980251b01b")
        (revision "0"))
    (package
      (name "guile-r0man-channel")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/r0man/guix-channel")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "038aarg23jygkwwpl6cnsaqxvkhp4gya5hnnbixfsvissgr80vii"))))
      (build-system copy-build-system)
      (arguments
       `(#:install-plan
         '(("modules/r0man" "share/guile/site/3.0/r0man"))))
      (synopsis "r0man Guix channel modules as a Guix package")
      (description "Provides the @code{(r0man guix …)} Guile modules
from r0man's personal Guix channel, packaged for use as a dev-shell
input where the channel mechanism isn't available.")
      (home-page "https://github.com/r0man/guix-channel")
      (license license:gpl3+))))

(define-public guile-r0man-home
  (package
    (name "guile-r0man-home")
    (version "0.1")
    (source
     (local-file
      (dirname (current-filename))
      #:recursive? #t
      #:select?
      (λ (file stat)
        (not (any (lambda (s) (string-contains file s))
                  (list ".git" ".dir-locals.el" "guix.scm"))))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf
           automake
           gnu-gettext
           guile-asahi-guix
           guile-next
           guile-nonguix
           guile-r0man-channel
           guix
           pkg-config
           texinfo))
    (inputs (list guile-next))
    (synopsis "r0man Guix Home channel")
    (description "Personal Guix Home channel by r0man.")
    (home-page "https://github.com/r0man/guix-home")
    (license license:gpl3+)))

guile-r0man-home
