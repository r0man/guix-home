;; Development shell entry point for the r0man Guix Home channel.
;; Usage:
;;   guix shell -D -f guix.scm   # native dev shell
;;   guix shell -D -f guix.scm --pure --container
;;
;; Mirrors guix-channel's guix.scm.  nonguix is packaged as
;; `guile-nonguix` so `(use-modules (nongnu ...))` and
;; `(use-modules (nonguix ...))` resolve inside `guix shell --pure
;; --container`.
;;
;; Cross-channel dependency on asahi-guix (many home-environment
;; modules import (asahi guix ...)): point the dev shell at a local
;; checkout via R0MAN_EXTRA_LOAD_PATH, which `pre-inst-env` honours:
;;
;;   R0MAN_EXTRA_LOAD_PATH=/path/to/asahi-guix/channel/modules \
;;     ./pre-inst-env make -j$(nproc)
;;
;; Packaging the asahi-guix channel via guix.scm directly would
;; require pinning a content-addressed hash to a specific commit on
;; codeberg, which is brittle and not worth the complexity for a dev
;; shell that already assumes a working checkout.

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
  (let ((commit "c4541fdb0b472664dafe5d7b1ec2e51e4ef7b772")
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
          (base32 "1v50pvwr65hn8h09sc0hg9lz2vwzywagmyvkjbxs6v68gwy1gi3f"))))
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
           guile-next
           guile-nonguix
           guix
           pkg-config
           texinfo))
    (inputs (list guile-next))
    (synopsis "r0man Guix Home channel")
    (description "Personal Guix Home channel by r0man.")
    (home-page "https://github.com/r0man/guix-home")
    (license license:gpl3+)))

guile-r0man-home
