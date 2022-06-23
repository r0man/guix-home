(define-module (r0man profile emacs)
  #:use-module (flat packages emacs)
  #:use-module (gnu packages))

(specifications->manifest
 '("emacs-native-comp"))
