(define-module (r0man guix system xorg)
  #:export (%xorg-libinput-config
            %xorg-modeset-config))

(define %xorg-libinput-config "
Section \"InputClass\"
  Identifier \"Touchpads\"
  Driver \"libinput\"
  MatchDevicePath \"/dev/input/event*\"
  MatchIsTouchpad \"on\"
  Option \"Tapping\" \"on\"
  Option \"TappingDrag\" \"on\"
  Option \"DisableWhileTyping\" \"on\"
  Option \"MiddleEmulation\" \"on\"
  Option \"ScrollMethod\" \"twofinger\"
EndSection

Section \"InputClass\"
  Identifier \"Keyboards\"
  Driver \"libinput\"
  MatchDevicePath \"/dev/input/event*\"
  MatchIsKeyboard \"on\"
EndSection
")

(define %xorg-modeset-config "
Section \"OutputClass\"
    Identifier \"appledrm\"
    MatchDriver \"apple\"
    Driver \"modesetting\"
    Option \"PrimaryGPU\" \"true\"
EndSection
")
