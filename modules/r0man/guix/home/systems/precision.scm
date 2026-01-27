(define-module (r0man guix home systems precision)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services gnupg)
  #:use-module (gnu home services guix)
  #:use-module (gnu home services mail)
  #:use-module (gnu home services pm)
  #:use-module (gnu home services shells)
  #:use-module (gnu home services sound)
  #:use-module (gnu home services ssh)
  #:use-module (gnu home services xdg)
  #:use-module (gnu home services)
  #:use-module (gnu home)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages ibus)
  #:use-module (gnu packages speech)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages xorg)
  #:use-module (gnu services xorg)
  #:use-module (gnu services)
  #:use-module (nongnu packages nvidia)
  #:use-module (r0man guix home bash)
  #:use-module (r0man guix home btop)
  #:use-module (r0man guix home channels)
  #:use-module (r0man guix home claude-code)
  #:use-module (r0man guix home clojure)
  #:use-module (r0man guix home container-home)
  #:use-module (r0man guix home common-lisp)
  #:use-module (r0man guix home eca)
  #:use-module (r0man guix home emacs)
  #:use-module (r0man guix home environment)
  #:use-module (r0man guix home fzf)
  #:use-module (r0man guix home git)
  #:use-module (r0man guix home gpg)
  #:use-module (r0man guix home guile)
  #:use-module (r0man guix home hyprland)
  #:use-module (r0man guix home i3status)
  #:use-module (r0man guix home ibus)
  #:use-module (r0man guix home kitty)
  #:use-module (r0man guix home librewolf)
  #:use-module (r0man guix home mbsync)
  #:use-module (r0man guix home msmtp)
  #:use-module (r0man guix home nix)
  #:use-module (r0man guix home packages)
  #:use-module (r0man guix home pm)
  #:use-module (r0man guix home rofi)
  #:use-module (r0man guix home ssh)
  #:use-module (r0man guix home stumpwm)
  #:use-module (r0man guix home sway)
  #:use-module (r0man guix home tmux)
  #:use-module (r0man guix home waybar)
  #:use-module (r0man guix home wofi)
  #:use-module (r0man guix home x11)
  #:use-module (r0man guix home xdg)
  #:use-module (r0man guix system keyboard)
  #:use-module (r0man guix system xorg))

;;; Commentary:
;;;
;;; Home environment configuration for Dell Precision laptop (x86-64).
;;;
;;; Hardware:
;;;   - Intel Alder Lake-P (Iris Xe Graphics) - PCI:0:2:0
;;;   - NVIDIA RTX A1000 Laptop GPU (GA107GLM) - PCI:1:0:0
;;;
;;; Hybrid Graphics (PRIME Render Offload):
;;;   This laptop has hybrid graphics where displays are physically
;;;   connected to the Intel GPU, not NVIDIA. X11 configuration uses
;;;   Intel's modesetting driver for display output and makes NVIDIA
;;;   available for 3D rendering via PRIME render offload.
;;;
;;; Verification:
;;;   Default renderer (Intel):
;;;     glxinfo | grep "OpenGL renderer"
;;;     => Mesa Intel(R) Iris(R) Xe Graphics
;;;
;;;   NVIDIA offload:
;;;     __NV_PRIME_RENDER_OFFLOAD=1 \
;;;     __GLX_VENDOR_LIBRARY_NAME=nvidia \
;;;     glxinfo | grep "OpenGL renderer"
;;;     => NVIDIA RTX A1000 Laptop GPU/PCIe/SSE2
;;;
;;; Usage:
;;;   Run applications with NVIDIA rendering by prefixing commands:
;;;     __NV_PRIME_RENDER_OFFLOAD=1 \
;;;     __GLX_VENDOR_LIBRARY_NAME=nvidia \
;;;     <application>
;;;
;;; Code:

(define services
  (append home-tmux-services
          (list (service home-bash-service-type
                         home-bash-default-configuration)
                (service home-batsignal-service-type
                         home-batsignal-default-configuration)
                (service home-btop-service-type)
        (service home-channels-service-type
                 home-channels-default-list)
        (service home-claude-code-service-type)
        (service home-clojure-service-type)
        (service home-container-home-service-type)
        (service home-common-lisp-service-type)
        (service home-dbus-service-type)
        (service home-eca-service-type)
        (service home-emacs-service-type)
        (service home-environment-service-type)
        (service home-fzf-service-type)
        (service home-git-service-type)
        (service home-gpg-agent-service-type
                 home-gpg-gtk-configuration)
        (service home-guile-service-type)
        (service home-hyprland-service-type)
        (service home-ibus-service-type
                 (home-ibus-configuration
                  (packages (map replace-mesa
                                 (list ibus
                                       ibus-speech-to-text-whisper
                                       gst-plugins-good
                                       gst-vosk)))))
        (service home-i3status-service-type)
        (service home-kitty-service-type)
        (service home-librewolf-service-type)
        (service home-mbsync-service-type)
        (service home-msmtp-service-type
                 home-msmtp-default-configuration)
        (service home-nix-service-type)
        (service home-openssh-service-type
                 home-openssh-default-configuration)
        (service home-pipewire-service-type)
        (service home-rofi-service-type)
        (service home-ssh-agent-service-type)
        (service home-startx-command-service-type
                 (xorg-configuration
                  ;; NVIDIA driver version must match nonguix package
                  ;; https://download.nvidia.com/XFree86/Linux-x86_64/580.126.09
                  (drivers '("modesetting" "nvidia"))
                  (extra-config
                   (list %xorg-libinput-config
                         "Section \"ServerLayout\"
    Identifier     \"layout\"
    Screen      0  \"intel\"
    Inactive       \"nvidia\"
EndSection

Section \"Device\"
    Identifier     \"intel\"
    Driver         \"modesetting\"
    BusID          \"PCI:0:2:0\"
EndSection

Section \"Screen\"
    Identifier     \"intel\"
    Device         \"intel\"
EndSection

Section \"Device\"
    Identifier     \"nvidia\"
    Driver         \"nvidia\"
    BusID          \"PCI:1:0:0\"
EndSection

Section \"Screen\"
    Identifier     \"nvidia\"
    Device         \"nvidia\"
EndSection"))
                  (keyboard-layout %keyboard-layout)
                  (modules (cons nvda %default-xorg-modules))
                  (server (replace-mesa xorg-server))))
        (service home-stumpwm-service-type)
        (service home-sway-service-type)
        (service home-unclutter-service-type)
        (service home-waybar-service-type)
        (service home-wofi-service-type)
        (service home-x11-custom-service-type)
        (service home-x11-service-type)
        (service home-xdg-mime-applications-service-type
                 home-xdg-mime-applications-default-configuration))))

(define-public precision-home-environment
  (home-environment
   (packages (map replace-mesa
                  (append packages-base
                          packages-desktop
                          packages-x86-64
                          (list vulkan-tools))))
   (services services)))

precision-home-environment
