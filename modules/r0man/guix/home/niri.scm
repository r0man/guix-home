(define-module (r0man guix home niri)
  #:use-module (gnu home services)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (home-niri-configuration
            home-niri-service-type))

;;; Commentary:
;;;
;;; Home service for Niri scrollable-tiling Wayland compositor.
;;; Manages Niri config, Niri-specific Waybar config/style, and
;;; installs required packages.
;;;
;;; Waybar config is deployed to ~/.config/niri/ (not ~/.config/waybar/)
;;; to avoid conflicts with the existing Hyprland-specific waybar setup.
;;;
;;; Code:

(define %niri-waybar-config
  (mixed-text-file
   "niri-waybar-config.jsonc"
   "{
      \"layer\": \"top\",
      \"position\": \"top\",
      \"spacing\": 0,
      \"modules-left\": [
        \"niri/workspaces\",
        \"niri/window\"
      ],
      \"modules-right\": [
        \"cpu\",
        \"memory\",
        \"battery\",
        \"bluetooth\",
        \"pulseaudio\",
        \"network\",
        \"clock#saopaulo\",
        \"custom/separator\",
        \"clock#utc\",
        \"custom/separator\",
        \"clock#berlin\",
        \"custom/separator\",
        \"clock#date\"
      ],
      \"niri/workspaces\": {
        \"format\": \"{icon}\",
        \"format-icons\": {
            \"1:Emacs\": \" 1:Emacs\",
            \"2:Web\": \" 2:Web\",
            \"3:Slack\": \" 3:Slack\",
            \"0:Logs\": \" 0:Logs\",
            \"9:Top\": \" 9:Top\"
        }
      },
      \"niri/window\": {
        \"max-length\": 128
      },
      \"battery\": {
          \"states\": {
              \"warning\": 30,
              \"critical\": 15
          },
          \"format\": \"{icon} {capacity}%\",
          \"format-charging\": \"\\ud83d\\uddf2 {capacity}%\",
          \"format-plugged\": \" {capacity}%\",
          \"format-alt\": \"{icon} {time}\",
          \"format-icons\": [\"\", \"\", \"\", \"\", \"\"]
      },
      \"custom/separator\": {
        \"format\": \"|\"
      },
      \"cpu\": {
          \"format\": \" {usage}%\",
          \"tooltip\": false
      },
      \"memory\": {
          \"format\": \" {}%\"
      },
      \"clock#saopaulo\": {
        \"format\": \"S\\u00c3O PAULO {:%H:%M}\",
        \"timezone\": \"Brazil/East\"
      },
      \"clock#utc\": {
        \"format\": \"UTC {:%H:%M}\",
        \"timezone\": \"UTC\",
        \"tooltip\": false
      },
      \"clock#berlin\": {
        \"format\": \"BERLIN {:%H:%M:%S}\",
        \"interval\": 1,
        \"timezone\": \"Europe/Berlin\",
        \"tooltip\": false
      },
      \"clock#date\": {
        \"format\": \"{:%a, %d %b}\",
        \"timezone\": \"Europe/Berlin\",
        \"tooltip\": false
      },
      \"network\": {
          \"format-wifi\": \" {essid}\",
          \"format-ethernet\": \"{ipaddr}/{cidr}\",
          \"tooltip-format\": \"{ifname} via {gwaddr}\",
          \"format-linked\": \"{ifname} (No IP)\",
          \"format-disconnected\": \"Disconnected \\u26a0\",
          \"format-alt\": \"{ifname}: {ipaddr}/{cidr}\"
      },
      \"pulseaudio\": {
          \"format\": \"%{icon} {volume} {format_source}\",
          \"format-bluetooth\": \"{icon} {volume}% {format_source}\",
          \"format-bluetooth-muted\": \"{icon} {format_source}\",
          \"format-muted\": \" {format_source}\",
          \"format-source\": \" {volume}%\",
          \"format-source-muted\": \"\",
          \"format-icons\": {
              \"headphone\": \"\",
              \"hands-free\": \"\",
              \"headset\": \"\",
              \"phone\": \"\",
              \"portable\": \"\",
              \"car\": \"\",
              \"default\": [\"\", \"\", \"\"]
          },
          \"on-click\": \"pavucontrol\"
      }}"))

(define %niri-waybar-style
  (mixed-text-file
   "niri-waybar-style.css"
   "
@define-color background-darker rgba(30, 31, 41, 230);
@define-color background #000000;
@define-color selection #44475a;
@define-color foreground #f8f8f2;
@define-color comment #6272a4;
@define-color cyan #8be9fd;
@define-color grey #888888;
@define-color green #50fa7b;
@define-color orange #ffb86c;
@define-color pink #ff79c6;
@define-color purple #bd93f9;
@define-color red #ff5555;
@define-color yellow #f1fa8c;

* {
    border: none;
    border-radius: 0;
    font-family: Hack;
    font-size: 11pt;
    min-height: 0;
}
window#waybar {
    opacity: 1;
    background: @background;
    color: @foreground;
}
#workspaces button {
    padding: 0 0.8em;
    color: @grey;
}

#workspaces button:hover {
    box-shadow: inherit;
    text-shadow: inherit;
    background-image: linear-gradient(0deg, @selection, @background);
}
#workspaces button.active {
    color: @white;
}
#workspaces button.urgent {
    color: @red;
}
#workspaces button.empty {
    color: @selection;
}

.module {
    padding: 0 0em;
    margin: 1 0.4em;
    color: @foreground;
}

#clock.saopaulo, #clock.utc {
  color: @grey;
}

#pulseaudio.bluetooth { color: @purple; }
#bluetooth.connected { color: @cyan; }
#battery.warning { color: @yellow; }
#battery.critical { color: @red; }
"))

(define-record-type* <home-niri-configuration>
  home-niri-configuration make-home-niri-configuration
  home-niri-configuration?
  (config-file home-niri-config-file
               (default (local-file "files/niri/config.kdl"))
               (description "Niri configuration file."))
  (waybar-config home-niri-waybar-config
                 (default %niri-waybar-config)
                 (description "Waybar config for Niri."))
  (waybar-style home-niri-waybar-style
                (default %niri-waybar-style)
                (description "Waybar style CSS for Niri."))
  (packages home-niri-packages
            (default (list brightnessctl
                           niri
                           swaybg
                           swayidle
                           swaylock
                           waybar
                           wireplumber))
            (description "List of Niri-related packages to install.")))

(define (home-niri-files config)
  "Return alist of Niri configuration files to deploy."
  `((".config/niri/config.kdl" ,(home-niri-config-file config))
    (".config/niri/waybar-config.jsonc" ,(home-niri-waybar-config config))
    (".config/niri/waybar-style.css" ,(home-niri-waybar-style config))))

(define (home-niri-profile-packages config)
  "Return list of Niri packages to install."
  (home-niri-packages config))

(define home-niri-service-type
  (service-type
   (name 'home-niri)
   (extensions
    (list (service-extension home-files-service-type
                             home-niri-files)
          (service-extension home-profile-service-type
                             home-niri-profile-packages)))
   (default-value (home-niri-configuration))
   (description
    "Install and configure Niri scrollable-tiling Wayland compositor.")))
