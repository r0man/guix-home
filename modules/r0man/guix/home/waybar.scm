(define-module (r0man guix home waybar)
  #:use-module (gnu home services)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages wm)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix records)
  #:export (home-waybar-configuration
            home-waybar-service-type))

;;; Commentary:
;;;
;;; Home service for Waybar status bar configuration.
;;; Manages ~/.config/waybar/ configuration and style files for
;;; Wayland compositors (Hyprland, Sway, etc.).
;;;
;;; Code:

(define %waybar-config
  (mixed-text-file
   "waybar-config.jsonc"
   "{
      \"layer\": \"top\",
      \"position\": \"top\",
      \"spacing\": 0,
      \"modules-left\": [
        \"hyprland/workspaces\",
        \"hyprland/window\",
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
        \"clock#date\",
      ],
      \"hyprland/workspaces\": {
        \"format\": \"{icon}\",
        \"format-icons\": {
            \"1\": \" 1:Emacs\",
            \"2\": \" 2:Web\",
            \"3\": \" 3:Term\",
            \"4\": \"4\",
            \"5\": \"5\",
            \"6\": \"6\",
            \"7\": \"7\",
            \"8\": \" 8:Log\",
            \"9\": \" 9:Top\"
        }
      },
      \"hyprland/window\": {
        \"max-length\": 128
      },
      \"battery\": {
          \"states\": {
              \"warning\": 30,
              \"critical\": 15
          },
          \"format\": \"{icon} {capacity}%\",
          \"format-charging\": \"🗲 {capacity}%\",
          \"format-plugged\": \" {capacity}%\",
          \"format-alt\": \"{icon} {time}\",
          \"format-icons\": [\"\", \"\", \"\", \"\", \"\"]
      },
      \"custom/separator\": {
        \"format\": \"|\"
      },
      \"cpu\": {
          \"format\": \" {usage}%\",
          \"tooltip\": false
      },
      \"memory\": {
          \"format\": \" {}%\"
      },
      \"clock#saopaulo\": {
        \"format\": \"SÃO PAULO: {:%H:%M:%S}\",
        \"interval\": 1,
        \"timezone\": \"Brazil/East\"
      },
      \"clock#utc\": {
        \"format\": \"UTC: {:%H:%M:%S}\",
        \"interval\": 1,
        \"timezone\": \"UTC\",
        \"tooltip\": false
      },
      \"clock#berlin\": {
        \"format\": \"BERLIN: {:%H:%M:%S}\",
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
          \"format-wifi\": \" {essid}\",
          \"format-ethernet\": \"{ipaddr}/{cidr}\",
          \"tooltip-format\": \"{ifname} via {gwaddr}\",
          \"format-linked\": \"{ifname} (No IP)\",
          \"format-disconnected\": \"Disconnected ⚠\",
          \"format-alt\": \"{ifname}: {ipaddr}/{cidr}\"
      },
      \"pulseaudio\": {
          \"format\": \"%{icon} {volume} {format_source}\",
          \"format-bluetooth\": \"{icon} {volume}% {format_source}\",
          \"format-bluetooth-muted\": \"{icon} {format_source}\",
          \"format-muted\": \" {format_source}\",
          \"format-source\": \" {volume}%\",
          \"format-source-muted\": \"\",
          \"format-icons\": {
              \"headphone\": \"\",
              \"hands-free\": \"\",
              \"headset\": \"\",
              \"phone\": \"\",
              \"portable\": \"\",
              \"car\": \"\",
              \"default\": [\"\", \"\", \"\"]
          },
          \"on-click\": \"pavucontrol\"
      }}"))

(define %waybar-style
  (mixed-text-file
   "waybar-style.css"
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
#workspaces button.special {
    color: @yellow;
}

.module {
    padding: 0 0em;
    margin: 1 0.4em;
    color: @foreground;
}

#clock.saopaulo, #clock.utc {
  color: @grey;
}

#custom-drive.on { color: @green; }
#custom-drive.off { color: @white; }
#custom-vpn.off { color: @white; }
#custom-vpn.on { color: @pink; }

#pulseaudio.bluetooth { color: @purple; }
#bluetooth.connected { color: @cyan; }
#battery.warning { color: @yellow; }
#battery.critical { color: @red; }
#custom-system { margin: 0 1em; }"
   ))

(define-record-type* <home-waybar-configuration>
  home-waybar-configuration make-home-waybar-configuration
  home-waybar-configuration?
  (config-file home-waybar-config-file
               (default %waybar-config)
               (description "Path to waybar config.jsonc file."))
  (style-file home-waybar-style-file
              (default %waybar-style)
              (description "Path to waybar style.css file."))
  (packages home-waybar-packages
            (default (list font-awesome pavucontrol waybar))
            (description "List of waybar-related packages to install.")))

(define (home-waybar-files config)
  "Return alist of waybar configuration files to deploy."
  `((".config/waybar/config.jsonc" ,(home-waybar-config-file config))
    (".config/waybar/style.css" ,(home-waybar-style-file config))))

(define (home-waybar-profile-packages config)
  "Return list of waybar packages to install."
  (home-waybar-packages config))

(define home-waybar-service-type
  (service-type
   (name 'home-waybar)
   (extensions
    (list (service-extension home-files-service-type
                             home-waybar-files)
          (service-extension home-profile-service-type
                             home-waybar-profile-packages)))
   (default-value (home-waybar-configuration))
   (description
    "Install and configure Waybar status bar for Wayland compositors.")))
