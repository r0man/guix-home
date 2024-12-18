(define-module (r0man guix home waybar)
  #:use-module (gnu home services)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages wm)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:export (home-waybar-services))

(define config
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
        \"bluetooth\",
        \"pulseaudio\",
        \"network\",
        \"battery\",
        \"custom/clock-icon\",
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
            \"3\": \" 3:Terminal\",
            \"8\": \" 8:Logs\",
            \"9\": \" 9:Monitor\",
        },
        \"all-outputs\": false,
        \"persistent-workspaces\": {
          \"*\": 6
        }
      },
      \"hyprland/window\": {
        \"max-length\": 128
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

(define style
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
    font-size: 14pt;
    min-height: 0;
}
window#waybar {
    opacity: 1;
    background: @background;
    color: @foreground;
}
#workspaces button {
    padding: 0 0.8em;
    color: @foreground;
}

#workspaces button:hover {
    box-shadow: inherit;
    text-shadow: inherit;
    background-image: linear-gradient(0deg, @selection, @background);
}
#workspaces button.active {
    color: @green;
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
    margin: 0 0.4em;
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

(define files
  `((".config/waybar/config.jsonc" ,config)
    (".config/waybar/style.css" ,style)))

(define packages
  (list font-awesome pavucontrol waybar))

(define home-waybar-services
  (list (simple-service 'waybar-config home-files-service-type files)
        (simple-service 'waybar-packages home-profile-service-type packages)))
