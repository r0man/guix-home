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
   "
{
  \"layer\": \"top\",
  \"position\": \"top\",
  \"height\": 32,
  \"spacing\": 6,
  \"modules-left\": [
    \"hyprland/workspaces\",
    \"custom/space\",
    \"custom/space\",
    \"custom/space\",
    \"custom/github\",
    \"custom/vpn\",
    \"custom/drive\",
  ],
  \"modules-center\": [
    \"clock\",
  ],
  \"modules-right\": [
    \"custom/space\",
    \"cpu\",
    \"memory\",
    \"custom/space\",
    \"bluetooth\",
    \"pulseaudio\",
    \"backlight\",
    \"network\",
    \"battery\",
    \"custom/system\",
  ],
  \"hyprland/workspaces\": {
    \"format\": \"{icon}\",
    \"format-icons\": {
        \"1\": \"󰞷\",
        \"2\": \"󰗀\",
        \"3\": \"󰊯\",
        \"4\": \"󰭻\",
        \"5\": \"\",
        \"6\": \"󰋩\",
    },
    \"all-outputs\": false,
    \"persistent-workspaces\": {
      \"*\": 6
    }
  },
  \"wlr/workspaces\": {
    \"on-click\": \"activate\",
    \"on-scroll-up\": \"hyprctl dispatch workspace e-1\",
    \"on-scroll-down\": \"hyprctl dispatch workspace e+1\"
  },
  \"hyprland/window\": {
    \"max-length\": 128
  },
  \"clock\": {
    \"format\": \"{:%c}\",
    \"tooltip-format\": \"<big>{:%Y %B}</big>\n<tt><small>{calendar}</small></tt>\"
  },
  \"tray\": {
    \"spacing\": 4
  },
  \"custom/weather\": {
    \"exec\": \"~/.config/waybar/wittr.sh\",
    \"return-type\": \"json\",
    \"format\": \"{}\",
    \"tooltip\": true,
    \"interval\": 900
  },
  \"hyprland/language\": {
    \"format-pl\": \"[pl]\",
    \"format-en\": \"[us]\",
    \"on-click\": \"hyprctl switchxkblayout at-translated-set-2-keyboard next\"
  },
  \"custom/space\": {
    \"format\": \"    \"
  },
  \"custom/github\": {
    \"format\": \" {}\",
    \"interval\": 30,
    \"exec\": \"gh api notifications | jq -r '. | length'\",
  },
  \"custom/vpn\": {
    \"format\": \"󰯄\",
    \"return-type\": \"json\",
    \"interval\": 3,
    \"exec\": \"~/.config/waybar/vpn.sh\",
    \"on-click\": \"exec ~/.config/waybar/vpn.sh toggle\",
  },
  \"custom/drive\": {
    \"format\": \"󰊶\",
    \"return-type\": \"json\",
    \"interval\": 5,
    \"exec\": \"~/.config/waybar/drive.sh\",
    \"on-click\": \"exec ~/.config/waybar/drive.sh toggle\",
  },
  \"clock\": {
    \"format\": \"{:%H:%M}\",
    \"tooltip-format\": \"<big>{:%d/%m - %H:%M}</big>\n<tt><small>{calendar}</small></tt>\"
  },
  \"cpu\": {
    \"interval\": 3,
    \"format\": \"{icon} {usage}\",
    \"format-icons\": [\"󰻠\"],
  },
  \"memory\": {
    \"interval\": 3,
    \"format\": \"{icon} {percentage}\",
    \"format-icons\": [\"󰘚\"]
  },
  \"bluetooth\": {
    \"format\": \"󰂯\",
    \"format-disabled\": \"󰂲\",
    \"format-off\": \"󰂲\",
    \"format-connected\": \"󰂯 {num_connections}\",
    \"on-click\": \"blueman-manager\",
    \"on-click-right\": \"exec ~/.config/waybar/toggle-bluetooth.sh\",
  },
  \"pulseaudio\": {
    \"format\": \"{icon} {volume}\",
    \"format-muted\": \"󰝟 {volume}\",
    \"format-icons\": {
      \"headphone\": \"\",
      \"headset\": \"\",
      \"phone\": \"\",
      \"phone-muted\": \"\",
      \"portable\": \"\",
      \"car\": \"\",
      \"default\": [ \"󰕿\", \"󰖀\", \"󰕾\" ],
    },
    \"max-volume\": 150,
    \"scroll-step\": 0.5,
    \"on-click\": \"pactl set-sink-mute @DEFAULT_SINK@ toggle\",
    \"on-click-right\": \"pavucontrol\",
  },
  \"backlight\": {
    \"format\": \"󰛨 {percent}\",
    \"scroll-step\": 10,
  },
  \"network\": {
    \"interval\": 3,
    \"format-wifi\": \"{icon} {signalStrength}\",
    \"format-ethernet\": \"󰙅 {ifname}\",
    \"format-disconnected\": \"󰤣 {signalStrength}\",
    \"format-disabled\": \"󰤮 --\",
    \"format-icons\": {
      \"wifi\": [ \"󰤟\", \"󰤢\", \"󰤥\", \"󰤨\", ],
    },
    \"on-click\": \"networkmanager_dmenu\",
  },
  \"battery\": {
    \"interval\": 3,
    \"format\": \"{icon} {capacity}%\",
    \"format-charging\": \"󰂄 {capacity}%\",
    \"states\": {
        \"warning\": 20,
        \"critical\": 10,
    },
    \"format-icons\": [\"󰂎\", \"󰁺\", \"󰁻\", \"󰁼\", \"󰁽\", \"󰁾\", \"󰁿\", \"󰂀\", \"󰂁\", \"󰂂\", \"󰁹\"]
  },
  \"custom/system\": {
    \"format\": \"󰐦\",
    \"on-click\": \"systemctl hibernate\",
  }
}"
   ))

(define style
  (mixed-text-file
   "waybar-style.css"
   "
@define-color background-darker rgba(30, 31, 41, 230);
@define-color background #282a36;
@define-color selection #44475a;
@define-color foreground #f8f8f2;
@define-color comment #6272a4;
@define-color cyan #8be9fd;
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
    border-top: 0.1em solid @background;
}

#workspaces button:hover {
    box-shadow: inherit;
    text-shadow: inherit;
    background-image: linear-gradient(0deg, @selection, @background);
}
#workspaces button.active {
    color: @green;
    border-top-color: @green;
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
    border-top: 0.1em solid @background;
}

#custom-drive.on { color: @green; }
#custom-drive.off { color: @white; }
#custom-vpn.off { color: @white; }
#custom-vpn.on { color: @pink; }

#cpu { border-top-color: @white; }
#memory { border-top-color: @white; }
#network { border-top-color: @yellow; }
#network.disconnected { border-top-color: @yellow; }
#network.disabled { border-top-color: @red; }
#backlight { border-top-color: @green; }
#pulseaudio { border-top-color: @purple; }
#pulseaudio.muted { border-top-color: @red; }
#pulseaudio.bluetooth { color: @purple; }
#bluetooth { border-top-color: @cyan; }
#bluetooth.connected { color: @cyan; }
#battery { border-top-color: @red; }
#battery.warning { color: @yellow; }
#battery.critical { color: @red; }
#custom-system { margin: 0 1em; }"
   ))

(define files
  `((".config/waybar/config.jsonc" ,config)
    (".config/waybar/style.css" ,style)))

(define packages
  (list font-awesome waybar))

(define home-waybar-services
  (list (simple-service 'waybar-config home-files-service-type files)
        (simple-service 'waybar-packages home-profile-service-type packages)))
