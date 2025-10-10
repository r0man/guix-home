(define-module (r0man guix home sway)
  #:use-module (gnu home services)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu services)
  #:use-module (gnu system pam)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:export (home-sway-configuration
            home-sway-service-type))

;;; Commentary:
;;;
;;; Home service for Sway Wayland compositor configuration.
;;; Manages Sway config file and installs required packages.
;;;
;;; Code:

;; emacsclient --create-frame --no-wait --eval '(multi-term)'

(define background
  (local-file "files/wallpapers/stop-customizing-start-working.jpg"))

;; (define %sway-variables
;;   '(("CLUTTER_BACKEND" . "wayland")     ; GTK
;;     ("QT_QPA_PLATFORM" . "wayland")     ; Qt
;;     ("MOZ_ENABLE_WAYLAND" . "1")        ; IceCat, et.al.
;;     ;; These are normally provided by login managers(?).
;;     ("XDG_SESSION_TYPE" . "wayland")
;;     ("XDG_SESSION_DESKTOP" . "sway")
;;     ("XDG_CURRENT_DESKTOP" . "sway")))

(define swaylock-config
  (plain-file
   "swaylock-config"
   "color=000000
daemonize
show-failed-attempts
ignore-empty-password"))

(define swaylock-command
  #~(string-append "/run/setuid-programs/swaylock --config="
                   #$swaylock-config))

(define %sway-config
  (mixed-text-file
   "sway-config"
   "# Read `man 5 sway` for a complete reference.

### Variables
#
# Logo key. Use Mod1 for Alt, Mod4 for \"Windows key\".
set $mod Mod4
# Home row direction keys, like vim
set $left h
set $down j
set $up k
set $right l
# Your preferred terminal emulator
set $term " kitty "/bin/kitty
# Your preferred application launcher
# Note: pass the final command to swaymsg so that the resulting window can be opened
# on the original workspace that the command was run on.
#set $menu dmenu_path | dmenu | xargs swaymsg exec --
set $menu " wofi "/bin/wofi --show run

### Output configuration
#
# Default wallpaper (more resolutions are available in $(guix build sway)/share/backgrounds/sway/)
output * bg " background " fill
#
# Example configuration:
#
#   output HDMI-A-1 resolution 1920x1080 position 1920,0
#
# You can get the names of your outputs by running: swaymsg -t get_outputs

### Idle configuration
#
# Example configuration:
#
exec swayidle -w \
         timeout 720 '" swaylock-command "' \
         timeout 900 'swaymsg \"output * dpms off\"' \
              resume 'swaymsg \"output * dpms on\"' \
         before-sleep '" swaylock-command "'
#
# This will lock your screen after 720 seconds of inactivity, then turn off
# your displays after another 180 seconds, and turn your screens back on when
# resumed. It will also lock your screen before your computer goes to sleep.

### Input configuration
#
# Example configuration:
#
#   input \"2:14:SynPS/2_Synaptics_TouchPad\" {
#       dwt enabled
#       tap enabled
#       natural_scroll enabled
#       middle_emulation enabled
#   }
#
# You can get the names of your inputs by running: swaymsg -t get_inputs
# Read `man 5 sway-input` for more information about this section.

### Key bindings
#
# Basics:
#
    # Start a terminal
    bindsym $mod+Return exec $term

    # Kill focused window
    bindsym $mod+Shift+q kill

    # Start your launcher
    bindsym $mod+d exec $menu

    # Drag floating windows by holding down $mod and left mouse button.
    # Resize them with right mouse button + $mod.
    # Despite the name, also works for non-floating windows.
    # Change normal to inverse to use left mouse button for resizing and right
    # mouse button for dragging.
    floating_modifier $mod normal

    # Reload the configuration file
    bindsym $mod+Shift+c reload

    # Exit sway (logs you out of your Wayland session)
    bindsym $mod+Shift+e exec swaynag -t warning -m 'You pressed the exit shortcut. Do you really want to exit sway? This will end your Wayland session.' -b 'Yes, exit sway' 'swaymsg exit'
#
# Moving around:
#
    # Move your focus around
    bindsym $mod+$left focus left
    bindsym $mod+$down focus down
    bindsym $mod+$up focus up
    bindsym $mod+$right focus right
    # Or use $mod+[up|down|left|right]
    bindsym $mod+Left focus left
    bindsym $mod+Down focus down
    bindsym $mod+Up focus up
    bindsym $mod+Right focus right

    # Move the focused window with the same, but add Shift
    bindsym $mod+Shift+$left move left
    bindsym $mod+Shift+$down move down
    bindsym $mod+Shift+$up move up
    bindsym $mod+Shift+$right move right
    # Ditto, with arrow keys
    bindsym $mod+Shift+Left move left
    bindsym $mod+Shift+Down move down
    bindsym $mod+Shift+Up move up
    bindsym $mod+Shift+Right move right
#
# Workspaces:
#
    # Switch to workspace
    bindsym $mod+1 workspace 1
    bindsym $mod+2 workspace 2
    bindsym $mod+3 workspace 3
    bindsym $mod+4 workspace 4
    bindsym $mod+5 workspace 5
    bindsym $mod+6 workspace 6
    bindsym $mod+7 workspace 7
    bindsym $mod+8 workspace 8
    bindsym $mod+9 workspace 9
    bindsym $mod+0 workspace 10
    # Move focused container to workspace
    bindsym $mod+Shift+1 move container to workspace 1
    bindsym $mod+Shift+2 move container to workspace 2
    bindsym $mod+Shift+3 move container to workspace 3
    bindsym $mod+Shift+4 move container to workspace 4
    bindsym $mod+Shift+5 move container to workspace 5
    bindsym $mod+Shift+6 move container to workspace 6
    bindsym $mod+Shift+7 move container to workspace 7
    bindsym $mod+Shift+8 move container to workspace 8
    bindsym $mod+Shift+9 move container to workspace 9
    bindsym $mod+Shift+0 move container to workspace 10
    # Note: workspaces can have any name you want, not just numbers.
    # We just use 1-10 as the default.
#
# Layout stuff:
#
    # You can \"split\" the current object of your focus with
    # $mod+b or $mod+v, for horizontal and vertical splits
    # respectively.
    bindsym $mod+b splith
    bindsym $mod+v splitv

    # Switch the current container between different layout styles
    bindsym $mod+s layout stacking
    bindsym $mod+w layout tabbed
    bindsym $mod+e layout toggle split

    # Make the current focus fullscreen
    bindsym $mod+f fullscreen

    # Toggle the current focus between tiling and floating mode
    bindsym $mod+Shift+space floating toggle

    # Swap focus between the tiling area and the floating area
    bindsym $mod+space focus mode_toggle

    # Move focus to the parent container
    bindsym $mod+a focus parent
#
# Scratchpad:
#
    # Sway has a 'scratchpad', which is a bag of holding for windows.
    # You can send windows there and get them back later.

    # Move the currently focused window to the scratchpad
    bindsym $mod+Shift+minus move scratchpad

    # Show the next scratchpad window or hide the focused scratchpad window.
    # If there are multiple scratchpad windows, this command cycles through them.
    bindsym $mod+minus scratchpad show
#
# Resizing containers:
#
mode \"resize\" {
    # left will shrink the containers width
    # right will grow the containers width
    # up will shrink the containers height
    # down will grow the containers height
    bindsym $left resize shrink width 10px
    bindsym $down resize grow height 10px
    bindsym $up resize shrink height 10px
    bindsym $right resize grow width 10px

    # Ditto, with arrow keys
    bindsym Left resize shrink width 10px
    bindsym Down resize grow height 10px
    bindsym Up resize shrink height 10px
    bindsym Right resize grow width 10px

    # Return to default mode
    bindsym Return mode \"default\"
    bindsym Escape mode \"default\"
}
bindsym $mod+r mode \"resize\"



#
# Status Bar:
#
# Read `man 5 sway-bar` for more information about this section.
bar {
    position top

    # When the status_command prints a new line to stdout, swaybar updates.
    # The default just shows the current date and time.
    #status_command while date +'%Y-%m-%d %l:%M:%S %p'; do sleep 1; done
    status_command i3status

    colors {
        statusline #ffffff
        background #323232
        inactive_workspace #32323200 #32323200 #5c5c5c
    }
}

input \"type:keyboard\" {
    xkb_layout \"us,no\"
    xkb_options \"caps:ctrl_modifier,altwin:swap_lalt_lwin\"
}

#input <identifier> xkb_model \"pc101\"

# Map the key left of 1 to back_and_forth regardless of active layout.
bindsym $mod+grave workspace back_and_forth
bindsym $mod+bar workspace back_and_forth

bindsym XF86AudioRaiseVolume exec wpctl set-volume -l 1.5 @DEFAULT_AUDIO_SINK@ 5%+
bindsym XF86AudioLowerVolume exec wpctl set-volume @DEFAULT_AUDIO_SINK@ 5%-
bindsym XF86AudioMute exec wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle
bindsym XF86AudioMicMute exec wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle
bindsym XF86MonBrightnessDown exec brightnessctl set 5%-
bindsym XF86MonBrightnessUp exec brightnessctl set +5%

# No X11 here.
xwayland disable

# Ensure Pulseaudio is started.
exec --no-startup-id pactl stat

# GTK+ applications take 20 seconds to start
# See: https://github.com/swaywm/sway/wiki#gtk-applications-take-20-seconds-to-start
exec dbus-update-activation-environment --systemd WAYLAND_DISPLAY XDG_CURRENT_DESKTOP=sway

# Lock the screen at login.
#exec " swaylock-command))

(define-record-type* <home-sway-configuration>
  home-sway-configuration make-home-sway-configuration
  home-sway-configuration?
  (config-file home-sway-config-file
               (default %sway-config)
               (description "Sway configuration file."))
  (packages home-sway-packages
            (default (list brightnessctl
                           i3status
                           kitty
                           sway
                           swaybg
                           swayidle
                           swaylock
                           wofi))
            (description "List of Sway-related packages to install.")))

(define (home-sway-files config)
  "Return alist of Sway configuration files to deploy."
  `((".config/sway/config" ,(home-sway-config-file config))))

(define (home-sway-profile-packages config)
  "Return list of Sway packages to install."
  (home-sway-packages config))

(define home-sway-service-type
  (service-type
   (name 'home-sway)
   (extensions
    (list (service-extension home-files-service-type
                             home-sway-files)
          (service-extension home-profile-service-type
                             home-sway-profile-packages)))
   (default-value (home-sway-configuration))
   (description
    "Install and configure Sway Wayland compositor for the user.")))
