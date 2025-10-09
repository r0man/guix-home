(define-module (r0man guix home i3status)
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
  #:export (home-i3status-configuration
            home-i3status-service-type))

;;; Commentary:
;;;
;;; Home service for i3status status bar configuration.
;;; Manages ~/.config/i3status/config and installs i3status package.
;;;
;;; Code:

(define %i3status-config
  (mixed-text-file
   "i3status-config"
   "general {
        output_format = \"none\"
        colors = true
        interval = 1
}

order += \"load\"
order += \"memory\"
order += \"battery 0\"
order += \"wireless wlan0\"
order += \"tztime sao_paulo\"
order += \"tztime utc\"
order += \"tztime berlin\"
order += \"tztime date\"

wireless wlan0 {
        format_up = \"W: (%quality at %essid, %bitrate) %ip\"
        format_down = \"W: down\"
}

ethernet eth0 {
        format_up = \"E: %ip (%speed)\"
        format_down = \"E: down\"
}

battery 0 {
        format = \"%status %percentage %remaining %emptytime\"
        format_down = \"No battery\"
        status_chr = \"⚡ CHR\"
        status_bat = \"🔋 BAT\"
        status_unk = \"? UNK\"
        status_full = \"☻ FULL\"
        path = \"/sys/class/power_supply/BAT%d/uevent\"
        low_threshold = 10
}

tztime utc {
        format = \"UTC: %H:%M:%S\"
        hide_if_equals_localtime = true
        timezone = \"UTC\"
}

tztime berlin {
        format = \"BERLIN: %H:%M:%S\"
        timezone = \"Europe/Berlin\"
}

tztime sao_paulo {
        format = \"SÃO PAULO: %H:%M:%S\"
        timezone = \"America/Sao_Paulo\"
}

tztime date {
        format = \"%a, %d %b\"
        timezone = \"UTC\"
}

load {
        format = \"%5min\"
}

cpu_temperature 0 {
        format = \"T: %degrees °C\"
        path = \"/sys/devices/platform/coretemp.0/hwmon/hwmon6/temp1_input\"
}

memory {
        format = \"%used\"
        threshold_degraded = \"10%\"
        format_degraded = \"MEMORY: %free\"
}

"))

(define-record-type* <home-i3status-configuration>
  home-i3status-configuration make-home-i3status-configuration
  home-i3status-configuration?
  (config-file home-i3status-config-file
               (default %i3status-config)
               (description "Path to i3status config file."))
  (packages home-i3status-packages
            (default (list i3status))
            (description "List of i3status-related packages to install.")))

(define (home-i3status-files config)
  "Return alist of i3status configuration files to deploy."
  `((".config/i3status/config" ,(home-i3status-config-file config))))

(define (home-i3status-profile-packages config)
  "Return list of i3status packages to install."
  (home-i3status-packages config))

(define home-i3status-service-type
  (service-type
   (name 'home-i3status)
   (extensions
    (list (service-extension home-files-service-type
                             home-i3status-files)
          (service-extension home-profile-service-type
                             home-i3status-profile-packages)))
   (default-value (home-i3status-configuration))
   (description
    "Install and configure i3status status bar for the user.")))
