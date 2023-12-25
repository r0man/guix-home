(define-module (r0man guix home i3status)
  #:use-module (gnu home services)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu system pam)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:export (home-i3status-services))

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

(define files
  `((".config/i3status/config" ,%i3status-config)))

(define packages
  (list i3status))

(define home-i3status-services
  (list (simple-service 'i3status-files home-files-service-type files)
        (simple-service 'i3status-profile home-profile-service-type packages)))
