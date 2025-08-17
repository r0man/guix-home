(define-module (r0man guix home kitty)
  #:use-module (gnu home services)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages terminals)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:export (home-kitty-services))

(define kitty-config
  (mixed-text-file
   "kitty-config" "
background_opacity 0.8
font_family Hack
font_size 14.0

foreground            #D8DEE9
background            #2E3440
selection_foreground  #000000
selection_background  #FFFACD
url_color             #0087BD
cursor                #81A1C1

# black
color0   #3B4252
color8   #4C566A

# red
color1   #BF616A
color9   #BF616A

# green
color2   #A3BE8C
color10  #A3BE8C

# yellow
color3   #EBCB8B
color11  #EBCB8B

# blue
color4  #81A1C1
color12 #81A1C1

# magenta
color5   #B48EAD
color13  #B48EAD

# cyan
color6   #88C0D0
color14  #8FBCBB

# white
color7   #E5E9F0
color15  #ECEFF4

shell " bash "/bin/bash --login
"))

(define home-kitty-services
  (list (simple-service 'kitty-config home-files-service-type
                        `((".config/kitty/kitty.conf" ,kitty-config)))
        (simple-service 'kitty-packages home-profile-service-type
                        (list kitty))))
