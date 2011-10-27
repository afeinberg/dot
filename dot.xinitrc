xsetroot -cursor_name left_ptr
xset s off
xscreensaver -no-splash &
unclutter -idle 1 &
tint2 -c ~/.xmonad/tint2rc &
nvidia-settings -l
eval `ssh-agent -s`
setxkbmap -option ctrl:nocaps    
#gnome-session --session=xmonad
/usr/local/bin/xmonad
