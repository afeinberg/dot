xsetroot -cursor_name left_ptr

xscreensaver -no-splash &

unclutter -idle 1 &

tint2 -c ~/.xmonad/tint2rc &

pidgin &

nvidia-settings -l

eval `ssh-agent -s`

xmodmap ~/.xmodmaprc

dbus-launch --exit-with-session xmonad
