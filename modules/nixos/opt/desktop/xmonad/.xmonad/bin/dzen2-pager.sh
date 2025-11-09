#!/usr/bin/env sh
BG='#111111'
FG=grey70
OPTS="-bg $BG -fg $FG -p -l 45 -w 1200 -x 5 -y 40 -e onstart=uncollapse,scrollhome;button5=scrolldown;key_Down=scrolldown;button4=scrollup;key_Up=scrollup;key_Page_Down=scrolldown:30;key_Page_Up=scrollup:30;key_Escape=exit;button3=exit;entertitle=grabkeys;enterslave=grabkeys;leaveslave=ungrabkeys"

(echo "^fg(#323232)^bg(#5FBF77) Hotkeys (use: scrollwheel, arrow keys, PgUP/PgDown to scroll. Escape or right mouse button to quit) ";
 cat \
     | sed -E 's/^(\w[^ ]+\s)(\w|<\w+>)\s/\1^fg(#ffa07a)\2 ^fg()/' \
     | sed -E 's/^(\w[^ ]+)/  ^fg(#F0C674)\1^fg()/' \
     | sed -E 's/>> --/--/' \
     | sed -E 's/^(--.*)$/^fg(#87ceeb)\1/';
 echo '~'
) | dzen2 -fn "dejavu sans mono:size=10" $OPTS
