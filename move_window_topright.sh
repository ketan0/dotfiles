#!/bin/bash
set -euo pipefail
# Query the size of the first display (modify if you have multiple displays and need a specific one)
display_info=$(/opt/homebrew/bin/yabai -m query --displays | /opt/homebrew/bin/jq '.[0]')
display_width=$(echo $display_info | /opt/homebrew/bin/jq '.frame.w')
display_height=$(echo $display_info | /opt/homebrew/bin/jq '.frame.h')

# Query the dimensions of the current window
window_info=$(/opt/homebrew/bin/yabai -m query --windows --window)
window_width=$(echo $window_info | /opt/homebrew/bin/jq '.frame.w')
window_height=$(echo $window_info | /opt/homebrew/bin/jq '.frame.h')

# Calculate x and y position for the top-right corner
x_position=$(expr $display_width - $window_width)
y_position=0

# Move the window
/opt/homebrew/bin/yabai -m window --move abs:$x_position:$y_position
