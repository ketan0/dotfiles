#!/bin/bash
set -euo pipefail

# Get current space, window and display information
current_space=$(/opt/homebrew/bin/yabai -m query --spaces --space | /opt/homebrew/bin/jq '.index')
current_window=$(/opt/homebrew/bin/yabai -m query --windows --window | /opt/homebrew/bin/jq '.id')
current_display=$(/opt/homebrew/bin/yabai -m query --displays --display | /opt/homebrew/bin/jq '.index')

# Get all displays and their count
displays=($(/opt/homebrew/bin/yabai -m query --displays | /opt/homebrew/bin/jq '.[].index'))
display_count=${#displays[@]}

# Determine the target display
target_display=${displays[display_count - 1]}
for ((i=display_count - 1; i > 0; i--)); do
    if [[ ${displays[i]} == $current_display ]]; then
        target_display=${displays[i-1]}
        break
    fi
done

# Move window to the target display
/opt/homebrew/bin/yabai -m window $current_window --display $target_display

# Focus the moved window and its new space
/opt/homebrew/bin/yabai -m display --focus $target_display
/opt/homebrew/bin/yabai -m space --focus $current_space
