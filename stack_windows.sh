#!/usr/bin/env bash

# Get the ID of the currently focused window
focused_window=$(/opt/homebrew/bin/yabai -m query --windows --window | /opt/homebrew/bin/jq '.id')

# Get the ID of the target window (second argument, if any; otherwise, the one under the mouse)
if [ -z "$1" ]; then
  target_window=$(/opt/homebrew/bin/yabai -m query --windows --window mouse | /opt/homebrew/bin/jq '.id')
else
  target_window=$1
fi

# Stack the focused window on top of the target window
/opt/homebrew/bin/yabai -m window "$focused_window" --stack "$target_window"

# Optional: Add a notification or message to indicate success
echo "Window $focused_window stacked on top of window $target_window."
