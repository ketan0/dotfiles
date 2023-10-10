#!/bin/bash

# Query all windows and filter those whose title contains the arg, then get the first one's ID
window_id=$(/opt/homebrew/bin/yabai -m query --windows | /opt/homebrew/bin/jq '.[] | select(.title | test('"\"$1\""'; "i")) | .id' | head -n 1)

# If a window was found, focus it
if [ ! -z "$window_id" ]; then
    /opt/homebrew/bin/yabai -m window --focus $window_id
else
    echo "No window with '$1' in the title found."
fi
