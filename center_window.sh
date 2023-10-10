#!/bin/bash

# Fetch the current space's id
space_id=$(/opt/homebrew/bin/yabai -m query --spaces --space | /opt/homebrew/bin/jq '.index')

# Get the dimensions of the screen for the current space
screen_width=$(/opt/homebrew/bin/yabai -m query --displays --space $space_id | /opt/homebrew/bin/jq '.frame.w')
screen_height=$(/opt/homebrew/bin/yabai -m query --displays --space $space_id | /opt/homebrew/bin/jq '.frame.h')

# Get the dimensions and position of the current window
window_width=$(/opt/homebrew/bin/yabai -m query --windows --window | /opt/homebrew/bin/jq '.frame.w')
window_height=$(/opt/homebrew/bin/yabai -m query --windows --window | /opt/homebrew/bin/jq '.frame.h')

# Calculate the centered x and y coordinates
centered_x=$(( ($screen_width / 2) - ($window_width / 2) ))
centered_y=$(( ($screen_height / 2) - ($window_height / 2) ))

# Move and resize the window to the calculated centered position
/opt/homebrew/bin/yabai -m window --move abs:$centered_x:$centered_y
