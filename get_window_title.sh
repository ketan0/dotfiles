#!/bin/bash
set -euo pipefail
title=$(/opt/homebrew/bin/yabai -m query --windows --window | /opt/homebrew/bin/jq -r '.title')
osascript -e "display notification \"Current window title: $title\" with title \"Window Info\""
