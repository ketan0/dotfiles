#!/usr/bin/env bash
if [ "$(/opt/homebrew/bin/blueutil --is-connected 88-c9-e8-37-fe-a0)" = "1" ]; then
    /opt/homebrew/bin/blueutil --disconnect 88-c9-e8-37-fe-a0
else
    /opt/homebrew/bin/blueutil --connect 88-c9-e8-37-fe-a0
fi
