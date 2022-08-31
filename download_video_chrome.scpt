tell application "Google Chrome"
    set theURL to URL of active tab of first window
    display notification "/opt/homebrew/bin/youtube-dl -o \"~/Downloads/%(title)s.%(ext)s\" '" & theURL & "'"
    do shell script "/opt/homebrew/bin/youtube-dl -o \"~/Downloads/%(title)s.%(ext)s\" '" & theURL & "'"
end tell
