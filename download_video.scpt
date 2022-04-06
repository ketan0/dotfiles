tell application "Safari"
    set theURL to URL of current tab of window 1
    display notification "/opt/homebrew/bin/youtube-dl -o \"~/Downloads/%(title)s.%(ext)s\" '" & theURL & "'"
    do shell script "/opt/homebrew/bin/youtube-dl -o \"~/Downloads/%(title)s.%(ext)s\" '" & theURL & "'"
end tell
