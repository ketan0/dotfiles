tell application "Safari"
    set theURL to URL of current tab of window 1
    tell application "Google Chrome" to open location theURL
end tell
