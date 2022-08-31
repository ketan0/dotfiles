tell application "Safari"
    set theURL to URL of current tab of window 1
    set the clipboard to theURL
end tell
