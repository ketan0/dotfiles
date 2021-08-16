tell application "System Events"
    tell appearance preferences
        set dark mode to not dark mode
        do shell script "/usr/local/bin/emacsclient --eval '(load-theme (ketan0/responsive-theme) t)'"
    end tell
end tell
