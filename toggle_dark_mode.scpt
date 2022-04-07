tell application "System Events"
    tell appearance preferences
        set dark mode to not dark mode
        do shell script "/opt/homebrew/Cellar/emacs-mac/emacs-28.1-mac-9.0/bin/emacsclient --eval '(load-theme (ketan0/responsive-theme) t)'"
    end tell
end tell
