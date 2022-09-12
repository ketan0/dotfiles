tell application "System Events"
    set activeApp to name of application processes whose frontmost is true
    set visibleApps to every process whose visible is true and name is not activeApp
    repeat with theApp in visibleApps
        set visible of theApp to false
    end repeat
end tell
