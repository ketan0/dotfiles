tell application "System Events"
    set frontApp to first application process whose frontmost is true
    set frontAppName to name of frontApp
    tell process frontAppName
        set currentWindow to (1st window whose value of attribute "AXMain" is true)
        set visible of currentWindow to false
    end tell
end tell
