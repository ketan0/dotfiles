on run (clp)
    tell application "Safari"
        repeat with w in windows
            set i to 1
            repeat with t in tabs of w
                if URL of t starts with clp's item 1 then
                    set current tab of w to t
                    -- set active tab index of w to i
                    tell w
                        set index to 1
                    end tell
                    -- delay 0.05
                    -- do shell script "open -a Safari"
                    tell application "System Events"
                        perform action "AXRaise" of front window of application process "Safari"
                    end tell
                    activate
                    return
                end if
                set i to i + 1
            end repeat
        end repeat
        open location clp's item 1
        activate
    end tell
end run
