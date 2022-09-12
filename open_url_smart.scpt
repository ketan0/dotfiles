on run (clp)
    -- given "block" argument on command line, block certain sites from 9am to 8pm
    if clp's length is 2 and clp's item 2 = "block"
        tell (current date) to set currentHour to (its hours)
        if currentHour >= 9 and currentHour < 20
            display notification "Blocked!"
            return
        end if
    end if
    tell application "Google Chrome"
    -- tell application "Safari"
        set window_list to every window
        repeat with w in window_list
            set i to 1
            set tab_list to every tab of w
            repeat with t in tab_list
                if URL of t starts with clp's item 1 then
                    -- set current tab of w to t
                    set active tab index of w to i
                    tell w
                        set index to 1
                    end tell
                    -- delay 0.05
                    -- do shell script "open -a Safari"
                    tell application "System Events"
                        perform action "AXRaise" of front window of application process "Google Chrome"
                        -- perform action "AXRaise" of front window of application process "Safari"
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
