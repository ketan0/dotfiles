tell application "Spotify"
    set vol to sound volume
    set vol to vol - 10
    if vol is less than 0 then
        set vol to 0
    end if
    set the sound volume to vol
end tell
