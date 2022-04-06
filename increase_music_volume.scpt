tell application "Spotify"
    set vol to sound volume
    set vol to vol + 10
    if vol is greater than 100 then
        set vol to 100
    end if
    set the sound volume to vol
end tell
