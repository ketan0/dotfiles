tell application "System Events"
  set frontApp to name of first application process whose frontmost is true
  tell process frontApp
    set visible of first window to false
  end tell
end tell
