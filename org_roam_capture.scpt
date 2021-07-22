tell application "Google Chrome"
    execute front window's active tab javascript "javascript:location.href = 'org-protocol://roam-ref?template=r&ref=' + encodeURIComponent(location.href) + '&title=' + encodeURIComponent(document.title)"
end tell
