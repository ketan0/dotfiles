use script "CalendarLib EC" version "1.1.5"
use scripting additions
use framework "Foundation"

property NSRegularExpressionCaseInsensitive : a reference to 1
property NSRegularExpression : a reference to current application's NSRegularExpression

-- fetch properties of events for next week
set now to current date
set theStore to fetch store
set theCal to fetch calendar "Calendar" cal type cal exchange event store theStore -- change to suit
set theEvents to fetch events starting date now ending date now searching cals {theCal} event store theStore -- get events that are occurring currently
if length of theEvents is 0
    display notification "No events currently!"
    return
end if
set theEvent to (item 1 of theEvents)
set theEventRecord to event info for event theEvent
set theEventNotes to (get event_description of theEventRecord)
if theEventNotes is missing value
    display notification "Couldn't find the zoom link. Opening calendar..."
    tell application "Calendar" to activate
    return
end if
set theNSStringSample to current application's NSString's stringWithString:theEventNotes
set passcodePattern to "Password:(?:\\s|\\n)+(\\d{6})"
set thePasscodeRegEx to NSRegularExpression's regularExpressionWithPattern:passcodePattern options:NSRegularExpressionCaseInsensitive |error|:(missing value)
set aMatch to thePasscodeRegEx's firstMatchInString:theNSStringSample options:0 range:[0, theNSStringSample's |length|]
if aMatch is not missing value then
    set partRange to (aMatch's rangeAtIndex:1) as record
    set passcode to (theNSStringSample's substringWithRange:partRange) as text
    set the clipboard to passcode -- copy the passcode in case zoom prompts for it
else
    display notification "Couldn't find the passcode."
end if

set zoomLinkPattern to "https:\\/\\/(?:.+\\.)?zoom\\.us\\/j\\/(\\d+)\\?pwd=([a-zA-Z0-9]+)"
set theZoomLinkRegEx to NSRegularExpression's regularExpressionWithPattern:zoomLinkPattern options:NSRegularExpressionCaseInsensitive |error|:(missing value)
set aMatch to theZoomLinkRegEx's firstMatchInString:theNSStringSample options:0 range:[0, theNSStringSample's |length|]
if aMatch is not missing value then
    set partRange to (aMatch's rangeAtIndex:1) as record
    set zoomConfNo to (theNSStringSample's substringWithRange:partRange) as text
    set partRange to (aMatch's rangeAtIndex:2) as record
    set zoomPwd to (theNSStringSample's substringWithRange:partRange) as text
    display notification "Starting zoom..."
    open location "zoommtg://zoom.us/join?confno=" & zoomConfno & "&pwd=" & zoomPwd
else
    display notification "Couldn't find the zoom link. Opening calendar..."
    tell application "Calendar" to activate
end if
