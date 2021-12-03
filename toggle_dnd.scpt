my setDoNoDisturbTo()

-- https://github.com/sindresorhus/do-not-disturb/issues/9
on setDoNoDisturbTo()
    set checkDNDstatusCMD to ¬
        {"defaults read", space, ¬
            "com.apple.ncprefs.plist", ¬
            space, "dnd_status"} as string
    set statusOfDND to ¬
        (do shell script checkDNDstatusCMD) ¬
            as boolean
    if not statusOfDND
        display notification "Turning on Do Not Disturb..."
        set OnOffData to "62706C6973743030D60102030405060708080A08085B646E644D6972726F7265645F100F646E64446973706C6179536C6565705F101E72657065617465644661636574696D6543616C6C73427265616B73444E445875736572507265665E646E64446973706C61794C6F636B5F10136661636574696D6543616E427265616B444E44090808D30B0C0D070F1057656E61626C6564546461746556726561736F6E093341C2B41C4FC9D3891001080808152133545D6C828384858C9499A0A1AAACAD00000000000001010000000000000013000000000000000000000000000000AE"
    else
        set OnOffData to "62706C6973743030D5010203040506070707075B646E644D6972726F7265645F100F646E64446973706C6179536C6565705F101E72657065617465644661636574696D6543616C6C73427265616B73444E445E646E64446973706C61794C6F636B5F10136661636574696D6543616E427265616B444E44090808080808131F3152617778797A7B0000000000000101000000000000000B0000000000000000000000000000007C"
    end if
    set changeDNDstatusCMD to ¬
        {"defaults write", space, ¬
            "com.apple.ncprefs.plist", ¬
            space, "dnd_prefs -data", space, OnOffData, ¬
            space, "&&", ¬
        "defaults write", space, ¬
            "com.apple.ncprefs.plist", ¬
            space, "dnd_status ", not statusOfDND, space, ¬
        "&& killall usernoted && killall ControlCenter"} as string
    do shell script changeDNDstatusCMD
    if statusOfDND
        display notification "Turned off Do Not Disturb."
    end if
end setDoNoDisturbTo
