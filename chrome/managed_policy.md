* Linux
```javascript
/*
  /etc/opt/chrome/policies/managed/managed_policy.json
*/

{
    "BookmarkBarEnabled": false
}
```
* MacOS
```xml
<!--
  1. edit chrome_disable_bookmark_bar.mobileconfig
  2. install via System Settings → Privacy & Security → Profiles
-->

<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"
    "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
  <dict>
    <key>PayloadContent</key>
    <array>
      <dict>
        <key>PayloadType</key>
        <string>com.google.Chrome</string>
        <key>PayloadVersion</key>
        <integer>1</integer>
        <key>PayloadIdentifier</key>
        <string>com.example.chrome.disablebookmarks</string>
        <key>PayloadUUID</key>
        <string>8B3F1234-5678-90AB-CDEF-001122334455</string>
        <key>PayloadDisplayName</key>
        <string>Disable Chrome Bookmark Bar</string>
        <key>PayloadEnabled</key>
        <true/>
        <key>BookmarkBarEnabled</key>
        <false/>
      </dict>
    </array>
    <key>PayloadType</key>
    <string>Configuration</string>
    <key>PayloadVersion</key>
    <integer>1</integer>
    <key>PayloadIdentifier</key>
    <string>com.example.chrome.config</string>
    <key>PayloadUUID</key>
    <string>F0A1B2C3-D4E5-F6A7-890B-CDEF01234567</string>
    <key>PayloadDisplayName</key>
    <string>Chrome Configuration</string>
  </dict>
</plist>
```
