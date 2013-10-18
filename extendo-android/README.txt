
#adb uninstall net.fortytwo.myotherbrain

# First start the emulator, then:
mvn clean install \
    && adb shell pm uninstall -k net.fortytwo.myotherbrain \
    && adb install target/myotherbrain-android-*.apk

adb logcat
