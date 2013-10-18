
#adb uninstall net.fortytwo.extendo

# First start the emulator, then:
mvn clean install \
    && adb shell pm uninstall -k net.fortytwo.extendo \
    && adb install target/extendo-android-*.apk

adb logcat
