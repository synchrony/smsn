
#adb uninstall net.fortytwo.extendo

# First start the emulator, then:
mvn clean install \
    && adb shell pm uninstall -k net.fortytwo.extendo \
    && adb install target/extendo-android-*.apk

adb logcat



# create Amarino Maven dependency
mvn install:install-file -DgroupId=at.abraxas -DartifactId=amarino -Dversion=2.0 -Dpackaging=jar -DcreateChecksum=true -Dfile=AmarinoLibrary.jar
