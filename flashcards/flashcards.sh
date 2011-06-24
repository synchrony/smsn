#!/bin/bash

# Path to JAR
JAR=`dirname $0`/target/myotherbrain-core-*.jar

# Find Java
if [ "$JAVA_HOME" = "" ] ; then
	JAVA="java"
else
	JAVA="$JAVA_HOME/bin/java"
fi

# Set Java options
if [ "$JAVA_OPTIONS" = "" ] ; then
	JAVA_OPTIONS="-Xms32M -Xmx512M"
fi

# Launch the application
$JAVA $JAVA_OPTIONS -cp $JAR net.fortytwo.myotherbrain.flashcards.games.CommandLineGame $*

# Return the program's exit code
exit $?
