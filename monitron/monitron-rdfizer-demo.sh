#!/bin/bash

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

DIR=`dirname $0`

# Launch the application
$JAVA $JAVA_OPTIONS -cp $DIR/target/classes:$DIR/"target/dependency/*" net.fortytwo.smsn.monitron.demos.RdfizerDemo $*

# Return the program's exit code
exit $?
