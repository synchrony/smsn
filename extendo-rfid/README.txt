

# Setting up serial communication (on 32-bit Linux server with Oracle JDK 7)
sudo echo "Driver=gnu.io.RXTXCommDriver" > $JAVA_HOME/jre/lib/javax.comm.properties
cd /tmp
wget http://www.linux.org.uk/~taj/rxtx-bins.1.tar.gz
tar -xzf rxtx-bins.1.tar.gz 
sudo cp /tmp/rxtx-bins.1/1.5/i386-pc-linux/lib* $JAVA_HOME/jre/lib/i386
sudo cp /tmp/rxtx-bins.1/1.5/RXTXcomm.jar $JAVA_HOME/jre/lib/ext
