package net.fortytwo.extendo.p2p;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.Socket;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class ExampleSubscriber {
    protected static final Logger LOGGER = Logger.getLogger(ExampleSubscriber.class.getName());

    public ExampleSubscriber() {
        //new Thread(new Runnable() {
        //    public void run() {
                try {
                    listenForNotifications();
                } catch (Throwable e) {
                    LOGGER.severe("SPARQL notification listener stream closed with error: " + e.getMessage());
                    e.printStackTrace(System.err);
                }
        //    }
        //}).start();
    }

    private void listenForNotifications() throws IOException {
        final String host = "localhost";
        final int portNumber = 4201;
        LOGGER.info("creating socket to '" + host + "' on port " + portNumber);

        while (true) {
            LOGGER.info("opening SPARQL notification socket for reading");

            Socket socket = new Socket(host, portNumber);
            BufferedReader br = new BufferedReader(new InputStreamReader(socket.getInputStream()));

            String line;
            while (null != (line = br.readLine())) {
                System.out.println("notification from facilitator: " + line);
            }
        }
    }

    public static void main(final String[] args) throws Exception {
        new ExampleSubscriber();
    }
}
