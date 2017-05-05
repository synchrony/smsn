package net.fortytwo.smsn.p2p;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.Socket;
import java.util.logging.Logger;

public class ExampleSubscriber {
    private static final Logger logger = Logger.getLogger(ExampleSubscriber.class.getName());

    private ExampleSubscriber() {
        try {
            listenForNotifications();
        } catch (Throwable e) {
            logger.severe("SPARQL notification listener stream closed with error: " + e.getMessage());
            e.printStackTrace(System.err);
        }
    }

    private void listenForNotifications() throws IOException {
        final String host = "localhost";
        final int portNumber = 4201;
        logger.info("creating socket to '" + host + "' on port " + portNumber);

        while (true) {
            logger.info("opening SPARQL notification socket for reading");

            Socket socket = new Socket(host, portNumber);
            BufferedReader br = new BufferedReader(new InputStreamReader(socket.getInputStream()));

            String line;
            while (null != (line = br.readLine())) {
                System.out.println("notification from coordinator: " + line);
            }
        }
    }

    public static void main(final String[] args) throws Exception {
        new ExampleSubscriber();
    }
}
