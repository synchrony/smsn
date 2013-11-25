package net.fortytwo.extendo.brainstem;

import android.util.Log;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.net.Socket;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class SparqlNotificationListener {

    private boolean started = false;

    private final String host;
    private final int port;
    private final Brainstem.NotificationToneGenerator toneGenerator;

    public SparqlNotificationListener(final String host,
                                      final int port,
                                      final Brainstem.NotificationToneGenerator toneGenerator) {
        this.host = host;
        this.port = port;
        this.toneGenerator = toneGenerator;
    }

    public void start() {
        if (!started) {
            Log.i(Brainstem.TAG, "starting notification listener");

            started = true;
            new Thread(new Runnable() {
                public void run() {
                    try {
                        listenForNotifications();
                    } catch (Throwable e) {
                        Log.e(Brainstem.TAG, "SPARQL notification listener stream closed with error: " + e.getMessage());
                        e.printStackTrace(System.err);
                    }
                }
            }).start();
        }
    }

    private void listenForNotifications() throws IOException {
        Log.i(Brainstem.TAG, "creating socket to '" + host + "' on port " + port);

        while (true) {
            Log.i(Brainstem.TAG, "opening SPARQL notification socket for reading");

            Socket socket = new Socket(host, port);
            BufferedReader br = new BufferedReader(new InputStreamReader(socket.getInputStream()));

            String line;
            while (null != (line = br.readLine())) {
                Log.i(Brainstem.TAG, "notification from facilitator: " + line);
                //textEditor.setText("notification: " + line);
                toneGenerator.play();
            }
        }
    }
}
