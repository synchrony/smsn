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
    private boolean stopped = false;

    private String host;
    private int port;
    private final Brainstem.NotificationToneGenerator toneGenerator;

    public SparqlNotificationListener(final Brainstem.NotificationToneGenerator toneGenerator) {
        this.toneGenerator = toneGenerator;
    }

    public void setHost(String host) {
        this.host = host;
    }

    public void setPort(int port) {
        this.port = port;
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

    public void stop() {
        stopped = true;
    }

    private void listenForNotifications() throws IOException {
        Log.i(Brainstem.TAG, "creating socket to '" + host + "' on port " + port);

        while (!stopped) {
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
