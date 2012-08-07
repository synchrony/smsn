package net.fortytwo.droidspeak;

import javax.sound.sampled.SourceDataLine;
import java.io.InputStream;

/**
* @author Joshua Shinavier (http://fortytwo.net)
*/ //Inner class to play back the data that was
// saved.
class ListenThread extends Thread {
    private final InputStream audioInputStream;
    private final SourceDataLine sourceDataLine;

    ListenThread(final InputStream audioInputStream,
                 final SourceDataLine sourceDataLine) {
        this.audioInputStream = audioInputStream;
        this.sourceDataLine = sourceDataLine;
    }

    public void run() {
        //This is a working buffer used to transfer
        // the data between the AudioInputStream and
        // the SourceDataLine.  The size is rather
        // arbitrary.
        byte playBuffer[] = new byte[3200];

        try {
            //Open and start the SourceDataLine
            sourceDataLine.open(Droidspeak.AUDIO_FORMAT);
            sourceDataLine.start();

            int cnt;

            //Transfer the audio data to the speakers
            while ((cnt = audioInputStream.read(
                    playBuffer, 0,
                    playBuffer.length))
                    != -1) {
                //Keep looping until the input read
                // method returns -1 for empty stream.
                if (cnt > 0) {
                    //Write data to the internal buffer of
                    // the data line where it will be
                    // delivered to the speakers in real
                    // time
                    sourceDataLine.write(playBuffer, 0, cnt);
                }
            }

            //Block and wait for internal buffer of the
            // SourceDataLine to become empty.
            sourceDataLine.drain();

            //Finish with the SourceDataLine
            sourceDataLine.stop();
            sourceDataLine.close();

        } catch (Exception e) {
            e.printStackTrace();
            System.exit(0);
        }//end catch

    }//end run
}
