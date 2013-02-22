package net.fortytwo.myotherbrain.speech;

import edu.cmu.sphinx.frontend.util.Microphone;
import edu.cmu.sphinx.recognizer.Recognizer;
import edu.cmu.sphinx.result.Result;
import edu.cmu.sphinx.util.props.ConfigurationManager;

import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class SpeechAgent {
    private static final Speaker SPEAKER = new Speaker();

    private final BlockingQueue<Result> queue = new LinkedBlockingQueue<Result>();
    private boolean listening;

    private synchronized void startListening() {
        System.out.println("LISTENING ----------------");
        listening = true;
    }

    private synchronized void stopListening() {
        System.out.println("--------------------------");
        listening = false;
    }

    private synchronized boolean isListening() {
        return listening;
    }

    public static void main(String[] args) {
        try {
            new SpeechAgent().doit();
        } catch (Throwable t) {
            t.printStackTrace(System.err);
            System.exit(1);
        }
    }

    private void doit() throws InterruptedException {
        ConfigurationManager cm = new ConfigurationManager(SpeechAgent.class.getResource("mobspeech.config.xml"));

        new Thread() {
            @Override
            public void run() {
                while (true) {
                    Result r = null;
                    try {
                        r = queue.take();
                    } catch (InterruptedException e) {
                        e.printStackTrace();
                    }
                    stopListening();
                    float score = -1 * r.getBestFinalToken().getScore() / 1000f;

                    System.out.println("got result: " + r + " (" + score + ")");
                    if (score < 5000) {
                        System.out.println("\tscore is too low");
                    } else {
                        String text = r.getBestFinalResultNoFiller().trim().toLowerCase();

                        if (text.startsWith("computer")) {
                            if (text.equals("computer time")) {
                                SPEAKER.speak("it is " + spokenTime());
                            } else if (text.equals("computer date")) {
                                SPEAKER.speak("it is " + spokenDate());
                            } else {
                                SPEAKER.speak("you said: " + text);
                            }

                            // Grace period keeps the recognizer from reacting to the speaker's voice.
                            // It takes some time to pull the spurious last result out of the recognizer and throw it away.
                            try {
                                Thread.sleep(1000);
                            } catch (InterruptedException e) {
                                throw new IllegalStateException(e);
                            }
                        } else {
                            System.out.println("\tred herring");
                        }
                    }

                    startListening();
                }
            }
        }.start();

        Recognizer recognizer = (Recognizer) cm.lookup("recognizer");
        //recognizer.addResultListener(listener);
        recognizer.allocate();

        // start the microphone or exit if the program if this is not possible
        Microphone microphone = (Microphone) cm.lookup("microphone");
        if (!microphone.startRecording()) {
            System.out.println("Cannot start microphone.");
            recognizer.deallocate();
            System.exit(1);
        }

        startListening();
        System.out.println("Start speaking.\n");
        while (true) {
            System.out.println("recognizing...");
            Result result = recognizer.recognize();
            System.out.println("\tdone.");

            if (result != null) {
                System.out.println("pulled result: " + result);
                if (isListening() && result.getBestFinalResultNoFiller().length() > 0) {
                    System.out.println("\tadding to queue");
                    queue.put(result);
                }
            } else {
                System.out.println("I can't hear what you said.\n");
            }
        }
    }

    private static final String[] MONTH_NAMES = {"January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"};

    private String spokenDate() {
        Calendar c = new GregorianCalendar();
        c.setTime(new Date());

        StringBuilder sb = new StringBuilder();

        sb.append(MONTH_NAMES[c.get(Calendar.MONTH)]);
        sb.append(" ");
        sb.append(c.get(Calendar.DAY_OF_MONTH));
        sb.append(", ");
        sb.append(c.get(Calendar.YEAR));

        return sb.toString();
    }

    private String spokenTime() {
        Calendar c = new GregorianCalendar();
        c.setTime(new Date());

        StringBuilder sb = new StringBuilder();

        sb.append(c.get(Calendar.HOUR));
        sb.append(" ");

        int min = c.get(Calendar.MINUTE);
        if (0 == min) {
            sb.append("o' clock");
        } else if (min < 10) {
            sb.append("oh ").append(min);
        } else {
            sb.append(min);
        }

        sb.append(" ");
        sb.append(c.get(Calendar.HOUR_OF_DAY) >= 12 ? "PM" : "AM");

        return sb.toString();
    }
}