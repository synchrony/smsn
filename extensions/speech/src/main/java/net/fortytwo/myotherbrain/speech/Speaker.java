package net.fortytwo.myotherbrain.speech;

import com.sun.speech.freetts.Voice;
import com.sun.speech.freetts.VoiceManager;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class Speaker {
    public static final String
        KEVIN = "kevin",
        ALAN = "alan";

    private final Voice voice;

    public Speaker(final String name) {
        VoiceManager voiceManager = VoiceManager.getInstance();
        voice = voiceManager.getVoice(name);

        if (null == voice) {
            throw new IllegalStateException("Cannot find a voice named " + name);
        }

        voice.allocate();
    }

    public Speaker() {
        this(KEVIN);
    }

    synchronized void speak(final String s) {
        System.out.println(voice.getName() + ": " + s);
        voice.speak(s);
    }

    @Override
    public void finalize() throws Throwable {
        if (null != voice) {
            voice.deallocate();
        }

        super.finalize();
    }

    public static void main(final String[] args) throws Exception {
        Speaker s = new Speaker();
        s.speak("it is 3:12PM");
    }
}
