package net.fortytwo.extendo.ping;

import android.content.Context;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class DeepThoughtsPinger extends Pinger {
    public DeepThoughtsPinger(final Context context) {
        super("deepthoughts", context);
    }

    public Ping createPing() {
        return new Ping() {
            public String getTitle() {
                return "Think deep.";
            }

            public Class getActivityClass() {
                return BrainPingPopup.class;
            }
        };
    }
}
