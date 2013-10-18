package net.fortytwo.extendo.ping;

import android.content.Context;

/**
 * User: josh
 * Date: 4/19/11
 * Time: 4:55 PM
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
