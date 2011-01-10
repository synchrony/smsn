package net.fortytwo.myotherbrain;

import android.app.Activity;
import android.content.SharedPreferences;
import android.os.Bundle;


public class BrainPingSettings extends Activity {
    private static final String
            NOTIFICATIONS_ACTIVE = "notificationsActive",
            START_TIME = "startTime",
            END_TIME = "endTime",
            FREQUENCY = "frequency";

    //private final DateFormat dateFormat = android.text.format.DateFormat.getDateFormat(getApplicationContext());

    // TODO: learn what the "mode" parameter is for
    private final SharedPreferences prefs = getPreferences(0);
    private final SharedPreferences.Editor editor = prefs.edit();

    private boolean isActive;
    private float startTime;
    private float endTime;
    private float frequency;

    @Override
    public void onCreate(final Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.debugging);


        isActive = prefs.getBoolean(NOTIFICATIONS_ACTIVE, false);
        startTime = prefs.getFloat(START_TIME, 10f);
        endTime = prefs.getFloat(END_TIME, 22f);
        frequency = prefs.getFloat(FREQUENCY, 1f);
    }

    private void setIsActive(final boolean b) {
        isActive = b;
        editor.putBoolean(NOTIFICATIONS_ACTIVE, isActive);
        editor.commit();
    }

    private void setStartTime(final float f) {
        if (f < 0f || f >= 24f) {
            throw new IllegalArgumentException();
        }

        startTime = f;
        editor.putFloat(START_TIME, startTime);
        editor.commit();
        if (startTime > endTime) {
            setEndTime(startTime);
        }
    }

    private void setEndTime(final float f) {
        if (f < 0f || f >= 24f) {
            throw new IllegalArgumentException();
        }

        endTime = f;
        editor.putFloat(END_TIME, endTime);
        editor.commit();
        if (endTime < startTime) {
            setStartTime(endTime);
        }
    }

    private void setFrequency(final float f) {
        if (f <= 0f || f > 288f) {
            throw new IllegalArgumentException();
        }

        frequency = f;
        editor.putFloat(FREQUENCY, frequency);
    }
}
