package net.fortytwo.myotherbrain;

import android.content.SharedPreferences;
import android.os.Bundle;
import android.preference.CheckBoxPreference;
import android.preference.Preference;
import android.preference.PreferenceActivity;
import android.preference.PreferenceManager;
import android.preference.TimePickerPreference;


public class BrainPingSettings extends PreferenceActivity {
    private static final String
            ENABLED = "brainping_enabled",
            START_TIME = "brainping_starttime",
            END_TIME = "brainping_endtime";

    @Override
    public void onCreate(final Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        // Note: this is the earliest you can get preferences.  Getting them at construction time results in an error.
        final SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(getApplicationContext());

        addPreferencesFromResource(R.xml.brainping);

        /*
        // TODO: this listener is unnecessary
        CheckBoxPreference enabler = (CheckBoxPreference) findPreference(ENABLED);
        enabler.setOnPreferenceChangeListener(new Preference.OnPreferenceChangeListener() {
            public boolean onPreferenceChange(final Preference preference, Object o) {
                System.out.println("enabled: " + prefs.getBoolean(ENABLED, false));
                return true;
            }
        });

        TimePickerPreference startTimePref = (TimePickerPreference) findPreference(START_TIME);
        startTimePref.setOnPreferenceChangeListener(new Preference.OnPreferenceChangeListener() {
            public boolean onPreferenceChange(final Preference preference,
                                              final Object o) {
                String start = (String) o;
                String end = prefs.getString(END_TIME, "00:00");

                if (start.compareTo(end) > 0) {
                    prefs.edit().putString(END_TIME, start).commit();
                }

                return true;
            }
        });

        TimePickerPreference endTimePref = (TimePickerPreference) findPreference(START_TIME);
        endTimePref.setOnPreferenceChangeListener(new Preference.OnPreferenceChangeListener() {
            public boolean onPreferenceChange(final Preference preference,
                                              final Object o) {
                String end = (String) o;
                String start = prefs.getString(START_TIME, "23:59");

                if (start.compareTo(end) > 0) {
                    prefs.edit().putString(START_TIME, end).commit();
                }

                return true;
            }
        }); */
    }
}
