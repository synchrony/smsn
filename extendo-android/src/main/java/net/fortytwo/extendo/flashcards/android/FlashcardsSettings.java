package net.fortytwo.extendo.flashcards.android;

import android.os.Bundle;
import android.preference.PreferenceActivity;
import net.fortytwo.extendo.R;


public class FlashcardsSettings extends PreferenceActivity {

    @Override
    public void onCreate(final Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        addPreferencesFromResource(R.xml.flashcards_settings);
    }

    @Override
    public void onStop() {
        //System.out.println("stopping...");
        super.onStop();
    }
}
