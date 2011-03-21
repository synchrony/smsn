package net.fortytwo.myotherbrain.flashcards.games.android;

import android.app.Activity;
import android.os.Bundle;
import android.widget.TextView;
import net.fortytwo.myotherbrain.R;

public class FlashcardsInfo extends Activity {
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.flashcards_info_layout);

        String info = this.getIntent().getExtras().getString(Flashcards4Android.INFO);
        TextView text = (TextView) findViewById(R.id.text);
        text.setText(info);
    }
}
