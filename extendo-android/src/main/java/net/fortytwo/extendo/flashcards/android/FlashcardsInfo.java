package net.fortytwo.extendo.flashcards.android;

import android.app.Activity;
import android.os.Bundle;
import android.webkit.WebView;
import net.fortytwo.extendo.R;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class FlashcardsInfo extends Activity {
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.flashcards_info_layout);

        String info = this.getIntent().getExtras().getString(Flashcards4Android.INFO);
        WebView view = (WebView) findViewById(R.id.text);
        view.setScrollBarStyle(WebView.SCROLLBARS_OUTSIDE_OVERLAY);
        String text = Flashcards4Android.HTML_PREFIX + info + Flashcards4Android.HTML_SUFFIX;
        view.loadDataWithBaseURL("file:///android_asset/", text, "application/xhtml+xml", "utf-8", null);

        //String info = this.getIntent().getExtras().getString(Flashcards4Android.INFO);
        //TextView text = (TextView) findViewById(R.id.text);
        //text.setText(info);
    }
}
