package net.fortytwo.extendo.events;

import android.app.Activity;
import android.os.Bundle;
import android.webkit.WebView;
import net.fortytwo.extendo.R;
import net.fortytwo.extendo.flashcards.android.Flashcards4Android;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class EventsActivity  extends Activity {
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.events_layout);

        String info = EventNotifier.getInstance().getInfo();
        WebView view = (WebView) findViewById(R.id.text);
        view.setScrollBarStyle(WebView.SCROLLBARS_OUTSIDE_OVERLAY);
        String text = Flashcards4Android.HTML_PREFIX + info + Flashcards4Android.HTML_SUFFIX;
        view.loadDataWithBaseURL("file:///android_asset/", text, "application/xhtml+xml", "utf-8", null);
    }
}
