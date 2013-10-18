package net.fortytwo.myotherbrain;

import android.app.Activity;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.widget.TextView;
import net.fortytwo.myotherbrain.R;

import java.util.Map;

public class Info extends Activity {
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.info_layout);

        StringBuilder sb = new StringBuilder();
        SharedPreferences prefs = PreferenceManager.getDefaultSharedPreferences(getApplicationContext());
        Map<String, ?> map = prefs.getAll();
        for (String key : map.keySet()) {
            sb.append(key).append(": ");
            sb.append(map.get(key)).append("\n");
        }
        TextView text = (TextView) findViewById(R.id.text);
        text.setText(sb.toString());
    }
}
