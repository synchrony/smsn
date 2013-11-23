package net.fortytwo.extendo.brainstem;

import android.util.Log;
import net.fortytwo.extendo.brain.Note;
import net.fortytwo.extendo.brain.wiki.NoteWriter;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.StatusLine;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpPut;
import org.apache.http.entity.StringEntity;
import org.apache.http.impl.client.DefaultHttpClient;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.ByteArrayOutputStream;
import java.io.IOException;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class EventStackProxy {
    private final String endpointUrl;
    private final NoteWriter writer = new NoteWriter();

    public EventStackProxy(final String serviceUrl) {
        this.endpointUrl = serviceUrl;
    }

    public void push(final Note event) {
        JSONObject j;
        try {
            j = writer.toJSON(event);
        } catch (JSONException e) {
            Log.e(Brainstem.TAG, "failed to create event JSON: " + e.getMessage());
            return;
        }

        try {
            HttpClient httpclient = new DefaultHttpClient();
            HttpPut request = new HttpPut(endpointUrl);
            request.setEntity(new StringEntity(j.toString()));
            HttpResponse response = httpclient.execute(request);
            StatusLine statusLine = response.getStatusLine();
            if (statusLine.getStatusCode() == HttpStatus.SC_OK) {
                ByteArrayOutputStream out = new ByteArrayOutputStream();
                response.getEntity().writeTo(out);
                out.close();
                String responseString = out.toString();
                //..more logic
            } else {
                //Closes the connection.
                response.getEntity().getContent().close();
                Log.e(Brainstem.TAG, "Extendo event service responded with error: " + statusLine.getReasonPhrase());
            }
        } catch (IOException e) {
            Log.e(Brainstem.TAG, "failed push to Extendo event service: " + e.getMessage());
        }
    }
}
