package net.fortytwo.extendo.brainstem;

import android.util.Log;
import net.fortytwo.extendo.brain.Note;
import net.fortytwo.extendo.brain.wiki.NoteWriter;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.NameValuePair;
import org.apache.http.StatusLine;
import org.apache.http.client.HttpClient;
import org.apache.http.client.entity.UrlEncodedFormEntity;
import org.apache.http.client.methods.HttpPost;
import org.apache.http.impl.client.DefaultHttpClient;
import org.apache.http.message.BasicNameValuePair;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class EventStackProxy {
    private final String endpointUrl;
    private final NoteWriter writer = new NoteWriter();

    private final HttpClient httpclient = new DefaultHttpClient();

    public EventStackProxy(final String serviceUrl) {
        this.endpointUrl = serviceUrl;
    }

    public void push(final Note event) {

        JSONObject r;
        try {
            JSONObject view = writer.toJSON(event);
            r = new JSONObject();
            r.put("view", view);
        } catch (JSONException e) {
            Log.e(Brainstem.TAG, "failed to create event JSON: " + e.getMessage());
            return;
        }

        Log.i(Brainstem.TAG, "pushing event to endpoint " + endpointUrl + " with data " + r);

        try {
            HttpPost request = new HttpPost(endpointUrl);

            //request.setEntity(new StringEntity(j.toString()));

            List<NameValuePair> params = new ArrayList<NameValuePair>();
            params.add(new BasicNameValuePair("request", r.toString()));
            request.setEntity(new UrlEncodedFormEntity(params));

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
