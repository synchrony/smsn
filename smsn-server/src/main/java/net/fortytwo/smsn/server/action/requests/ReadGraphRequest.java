package net.fortytwo.smsn.server.action.requests;

import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.server.Request;
import org.json.JSONException;
import org.json.JSONObject;

public class ReadGraphRequest extends Request {
    private final String format;
    private final String file;

    public ReadGraphRequest(final JSONObject json) throws JSONException {
        super(json);

        format = this.json.getString(Params.FORMAT);
        file = this.json.getString(Params.FILE);
    }

    public String getFormat() {
        return format;
    }

    public String getFile() {
        return file;
    }
}
