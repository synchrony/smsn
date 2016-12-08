package net.fortytwo.smsn.server.action.requests;

import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.server.Request;
import org.json.JSONException;
import org.json.JSONObject;

import java.security.Principal;

public class ReadGraphRequest extends Request {
    private final String format;
    private final String file;

    public ReadGraphRequest(final JSONObject json,
                            final Principal user) throws JSONException {
        super(json, user);

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
