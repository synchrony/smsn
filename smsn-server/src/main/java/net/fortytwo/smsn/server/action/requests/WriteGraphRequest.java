package net.fortytwo.smsn.server.action.requests;

import net.fortytwo.smsn.brain.Params;
import org.json.JSONException;
import org.json.JSONObject;

import java.security.Principal;

public class WriteGraphRequest extends FilteredResultsRequest {
    private final String format;
    private final String file;

    private final String rootId;
    private final int height;

    public WriteGraphRequest(final JSONObject json,
                             final Principal user) throws JSONException {
        super(json, user);

        format = this.json.getString(Params.FORMAT);
        file = this.json.getString(Params.FILE);

        rootId = this.json.optString(Params.ROOT);
        height = this.json.optInt(Params.HEIGHT, 0);
    }

    public String getFormat() {
        return format;
    }

    public String getFile() {
        return file;
    }

    public String getRootId() {
        return rootId;
    }

    public int getHeight() {
        return height;
    }
}
