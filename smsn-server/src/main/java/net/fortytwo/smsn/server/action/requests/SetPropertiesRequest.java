package net.fortytwo.smsn.server.action.requests;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.server.Request;
import org.json.JSONException;
import org.json.JSONObject;

import java.security.Principal;

public class SetPropertiesRequest extends Request {
    private final String id;
    private final String name;
    private final Object value;

    public SetPropertiesRequest(JSONObject json, Principal user) throws JSONException {
        super(json, user);

        id = this.json.getString(Params.ID);
        name = this.json.getString(Params.NAME);
        value = getName().equals(SemanticSynchrony.SHORTCUT)
                ? this.json.getString(Params.VALUE)
                : (float) this.json.getDouble(Params.VALUE);
    }

    public String getId() {
        return id;
    }

    public String getName() {
        return name;
    }

    public Object getValue() {
        return value;
    }
}
