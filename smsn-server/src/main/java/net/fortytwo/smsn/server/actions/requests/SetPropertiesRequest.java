package net.fortytwo.smsn.server.actions.requests;

import net.fortytwo.smsn.server.Request;

import javax.validation.constraints.NotNull;

public class SetPropertiesRequest extends Request {
    @NotNull
    private String id;
    @NotNull
    private String name;
    @NotNull
    private Object value;

    public String getId() {
        return id;
    }

    public String getName() {
        return name;
    }

    public Object getValue() {
        return value;
    }

    public void setId(String id) {
        this.id = id;
    }

    public void setName(String name) {
        this.name = name;
    }

    public void setValue(Object value) {
        this.value = value;
    }
}
