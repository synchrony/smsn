package net.fortytwo.smsn.server;

import org.apache.tinkerpop.shaded.jackson.annotation.JsonTypeInfo;

import javax.validation.constraints.NotNull;
import java.io.Serializable;

/*
@JsonTypeInfo(
        use = JsonTypeInfo.Id.CUSTOM,
        include = JsonTypeInfo.As.PROPERTY,
        property = "action")*/
@JsonTypeInfo(use = JsonTypeInfo.Id.CLASS, include = JsonTypeInfo.As.PROPERTY, property = "action")
//@JsonTypeInfo(use = JsonTypeInfo.Id.MINIMAL_CLASS, include = JsonTypeInfo.As.PROPERTY, property = "action")
public abstract class Request implements Serializable {
    @NotNull
    private String action;

    public String getAction() {
        return action;
    }

    public void setAction(String action) {
        this.action = action;
    }
}
