package net.fortytwo.smsn.config;

import java.io.Serializable;

public class DataSource implements Serializable {
    private String name;
    private String location;
    private Float sharability;

    public DataSource() {
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getLocation() {
        return location;
    }

    public void setLocation(String location) {
        this.location = location;
    }

    public Float getSharability() {
        return sharability;
    }

    public void setSharability(Float sharability) {
        this.sharability = sharability;
    }
}
