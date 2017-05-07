package net.fortytwo.smsn.config;

import java.io.File;
import java.io.Serializable;

public class DataSource implements Serializable {

    private String name;
    private String location;
    private Float sharability;
    private Integer displayColor;

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

    public void validate() {
        validateName();
        validateLocation();
        validateSharability();
    }

    private void validateName() {
        if (null == name || 0 == name.trim().length()) {
            throw new IllegalArgumentException("missing name");
        }
    }

    private void validateLocation() {
        if (null == location || 0 == location.trim().length()) {
            throw new IllegalArgumentException("missing location");
        }

        File file = new File(location);
        if (file.exists()) {
            if (!file.isDirectory()) {
                throw new IllegalArgumentException("source file is not a directory");
            }
        } else {
            // attempt to create the directory, and fail if this is not possible
            file.mkdirs();
        }
    }

    private void validateSharability() {
        if (null == sharability) {
            throw new IllegalArgumentException("missing sharability");
        }

        if (sharability <= 0f || sharability > 1f) {
            throw new IllegalArgumentException("invalid sharability");
        }
    }

    public Integer getDisplayColor() {
        return displayColor;
    }

    public void setDisplayColor(Integer displayColor) {
        this.displayColor = displayColor;
    }
}
