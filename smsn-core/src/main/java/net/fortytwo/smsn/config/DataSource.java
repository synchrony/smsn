package net.fortytwo.smsn.config;

import java.io.File;
import java.io.Serializable;

public class DataSource implements Serializable {

    private String name;
    private String code;
    private String location;
    private Float sharability;
    private Integer displayColor;

    public String getName() {
        return name;
    }

    public void setName(final String name) {
        this.name = name.trim();
    }

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code.trim();
    }

    public String getLocation() {
        return location;
    }

    public void setLocation(String location) {
        this.location = location.trim();
    }

    public Float getSharability() {
        return sharability;
    }

    public void setSharability(Float sharability) {
        this.sharability = sharability;
    }

    public Integer getDisplayColor() {
        return displayColor;
    }

    public void setDisplayColor(Integer displayColor) {
        this.displayColor = displayColor;
    }

    public void validate() {
        validateName();
        validateCode();
        validateLocation();
        validateSharability();
        validateDisplayColor();
    }

    private void validateName() {
        if (null == name || 0 == name.length()) {
            throw new IllegalArgumentException("missing name");
        }
    }

    private void validateCode() {
        if (null == code || 0 == code.length()) {
            throw new IllegalArgumentException("missing code");
        }
        if (1 < code.length()) {
            throw new IllegalArgumentException("code must be a single character");
        }
    }

    private void validateLocation() {
        if (null == location || 0 == location.length()) {
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

    private void validateDisplayColor() {
        if (null == displayColor) {
            throw new IllegalArgumentException("missing displayColor");
        }

        if (displayColor < 0 || displayColor > 0xffffff) {
            throw new IllegalArgumentException("displayColor out of 24-bit range");
        }
    }
}
