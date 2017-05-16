package net.fortytwo.smsn.config;

import java.io.File;
import java.io.Serializable;

public class DataSource implements Serializable {

    private String name;
    private String code;
    private String location;
    private Integer color;

    public String getName() {
        return name;
    }

    public void setName(final String name) {
        if (null == name || 0 == name.length()) {
            throw new IllegalArgumentException("missing name");
        }
        this.name = name.trim();
    }

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        if (null == code || 0 == code.length()) {
            throw new IllegalArgumentException("missing code");
        }
        if (1 < code.length()) {
            throw new IllegalArgumentException("code must be a single character");
        }

        this.code = code.trim();
    }

    public String getLocation() {
        return location;
    }

    public void setLocation(String location) {
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

        this.location = location.trim();
    }

    public Integer getColor() {
        return color;
    }

    public void setColor(Integer color) {
        this.color = color;

        if (null == color) {
            throw new IllegalArgumentException("missing displayColor");
        }

        if (color < 0 || color > 0xffffff) {
            throw new IllegalArgumentException("displayColor out of 24-bit range");
        }
    }
}
