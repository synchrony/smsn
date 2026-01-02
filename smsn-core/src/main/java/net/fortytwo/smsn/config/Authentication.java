package net.fortytwo.smsn.config;

import java.io.Serializable;
import java.util.LinkedList;
import java.util.List;

/**
 * Authentication configuration for SmSn server.
 *
 * When enabled, connections without a valid token are treated as anonymous
 * and can only read from sources listed in publicSources.
 * Write operations always require authentication.
 */
public class Authentication implements Serializable {

    private boolean enabled = false;
    private String token;
    private List<String> publicSources = new LinkedList<>();

    public boolean isEnabled() {
        return enabled;
    }

    public void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }

    public String getToken() {
        return token;
    }

    public void setToken(String token) {
        this.token = token;
    }

    public List<String> getPublicSources() {
        return publicSources;
    }

    public void setPublicSources(List<String> publicSources) {
        this.publicSources = publicSources;
    }

    /**
     * Check if a source is publicly readable.
     */
    public boolean isPublicSource(String sourceName) {
        return publicSources != null && publicSources.contains(sourceName);
    }
}
