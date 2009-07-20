package net.fortytwo.myotherbrain.flashmob.model;

/**
 * Author: josh
 * Date: Jul 18, 2009
 * Time: 7:12:19 PM
 */
public class SessionInfo {
    private String userName;
    private String visibilityLevel;
    private String versionInfo;
    private String baseURI;
    private float emphasisThreshold;

    public String getUserName() {
        return userName;
    }

    public void setUserName(String userName) {
        this.userName = userName;
    }

    public String getVisibilityLevel() {
        return visibilityLevel;
    }

    public void setVisibilityLevel(String visibilityLevel) {
        this.visibilityLevel = visibilityLevel;
    }

    public String getVersionInfo() {
        return versionInfo;
    }

    public void setVersionInfo(String versionInfo) {
        this.versionInfo = versionInfo;
    }

    public String getBaseURI() {
        return baseURI;
    }

    public void setBaseURI(String baseURI) {
        this.baseURI = baseURI;
    }

    public float getEmphasisThreshold() {
        return emphasisThreshold;
    }

    public void setEmphasisThreshold(float emphasisThreshold) {
        this.emphasisThreshold = emphasisThreshold;
    }
}
