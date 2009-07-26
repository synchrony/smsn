package net.fortytwo.myotherbrain.flashmob;

/**
 * Author: josh
 * Date: Jul 18, 2009
 * Time: 7:12:19 PM
 */
public class SessionInfo {
    private String userName;
    private String versionInfo;
    private String baseURI;
    private String sensitivityUpperBound;
    private float emphasisLowerBound;

    public String getUserName() {
        return userName;
    }

    public void setUserName(final String userName) {
        this.userName = userName;
    }

    public String getSensitivityUpperBound() {
        return sensitivityUpperBound;
    }

    public void setSensitivityUpperBound(final String sensitivityUpperBound) {
        this.sensitivityUpperBound = sensitivityUpperBound;
    }

    public String getVersionInfo() {
        return versionInfo;
    }

    public void setVersionInfo(final String versionInfo) {
        this.versionInfo = versionInfo;
    }

    public String getBaseURI() {
        return baseURI;
    }

    public void setBaseURI(final String baseURI) {
        this.baseURI = baseURI;
    }

    public float getEmphasisLowerBound() {
        return emphasisLowerBound;
    }

    public void setEmphasisLowerBound(final float emphasisLowerBound) {
        this.emphasisLowerBound = emphasisLowerBound;
    }
}
