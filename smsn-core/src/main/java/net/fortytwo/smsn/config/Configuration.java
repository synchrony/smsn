package net.fortytwo.smsn.config;

import java.io.Serializable;
import java.util.LinkedList;
import java.util.List;

public class Configuration implements Serializable {

    private String version = "unknown";
    private String activityLog = "smsn-activity.tsv";
    private Integer transactionBufferSize;
    private String thingNamespace = "http://example.org/things/";
    private String brainstream;

    private boolean verbose = false;

    private Services services = new Services();
    private List<DataSource> sources = new LinkedList<>();

    public Configuration() {
    }

    public Services getServices() {
        return services;
    }

    public void setServices(Services services) {
        this.services = services;
    }

    public List<DataSource> getSources() {
        return sources;
    }

    public void setSources(List<DataSource> sources) {
        this.sources = sources;
    }

    public String getActivityLog() {
        return activityLog;
    }

    public void setActivityLog(String activityLog) {
        this.activityLog = activityLog;
    }

    public Integer getTransactionBufferSize() {
        return transactionBufferSize;
    }

    public void setTransactionBufferSize(Integer transactionBufferSize) {
        this.transactionBufferSize = transactionBufferSize;
    }

    public String getVersion() {
        return version;
    }

    public void setVersion(String version) {
        this.version = version;
    }

    public String getThingNamespace() {
        return thingNamespace;
    }

    public void setThingNamespace(String thingNamespace) {
        this.thingNamespace = thingNamespace;
    }

    public String getBrainstream() {
        return brainstream;
    }

    public void setBrainstream(String brainstream) {
        this.brainstream = brainstream;
    }

    public boolean isVerbose() {
        return verbose;
    }

    public void setVerbose(boolean verbose) {
        this.verbose = verbose;
    }
}
