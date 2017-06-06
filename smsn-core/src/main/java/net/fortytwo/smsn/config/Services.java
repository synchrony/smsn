package net.fortytwo.smsn.config;

public class Services {

    private String agentIri;

    private Service broadcast;
    private Service osc;
    private Service pubSub;
    private Service server;
    private Service music;

    public Service getBroadcast() {
        return broadcast;
    }

    public void setBroadcast(Service broadcast) {
        this.broadcast = broadcast;
    }

    public Service getOsc() {
        return osc;
    }

    public void setOsc(Service osc) {
        this.osc = osc;
    }

    public Service getPubSub() {
        return pubSub;
    }

    public void setPubSub(Service pubSub) {
        this.pubSub = pubSub;
    }

    public Service getServer() {
        return server;
    }

    public void setServer(Service server) {
        this.server = server;
    }

    public Service getMusic() {
        return music;
    }

    public void setMusic(Service music) {
        this.music = music;
    }

    public String getAgentIri() {
        return agentIri;
    }

    public void setAgentIri(String agentIri) {
        this.agentIri = agentIri;
    }
}
