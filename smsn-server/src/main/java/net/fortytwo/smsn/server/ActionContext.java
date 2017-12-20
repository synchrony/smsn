package net.fortytwo.smsn.server;

import net.fortytwo.smsn.brain.Brain;
import net.fortytwo.smsn.brain.io.json.JsonParser;
import net.fortytwo.smsn.brain.io.json.JsonPrinter;
import net.fortytwo.smsn.brain.io.wiki.WikiParser;
import net.fortytwo.smsn.brain.model.pg.GraphWrapper;
import net.fortytwo.smsn.brain.query.Model;

import java.util.Map;

public class ActionContext {

    private Map<String, Object> map;
    private GraphWrapper graphWrapper;
    private Brain brain;
    private Model model;
    private WikiParser wikiParser;
    private JsonParser jsonParser;
    private JsonPrinter jsonPrinter;

    public GraphWrapper getGraphWrapper() {
        return graphWrapper;
    }

    public void setGraphWrapper(GraphWrapper graphWrapper) {
        this.graphWrapper = graphWrapper;
    }

    public Brain getBrain() {
        return brain;
    }

    public void setBrain(Brain brain) {
        this.brain = brain;
    }

    public Map<String, Object> getMap() {
        return map;
    }

    public void setMap(Map<String, Object> map) {
        this.map = map;
    }

    public Model getModel() {
        return model;
    }

    public void setModel(Model model) {
        this.model = model;
    }

    public JsonPrinter getJsonPrinter() {
        return jsonPrinter;
    }

    public void setJsonPrinter(JsonPrinter jsonPrinter) {
        this.jsonPrinter = jsonPrinter;
    }

    public JsonParser getJsonParser() {
        return jsonParser;
    }

    public void setJsonParser(JsonParser jsonParser) {
        this.jsonParser = jsonParser;
    }

    public WikiParser getWikiParser() {
        return wikiParser;
    }

    public void setWikiParser(WikiParser wikiParser) {
        this.wikiParser = wikiParser;
    }
}
