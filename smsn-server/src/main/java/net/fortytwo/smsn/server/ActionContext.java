package net.fortytwo.smsn.server;

import net.fortytwo.smsn.brain.Brain;
import net.fortytwo.smsn.brain.io.json.JsonParser;
import net.fortytwo.smsn.brain.io.json.JsonPrinter;
import net.fortytwo.smsn.brain.io.json.TreeNodeJsonParser;
import net.fortytwo.smsn.brain.io.json.TreeNodeJsonPrinter;
import net.fortytwo.smsn.brain.io.wiki.WikiParser;
import net.fortytwo.smsn.brain.model.pg.GraphWrapper;
import net.fortytwo.smsn.brain.repository.AtomRepository;

import java.util.Map;

public class ActionContext {

    private Map<String, Object> map;
    private GraphWrapper graphWrapper;
    private Brain brain;
    private WikiParser wikiParser;
    private JsonParser jsonParser;
    private JsonPrinter jsonPrinter;
    private TreeNodeJsonParser treeNodeJsonParser;
    private TreeNodeJsonPrinter treeNodeJsonPrinter;
    private AtomRepository repository;

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

    public TreeNodeJsonParser getTreeNodeJsonParser() {
        return treeNodeJsonParser;
    }

    public void setTreeNodeJsonParser(TreeNodeJsonParser treeNodeJsonParser) {
        this.treeNodeJsonParser = treeNodeJsonParser;
    }

    public TreeNodeJsonPrinter getTreeNodeJsonPrinter() {
        return treeNodeJsonPrinter;
    }

    public void setTreeNodeJsonPrinter(TreeNodeJsonPrinter treeNodeJsonPrinter) {
        this.treeNodeJsonPrinter = treeNodeJsonPrinter;
    }

    /**
     * Get the AtomRepository for direct Atom-based operations.
     * Lazily creates the repository from the GraphWrapper if needed.
     */
    public AtomRepository getRepository() {
        if (repository == null && graphWrapper != null) {
            repository = new AtomRepository(graphWrapper);
        }
        return repository;
    }
}
