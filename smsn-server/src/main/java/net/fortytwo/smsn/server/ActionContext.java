package net.fortytwo.smsn.server;

import net.fortytwo.smsn.brain.ActivityLog;
import net.fortytwo.smsn.brain.Brain;
import net.fortytwo.smsn.brain.io.json.TreeNodeJsonParser;
import net.fortytwo.smsn.brain.io.json.TreeNodeJsonPrinter;
import net.fortytwo.smsn.brain.io.wiki.TreeNodeWikiParser;
import net.fortytwo.smsn.brain.model.pg.GraphWrapper;
import net.fortytwo.smsn.brain.repository.AtomRepository;
import net.fortytwo.smsn.brain.repository.AtomRepositoryInterface;

import java.util.Map;

public class ActionContext {

    private Map<String, Object> map;
    private GraphWrapper graphWrapper;
    private Brain brain;
    private ActivityLog activityLog;
    private TreeNodeWikiParser treeNodeWikiParser;
    private TreeNodeJsonParser treeNodeJsonParser;
    private TreeNodeJsonPrinter treeNodeJsonPrinter;
    private AtomRepositoryInterface repository;

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

    public ActivityLog getActivityLog() {
        // Prefer direct activity log if set, otherwise get from brain
        if (activityLog != null) {
            return activityLog;
        }
        return brain != null ? brain.getActivityLog() : null;
    }

    public void setActivityLog(ActivityLog activityLog) {
        this.activityLog = activityLog;
    }

    public Map<String, Object> getMap() {
        return map;
    }

    public void setMap(Map<String, Object> map) {
        this.map = map;
    }

    public TreeNodeWikiParser getTreeNodeWikiParser() {
        return treeNodeWikiParser;
    }

    public void setTreeNodeWikiParser(TreeNodeWikiParser treeNodeWikiParser) {
        this.treeNodeWikiParser = treeNodeWikiParser;
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
    public AtomRepositoryInterface getRepository() {
        if (repository == null && graphWrapper != null) {
            repository = new AtomRepository(graphWrapper);
        }
        return repository;
    }

    /**
     * Set the repository directly (for file-based or other implementations).
     */
    public void setRepository(AtomRepositoryInterface repository) {
        this.repository = repository;
    }
}
