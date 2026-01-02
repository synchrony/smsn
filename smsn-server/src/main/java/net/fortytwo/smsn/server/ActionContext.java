package net.fortytwo.smsn.server;

import net.fortytwo.smsn.brain.ActivityLog;
import net.fortytwo.smsn.brain.io.json.TreeNodeJsonParser;
import net.fortytwo.smsn.brain.io.json.TreeNodeJsonPrinter;
import net.fortytwo.smsn.brain.io.wiki.TreeNodeWikiParser;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.repository.AtomRepositoryInterface;

import java.util.Map;

public class ActionContext {

    private Map<String, Object> map;
    private ActivityLog activityLog;
    private TreeNodeWikiParser treeNodeWikiParser;
    private TreeNodeJsonParser treeNodeJsonParser;
    private TreeNodeJsonPrinter treeNodeJsonPrinter;
    private AtomRepositoryInterface repository;
    private TopicGraph topicGraph;

    public ActivityLog getActivityLog() {
        return activityLog;
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

    public AtomRepositoryInterface getRepository() {
        return repository;
    }

    public void setRepository(AtomRepositoryInterface repository) {
        this.repository = repository;
    }

    public TopicGraph getTopicGraph() {
        return topicGraph;
    }

    public void setTopicGraph(TopicGraph topicGraph) {
        this.topicGraph = topicGraph;
    }
}
