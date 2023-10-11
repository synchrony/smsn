package net.fortytwo.smsn.brain;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.TopicGraph;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;

public class Brain {

    // TODO: make this configurable
    private static final int EVENT_STACK_CAPACITY = 50;

    private static final boolean RUN_BACKGROUND_TASKS = false;

    private final TopicGraph topicGraph;

    private final ActivityLog activityLog;

    private final Priorities priorities;

    private final EventStack eventStack;

    public Brain(final TopicGraph topicGraph) throws BrainException {
        this.topicGraph = topicGraph;

        String filePath = SemanticSynchrony.getConfiguration().getActivityLog();

        if (null == filePath) {
            SemanticSynchrony.getLogger().warning("no activity log specified");
            activityLog = null;
        } else {
            SemanticSynchrony.getLogger().fine("using activity log at " + filePath);
            try {
                File logFile = new File(filePath);
                createDirectories(logFile);
                activityLog = new ActivityLog(new FileWriter(logFile, true));
            } catch (IOException e) {
                throw new BrainException(e);
            }
        }

        priorities = new Priorities();

        eventStack = new EventStack(EVENT_STACK_CAPACITY);
    }

    public void startBackgroundTasks() {
        if (!RUN_BACKGROUND_TASKS) return;

        priorities.refreshQueue(topicGraph);
    }

    public TopicGraph getTopicGraph() {
        return topicGraph;
    }

    public ActivityLog getActivityLog() {
        return activityLog;
    }

    public Priorities getPriorities() {
        return priorities;
    }

    public EventStack getEventStack() {
        return eventStack;
    }

    public class BrainException extends Exception {
        public BrainException(final Throwable cause) {
            super(cause);
        }
    }

    private void createDirectories(final File file) {
        file.getParentFile().mkdirs();
    }
}
