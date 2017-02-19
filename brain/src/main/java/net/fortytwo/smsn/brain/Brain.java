package net.fortytwo.smsn.brain;

import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.model.TopicGraph;
import net.fortytwo.smsn.brain.rdf.KnowledgeBase;
import net.fortytwo.smsn.util.TypedProperties;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.logging.Logger;

public class Brain {
    private static final Logger logger = SemanticSynchrony.getLogger(Brain.class);

    /**
     * A configuration property indicating a special atom to which notes may be prepended
     * in a stream-of-consciousness style
     */
    public static final String PROP_BRAINSTREAM = "net.fortytwo.smsn.brain.brainStream";

    // TODO: make this configurable
    private static final int EVENT_STACK_CAPACITY = 50;

    // TODO: make these configurable
    private static final long
            INFERENCE_PERIOD = 1000L * 60,
            INFERENCE_INITIAL_WAIT = 1000L * 30;

    private static final boolean RUN_BACKGROUND_TASKS = false;

    private final TopicGraph topicGraph;

    private final KnowledgeBase knowledgeBase;

    private final ActivityLog activityLog;

    private final Priorities priorities;

    private final EventStack eventStack;

    public Brain(final TopicGraph topicGraph) throws BrainException {
        this.topicGraph = topicGraph;

        knowledgeBase = new KnowledgeBase(topicGraph);

        try {
            knowledgeBase.addDefaultClasses();
        } catch (InstantiationException | IllegalAccessException e) {
            throw new BrainException(e);
        }

        File logFile;
        try {
            logFile = SemanticSynchrony.getConfiguration().getFile(SemanticSynchrony.ACTIVITY_LOG, null);
        } catch (TypedProperties.PropertyException e) {
            throw new BrainException(e);
        }

        if (null == logFile) {
            logger.warning("no activity log specified");
            activityLog = null;
        } else {
            logger.info("will use activity log at " + logFile.getPath());
            try {
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

        knowledgeBase.inferAutomatically(INFERENCE_INITIAL_WAIT, INFERENCE_PERIOD);
    }

    public TopicGraph getTopicGraph() {
        return topicGraph;
    }

    public KnowledgeBase getKnowledgeBase() {
        return knowledgeBase;
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
}
