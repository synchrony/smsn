package net.fortytwo.extendo.brain;

import net.fortytwo.extendo.Extendo;
import net.fortytwo.extendo.util.properties.PropertyException;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public class ExtendoBrain {
    private static final Logger LOGGER = Extendo.getLogger(ExtendoBrain.class);

    private final BrainGraph brainGraph;

    private final ActivityLog activityLog;

    private final Priorities priorities;

    public ExtendoBrain(final BrainGraph brainGraph) throws ExtendoBrainException {
        this.brainGraph = brainGraph;


        File logFile;
        try {
            logFile = Extendo.getConfiguration().getFile(Extendo.ACTIVITY_LOG, null);
        } catch (PropertyException e) {
            throw new ExtendoBrainException(e);
        }

        if (null == logFile) {
            LOGGER.warning("no activity log specified");
            activityLog = null;
        } else {
            LOGGER.info("will use activity log at " + logFile.getPath());
            try {
                activityLog = new ActivityLog(new FileWriter(logFile, true));
            } catch (IOException e) {
                throw new ExtendoBrainException(e);
            }
        }

        priorities = new Priorities();
        priorities.refreshQueue(brainGraph);
    }

    public BrainGraph getBrainGraph() {
        return brainGraph;
    }

    public ActivityLog getActivityLog() {
        return activityLog;
    }

    public Priorities getPriorities() {
        return priorities;
    }

    public class ExtendoBrainException extends Exception {
        public ExtendoBrainException(final Throwable cause) {
            super(cause);
        }
    }
}
