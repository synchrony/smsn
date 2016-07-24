package net.fortytwo.smsn.server.io;

import net.fortytwo.smsn.brain.Brain;
import net.fortytwo.smsn.brain.Filter;

import java.io.IOException;
import java.io.OutputStream;
import java.util.List;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public abstract class BrainWriter {
    protected static final Logger logger = Logger.getLogger(BrainWriter.class.getName());

    private Filter filter;
    private String rootId;

    public abstract List<Format> getFormats();

    protected abstract void exportInternal(Brain sourceBrain, OutputStream destStream, Format format)
            throws IOException;

    public void doExport(Brain sourceBrain, OutputStream destStream, Format format) throws IOException {
        exportInternal(sourceBrain, destStream, format);
    }

    protected String requireRootId() {
        if (null == rootId) throw new IllegalStateException("no root id");
        return rootId;
    }

    public void setRootId(String rootId) {
        this.rootId = rootId;
    }

    protected Filter requireFilter() {
        if (null == filter) throw new IllegalArgumentException("no filter");
        return filter;
    }

    public void setFilter(Filter filter) {
        this.filter = filter;
    }
}