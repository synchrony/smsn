package net.fortytwo.smsn.server.io;

import net.fortytwo.smsn.brain.BrainGraph;
import net.fortytwo.smsn.brain.ExtendoBrain;
import net.fortytwo.smsn.brain.Filter;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.List;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public abstract class Exporter {
    protected static final Logger logger = Logger.getLogger(Exporter.class.getName());

    private Filter filter;
    private String rootId;

    public abstract List<String> getFormats();

    protected abstract void exportInternal(ExtendoBrain sourceBrain, OutputStream destStream) throws IOException;

    public void doExport(ExtendoBrain sourceBrain, OutputStream destStream) throws IOException {
        exportInternal(sourceBrain, destStream);
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