package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.io.BrainReader;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;
import org.apache.tinkerpop.gremlin.structure.Graph;

import javax.validation.constraints.NotNull;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * A service for importing an Extend-o-Brain subgraph
 */
public class ReadGraph extends Action {
    private static final Map<Graph, Set<String>> importsInProgress = new HashMap<>();
    private static final Map<Graph, Set<String>> importsSucceeded = new HashMap<>();

    @NotNull
    private String format;
    @NotNull
    private String file;

    public String getFormat() {
        return format;
    }

    public String getFile() {
        return file;
    }

    public void setFormat(String format) {
        this.format = format;
    }

    public void setFile(String file) {
        this.file = file;
    }

    @Override
    public void parseRequest(final RequestParams params) throws IOException {
        params.setFile(getFile());
        params.setFormat(getFormat());
    }

    @Override
    protected void performTransaction(final RequestParams params) throws RequestProcessingException, BadRequestException {
        if (null == params.getFormat()) {
            throw new BadRequestException("format is required");
        }

        Format format = Format.getFormat(params.getFormat());
        BrainReader reader = Format.getReader(format);

        beginImport(params.getGraphWrapper().getGraph(), params.getFile());

        boolean success = false;
        try {
            reader.doImport(new File(params.getFile()), format, params.getBrain(), true);
            success = true;
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        } finally {
            finishImport(params.getGraphWrapper().getGraph(), params.getFile(), success);
        }
    }

    @Override
    protected boolean doesRead() {
        // doesn't expose data which has been read from the graph,
        // although data is loaded into the graph from the file system
        return false;
    }

    @Override
    protected boolean doesWrite() {
        // this operation does modify the graph
        return true;
    }

    private static synchronized void beginImport(final Graph g, final String file) throws BadRequestException {
        Set<String> files = importsSucceeded.get(g);
        if (null != files && files.contains(file)) {
            throw new BadRequestException("graph at " + file + " has already been imported successfully");
        }

        files = importsInProgress.get(g);
        if (null != files) {
            if (files.contains(file)) {
                throw new BadRequestException("graph at " + file + " is currently being imported");
            }
        } else {
            files = new HashSet<>();
        }
        files.add(file);
    }

    private static synchronized void finishImport(final Graph g, final String file, final boolean success) {
        Set<String> files = importsInProgress.get(g);
        if (null != files) {
            files.remove(file);
        }

        if (success) {
            files = importsSucceeded.get(g);
            if (null == files) {
                files = new HashSet<>();
                importsSucceeded.put(g, files);
            }
            files.add(file);
        }
    }
}
