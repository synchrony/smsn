package net.fortytwo.smsn.server.actions;

import net.fortytwo.smsn.brain.io.BrainReader;
import net.fortytwo.smsn.brain.io.Format;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.actions.requests.ReadGraphRequest;
import net.fortytwo.smsn.server.errors.BadRequestException;
import net.fortytwo.smsn.server.errors.RequestProcessingException;
import org.apache.tinkerpop.gremlin.structure.Graph;

import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * A service for importing an Extend-o-Brain subgraph
 */
public class ReadGraph extends Action<ReadGraphRequest> {
    private static final Map<Graph, Set<String>> importsInProgress = new HashMap<>();
    private static final Map<Graph, Set<String>> importsSucceeded = new HashMap<>();

    @Override
    public String getName() {
        return "import";
    }

    @Override
    public void parseRequest(final ReadGraphRequest request, final RequestParams p) throws IOException {
        p.setFile(request.getFile());
        p.setFormat(request.getFormat());
    }

    @Override
    protected void performTransaction(final RequestParams p) throws RequestProcessingException, BadRequestException {
        if (null == p.getFormat()) {
            throw new BadRequestException("format is required");
        }

        Format format = Format.getFormat(p.getFormat());
        BrainReader reader = Format.getReader(format);

        beginImport(p.getGraphWrapper().getGraph(), p.getFile());

        boolean success = false;
        try {
            reader.doImport(new File(p.getFile()), format, p.getBrain(), true);
            success = true;
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        } finally {
            finishImport(p.getGraphWrapper().getGraph(), p.getFile(), success);
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
