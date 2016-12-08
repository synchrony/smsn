package net.fortytwo.smsn.server.action;

import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.RequestParams;
import net.fortytwo.smsn.server.action.requests.ReadGraphRequest;
import net.fortytwo.smsn.server.error.BadRequestException;
import net.fortytwo.smsn.server.error.RequestProcessingException;
import net.fortytwo.smsn.brain.io.BrainReader;
import net.fortytwo.smsn.brain.io.Format;
import org.apache.tinkerpop.gremlin.structure.Graph;
import org.json.JSONException;
import org.json.JSONObject;

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
    private final Map<Graph, Set<String>> importsInProgress;
    private final Map<Graph, Set<String>> importsSucceeded;

    public ReadGraph() {
        importsInProgress = new HashMap<>();
        importsSucceeded = new HashMap<>();
    }

    @Override
    public String getName() {
        return "import";
    }

    @Override
    public void parseRequest(final JSONObject request, final RequestParams p) throws JSONException {
        ReadGraphRequest r = new ReadGraphRequest(request, p.getUser());

        p.setFile(r.getFile());
        p.setFormat(r.getFormat());
    }

    private synchronized void beginImport(final Graph g, final String file) throws BadRequestException {
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

    private synchronized void finishImport(final Graph g, final String file, final boolean success) {
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

    protected boolean doesRead() {
        // doesn't expose data which has been read from the graph,
        // although data is loaded into the graph from the file system
        return false;
    }

    protected boolean doesWrite() {
        // this operation does modify the graph
        return true;
    }

}
