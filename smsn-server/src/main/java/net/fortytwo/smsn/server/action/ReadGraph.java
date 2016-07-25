package net.fortytwo.smsn.server.action;

import com.tinkerpop.blueprints.Graph;
import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.server.Action;
import net.fortytwo.smsn.server.Request;
import net.fortytwo.smsn.server.error.BadRequestException;
import net.fortytwo.smsn.server.error.RequestProcessingException;
import net.fortytwo.smsn.server.io.BrainReader;
import net.fortytwo.smsn.server.io.Format;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.File;
import java.io.IOException;
import java.security.Principal;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

/**
 * A service for importing an Extend-o-Brain subgraph
 *
 * @author Joshua Shinavier (http://fortytwo.net)
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
        ImportRequest r = new ImportRequest(request, p.user);

        p.file = r.file;
        p.format = r.format;
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
        if (null == p.format) {
            throw new BadRequestException("format is required");
        }

        Format format = Format.getFormat(p.format);
        BrainReader reader = Format.getReader(format);

        beginImport(p.baseGraph, p.file);

        boolean success = false;
        try {
            reader.doImport(new File(p.file), format, p.brain, true);
            success = true;
        } catch (IOException e) {
            throw new RequestProcessingException(e);
        } finally {
            finishImport(p.baseGraph, p.file, success);
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

    private class ImportRequest extends Request {
        private final String format;
        private final String file;

        public ImportRequest(final JSONObject json,
                             final Principal user) throws JSONException {
            super(json, user);

            format = this.json.getString(Params.FORMAT);
            file = this.json.getString(Params.FILE);
        }
    }
}
