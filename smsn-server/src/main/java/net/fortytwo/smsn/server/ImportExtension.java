package net.fortytwo.smsn.server;

import com.tinkerpop.blueprints.Graph;
import com.tinkerpop.blueprints.KeyIndexableGraph;
import com.tinkerpop.rexster.RexsterResourceContext;
import com.tinkerpop.rexster.extension.ExtensionDefinition;
import com.tinkerpop.rexster.extension.ExtensionDescriptor;
import com.tinkerpop.rexster.extension.ExtensionNaming;
import com.tinkerpop.rexster.extension.ExtensionPoint;
import com.tinkerpop.rexster.extension.ExtensionRequestParameter;
import com.tinkerpop.rexster.extension.ExtensionResponse;
import com.tinkerpop.rexster.extension.RexsterContext;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.Params;
import net.fortytwo.smsn.server.io.FreeplaneImporter;
import net.fortytwo.smsn.server.io.GraphMLImporter;
import net.fortytwo.smsn.server.io.Importer;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.FileInputStream;
import java.io.InputStream;
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
@ExtensionNaming(namespace = "smsn", name = "import")
public class ImportExtension extends SmSnExtension {
    private final Map<Graph, Set<String>> importsInProgress;
    private final Map<Graph, Set<String>> importsSucceeded;

    public ImportExtension() {
        importsInProgress = new HashMap<>();
        importsSucceeded = new HashMap<>();
    }

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH)
    @ExtensionDescriptor(description = "an extension for exporting an Extend-o-Brain subgraph")
    public ExtensionResponse handleRequest(@RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph,
                                           @ExtensionRequestParameter(name = Params.REQUEST,
                                                   description = "request description (JSON object)") String request) {
        // TODO: any security restrictions here?  See also the export extension.

        RequestParams p = createParams(context, (KeyIndexableGraph) graph);

        ImportRequest r;
        try {
            r = new ImportRequest(new JSONObject(request), p.user);
        } catch (JSONException e) {
            return ExtensionResponse.error(e.getMessage());
        }

        p.file = r.file;
        p.format = r.format;

        SemanticSynchrony.logInfo("SmSn import " + r.format + " from " + r.file);

        return handleRequestInternal(p);
    }

    private synchronized ExtensionResponse beginImport(final Graph g, final String file) {
        Set<String> files = importsSucceeded.get(g);
        if (null != files && files.contains(file)) {
            return ExtensionResponse.error("graph at " + file + " has already been imported successfully");
        }

        files = importsInProgress.get(g);
        if (null != files) {
            if (files.contains(file)) {
                return ExtensionResponse.error("graph at " + file + " is currently being imported");
            }
        } else {
            files = new HashSet<>();
        }
        files.add(file);

        return null;
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

    protected ExtensionResponse performTransaction(final RequestParams p) throws Exception {
        Params.Format format = Params.Format.valueOf(p.format);
        if (null == format) {
            return ExtensionResponse.error("no such format: " + p.format);
        }

        ExtensionResponse r = beginImport(p.baseGraph, p.file);
        if (null != r) {
            return r;
        }

        boolean success = false;
        try (InputStream in = new FileInputStream(p.file)) {
            Importer importer;

            switch (format) {
                case GraphML:
                    importer = new GraphMLImporter();
                    break;
                case Freeplane:
                    importer = new FreeplaneImporter();
                    break;
                default:
                    throw new IllegalStateException("no importer for format " + format);
            }

            importer.doImport(p.brain.getBrainGraph(), in);
            success = true;
        } finally {
            finishImport(p.baseGraph, p.file, success);
        }

        return ExtensionResponse.ok(p.map);
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
