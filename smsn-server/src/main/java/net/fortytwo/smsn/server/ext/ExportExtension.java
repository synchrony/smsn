package net.fortytwo.smsn.server.ext;

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
import net.fortytwo.smsn.server.SmSnExtension;
import net.fortytwo.smsn.server.io.BrainWriter;
import net.fortytwo.smsn.server.io.Format;
import net.fortytwo.smsn.server.requests.FilteredResultsRequest;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.FileOutputStream;
import java.io.OutputStream;
import java.security.Principal;
import java.util.logging.Level;

/**
 * A service for exporting an Extend-o-Brain graph to the file system
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
@ExtensionNaming(namespace = "smsn", name = "export")
public class ExportExtension extends SmSnExtension {

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH)
    @ExtensionDescriptor(description = "an extension for exporting an Extend-o-Brain graph to the file system")
    public ExtensionResponse handleRequest(@RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph,
                                           @ExtensionRequestParameter(name = Params.REQUEST,
                                                   description = "request description (JSON object)") String request) {

        // TODO: any security restrictions here?
        try {
            RequestParams p = createParams(context, (KeyIndexableGraph) graph);

            ExportRequest r;
            try {
                r = new ExportRequest(new JSONObject(request), p.user);
            } catch (JSONException e) {
                return ExtensionResponse.error(e.getMessage());
            }

            p.filter = r.getFilter();
            p.file = r.file;
            p.format = r.format;

            p.rootId = r.rootId;
            p.height = r.height;

            SemanticSynchrony.logInfo("SmSn export " + r.format + " to " + r.file);

            return handleRequestInternal(p);
        } catch (Exception e) {
            logger.log(Level.SEVERE, "export failed", e);
            throw e;
        }
    }

    protected ExtensionResponse performTransaction(final RequestParams p) throws Exception {
        if (null == p.format) {
            return ExtensionResponse.error("format is required");
        }

        Format format = Format.getFormat(p.format);
        BrainWriter writer = Format.getWriter(format);

        writer.setFilter(p.filter);
        writer.setRootId(p.rootId);

        try (OutputStream destStream = new FileOutputStream(p.file)) {
            writer.doExport(p.brain, destStream, format);
        }

        return ExtensionResponse.ok(p.map);
    }

    protected boolean doesRead() {
        // doesn't read, in that no data is returned by the service (data is only written to the file system)
        return false;
    }

    protected boolean doesWrite() {
        return false;
    }

    private class ExportRequest extends FilteredResultsRequest {
        private final String format;
        private final String file;

        private final String rootId;
        private final int height;

        public ExportRequest(final JSONObject json,
                             final Principal user) throws JSONException {
            super(json, user);

            format = this.json.getString(Params.FORMAT);
            file = this.json.getString(Params.FILE);

            rootId = this.json.optString(Params.ROOT);
            height = this.json.optInt(Params.HEIGHT, 0);
        }
    }
}
