package net.fortytwo.extendo.server;

import com.tinkerpop.blueprints.Graph;
import com.tinkerpop.blueprints.KeyIndexableGraph;
import com.tinkerpop.rexster.RexsterResourceContext;
import com.tinkerpop.rexster.extension.ExtensionDefinition;
import com.tinkerpop.rexster.extension.ExtensionDescriptor;
import com.tinkerpop.rexster.extension.ExtensionNaming;
import com.tinkerpop.rexster.extension.ExtensionPoint;
import com.tinkerpop.rexster.extension.ExtensionRequestParameter;
import com.tinkerpop.rexster.extension.ExtensionResponse;
import com.tinkerpop.rexster.extension.HttpMethod;
import com.tinkerpop.rexster.extension.RexsterContext;
import net.fortytwo.extendo.Extendo;
import net.fortytwo.extendo.brain.Note;
import net.fortytwo.extendo.brain.NoteQueries;
import net.fortytwo.extendo.brain.Params;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.security.Principal;

/**
 * A service for updating an Extend-o-Brain graph
 *
 * @author Joshua Shinavier (http://fortytwo.net)
 */
@ExtensionNaming(namespace = "extendo", name = "update")
//@ExtensionDescriptor(description = "update an Extend-o-Brain graph")
public class UpdateExtension extends ExtendoExtension {

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH, method = HttpMethod.POST)
    @ExtensionDescriptor(description = "update an Extend-o-Brain graph using the Extendo Wiki format")
    public ExtensionResponse handleRequest(@RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph,
                                           @ExtensionRequestParameter(name = Params.REQUEST,
                                                   description = "request description (JSON object)") String request) {

        RequestParams p = createParams(context, (KeyIndexableGraph) graph);
        UpdateRequest r;
        try {
            r = new UpdateRequest(new JSONObject(request), p.user);
        } catch (JSONException e) {
            return ExtensionResponse.error(e.getMessage());
        }
        p.depth = r.getDepth();
        p.rootId = r.getRootId();
        p.styleName = r.getStyleName();
        p.wikiView = r.wikiView;
        p.filter = r.getFilter();

        Extendo.logInfo("extendo update " + r.getRootId());

        return handleRequestInternal(p);
    }

    protected ExtensionResponse performTransaction(final RequestParams p) throws Exception {
        Note rootNote;

        InputStream in = new ByteArrayInputStream(p.wikiView.getBytes());
        try {
            rootNote = p.parser.fromWikiText(in);
        } finally {
            in.close();
        }

        // Apply the update
        try {
            p.queries.update(p.root, rootNote, p.depth, p.filter, p.style);
        } catch (NoteQueries.InvalidUpdateException e) {
            return ExtensionResponse.error("invalid update: " + e.getMessage());
        }

        Note n = p.queries.view(p.root, p.depth, p.filter, p.style);
        addView(n, p);

        return ExtensionResponse.ok(p.map);
    }

    protected boolean doesRead() {
        return true;
    }

    protected boolean doesWrite() {
        return true;
    }

    private class UpdateRequest extends RootedViewRequest {
        public final String wikiView;

        public UpdateRequest(final JSONObject json,
                             final Principal user) throws JSONException {
            super(json, user);

            wikiView = this.json.getString(Params.VIEW);
        }
    }
}
