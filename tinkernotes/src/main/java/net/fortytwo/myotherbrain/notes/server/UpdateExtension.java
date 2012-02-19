package net.fortytwo.myotherbrain.notes.server;

import com.tinkerpop.blueprints.pgm.Graph;
import com.tinkerpop.rexster.RexsterResourceContext;
import com.tinkerpop.rexster.extension.ExtensionDefinition;
import com.tinkerpop.rexster.extension.ExtensionDescriptor;
import com.tinkerpop.rexster.extension.ExtensionNaming;
import com.tinkerpop.rexster.extension.ExtensionPoint;
import com.tinkerpop.rexster.extension.ExtensionRequestParameter;
import com.tinkerpop.rexster.extension.ExtensionResponse;
import com.tinkerpop.rexster.extension.HttpMethod;
import com.tinkerpop.rexster.extension.RexsterContext;
import net.fortytwo.myotherbrain.notes.Filter;
import net.fortytwo.myotherbrain.notes.Note;
import net.fortytwo.myotherbrain.notes.NotesSemantics;

import javax.ws.rs.core.SecurityContext;
import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.security.Principal;
import java.util.List;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
@ExtensionNaming(namespace = "tinkernotes", name = "update")
public class UpdateExtension extends TinkerNotesExtension {

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH, method = HttpMethod.POST)
    @ExtensionDescriptor(description = "an extension for updating a portion of a MyOtherBrain graph using the MOB Notes format")
    public ExtensionResponse handleRequest(@RexsterContext SecurityContext security,
                                           @RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph,
                                           @ExtensionRequestParameter(name = "root", description = "root atom (vertex) of the view") String rootKey,
                                           @ExtensionRequestParameter(name = "depth", description = "depth of the view") Integer depth,
                                           @ExtensionRequestParameter(name = "minWeight", description = "minimum-weight criterion for atoms in the view") Float minWeight,
                                           @ExtensionRequestParameter(name = "maxWeight", description = "maximum-weight criterion for atoms in the view") Float maxWeight,
                                           @ExtensionRequestParameter(name = "defaultWeight", description = "weight of new atoms added to the view") Float defaultWeight,
                                           @ExtensionRequestParameter(name = "minSharability", description = "minimum-sharability criterion for atoms in the view") Float minSharability,
                                           @ExtensionRequestParameter(name = "maxSharability", description = "maximum-sharability criterion for atoms in the view") Float maxSharability,
                                           @ExtensionRequestParameter(name = "defaultSharability", description = "sharability of new atoms added to the view") Float defaultSharability,
                                           @ExtensionRequestParameter(name = "view", description = "the updated view") String view,
                                           @ExtensionRequestParameter(name = "style", description = "the style of view to generate") String styleName) {

        LOGGER.info("tinkernotes update " + rootKey + " (depth " + depth + ")");
        System.err.println("tinkernotes update " + rootKey + " (depth " + depth + ")");

        Principal user = null == security ? null : security.getUserPrincipal();

        if (!canWrite(user)) {
            return ExtensionResponse.error("user does not have permission to push updates");
        }

        if (null == user) {
            System.err.println("no security");
        }

        Filter filter;

        try {
            float m = findMinAuthorizedSharability(user, minSharability);
            filter = new Filter(m, maxSharability, defaultSharability, minWeight, maxWeight, defaultWeight);
        } catch (IllegalArgumentException e) {
            return ExtensionResponse.error(e.getMessage());
        }

        Params p = new Params();
        p.baseGraph = graph;
        p.depth = depth;
        p.filter = filter;
        p.view = view;
        p.rootKey = rootKey;
        p.styleName = styleName;
        return this.handleRequestInternal(p);
    }

    @Override
    protected ExtensionResponse performTransaction(final Params p) throws Exception {
        List<Note> children;

        InputStream in = new ByteArrayInputStream(p.view.getBytes());
        try {
            children = p.syntax.readNotes(in);
        } finally {
            in.close();
        }

        // Apply the update
        try {
            p.semantics.update(p.root, children, p.depth, p.filter, p.inverse);
        } catch (NotesSemantics.InvalidUpdateException e) {
            return ExtensionResponse.error("invalid update: " + e.getMessage());
        }

        addView(p);

        return ExtensionResponse.ok(p.map);
    }

    @Override
    protected boolean isReadOnly() {
        return false;
    }
}
