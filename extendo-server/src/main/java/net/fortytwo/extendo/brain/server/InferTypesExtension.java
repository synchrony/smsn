package net.fortytwo.extendo.brain.server;

import com.tinkerpop.blueprints.Graph;
import com.tinkerpop.blueprints.KeyIndexableGraph;
import com.tinkerpop.rexster.RexsterResourceContext;
import com.tinkerpop.rexster.extension.ExtensionDefinition;
import com.tinkerpop.rexster.extension.ExtensionDescriptor;
import com.tinkerpop.rexster.extension.ExtensionNaming;
import com.tinkerpop.rexster.extension.ExtensionPoint;
import com.tinkerpop.rexster.extension.ExtensionResponse;
import com.tinkerpop.rexster.extension.RexsterContext;
import net.fortytwo.extendo.brain.rdf.KnowledgeBase;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
@ExtensionNaming(namespace = "extendo", name = "infer-types")
public class InferTypesExtension extends ExtendoExtension {

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH)
    @ExtensionDescriptor(description = "an extension for performing type inference on an Extend-o-Brain knowledge base")
    public ExtensionResponse handleRequest(@RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph) {
        logInfo("extendo history");

        Params p = createParams(context, (KeyIndexableGraph) graph);

        return handleRequestInternal(p);
    }

    protected ExtensionResponse performTransaction(final Params p) throws Exception {
        KnowledgeBase kb = p.brain.getKnowledgeBase();
        kb.reset();
        long timeBefore = System.currentTimeMillis();

        kb.inferTypes();

        long timeAfter = System.currentTimeMillis();
        LOGGER.info("completed type inference in " + (timeAfter - timeBefore) + "ms");

        return ExtensionResponse.ok(p.map);
    }

    protected boolean doesRead() {
        // doesn't read, in that no data is returned by the service; this operation only affects future calls which do read
        return false;
    }

    protected boolean doesWrite() {
        return false;
    }
}
