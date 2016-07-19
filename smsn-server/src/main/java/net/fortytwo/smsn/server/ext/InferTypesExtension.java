package net.fortytwo.smsn.server.ext;

import com.tinkerpop.blueprints.Graph;
import com.tinkerpop.blueprints.KeyIndexableGraph;
import com.tinkerpop.rexster.RexsterResourceContext;
import com.tinkerpop.rexster.extension.ExtensionDefinition;
import com.tinkerpop.rexster.extension.ExtensionDescriptor;
import com.tinkerpop.rexster.extension.ExtensionNaming;
import com.tinkerpop.rexster.extension.ExtensionPoint;
import com.tinkerpop.rexster.extension.ExtensionResponse;
import com.tinkerpop.rexster.extension.RexsterContext;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.rdf.KnowledgeBase;
import net.fortytwo.smsn.server.SmSnExtension;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
@ExtensionNaming(namespace = "smsn", name = "infer-types")
public class InferTypesExtension extends SmSnExtension {

    @ExtensionDefinition(extensionPoint = ExtensionPoint.GRAPH)
    @ExtensionDescriptor(description = "an extension for performing type inference on an Extend-o-Brain knowledge base")
    public ExtensionResponse handleRequest(@RexsterContext RexsterResourceContext context,
                                           @RexsterContext Graph graph) {
        RequestParams p = createParams(context, (KeyIndexableGraph) graph);

        SemanticSynchrony.logInfo("SmSn infer-types");

        return handleRequestInternal(p);
    }

    protected ExtensionResponse performTransaction(final RequestParams p) throws Exception {
        // do *not* reset the knowledge base.  Build upon inference performed in previous iterations
        KnowledgeBase kb = p.brain.getKnowledgeBase();
        //kb.reset();

        long timeBefore = System.currentTimeMillis();

        // note: multiple (typically four) invocations are required before the knowledge base is ready for RDF export
        kb.inferClasses(null, null);

        long timeAfter = System.currentTimeMillis();
        logger.info("completed type inference in " + (timeAfter - timeBefore) + "ms");

        return ExtensionResponse.ok(p.map);
    }

    protected boolean doesRead() {
        // doesn't read, in that no data is returned by the service;
        // this operation only affects future calls which do read
        return false;
    }

    protected boolean doesWrite() {
        return false;
    }
}
