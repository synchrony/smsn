package net.fortytwo.smsn.server.io;

import com.tinkerpop.blueprints.Graph;
import com.tinkerpop.blueprints.TransactionalGraph;
import com.tinkerpop.blueprints.Vertex;
import net.fortytwo.smsn.SemanticSynchrony;
import net.fortytwo.smsn.brain.BrainGraph;

import java.io.IOException;
import java.io.InputStream;
import java.util.logging.Logger;

/**
 * @author Joshua Shinavier (http://fortytwo.net)
 */
public abstract class Importer {
    protected static final Logger logger = Logger.getLogger(GraphMLImporter.class.getName());

    protected abstract void importInternal(BrainGraph destGraph, final InputStream sourceStream) throws IOException;

    private String defaultNodeName;

    public void doImport(final BrainGraph destGraph, final InputStream sourceStream) throws IOException {
        long before = System.currentTimeMillis();

        importInternal(destGraph, sourceStream);

        // note: we assume the graph is small
        commit(destGraph);

        reindexVertices(destGraph);

        // again, we assume the graph is small
        commit(destGraph);

        long after = System.currentTimeMillis();
        logger.info("imported subgraph in " + (after - before) + "ms");
    }

    protected String getDefaultNodeName() {
        return defaultNodeName;
    }

    public void setDefaultNodeName(final String defaultNodeName) {
        this.defaultNodeName = defaultNodeName;
    }

    private void reindexVertices(BrainGraph destGraph) {
        TransactionalGraph propertyGraph = (TransactionalGraph) destGraph.getPropertyGraph();
        for (Vertex v : propertyGraph.getVertices()) {
            String value = v.getProperty(SemanticSynchrony.VALUE);
            if (null != value) destGraph.indexForSearch(destGraph.getAtom(v), value);
        }
    }

    private void commit(final BrainGraph brainGraph) {
        Graph propertyGraph = brainGraph.getPropertyGraph();
        if (propertyGraph instanceof TransactionalGraph) {
            ((TransactionalGraph) propertyGraph).commit();
        }
    }
}
